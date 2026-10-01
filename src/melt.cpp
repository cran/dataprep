// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]
//
// ============================================================================
// melt.cpp -- CRAN-compliant, SIMD-dispatched dataframe melt for R.
// Optimized for large mixed-id datasets; matches/exceeds polars performance.
//
// Key optimizations (melt_cpp and its SPMD path; tiny/small fast paths
// short-circuit most of these to minimize fixed overhead):
//   * Blocked id-column doubling: id columns read ONCE from input, replicated
//     via L2-cached block doubling. Cuts id-column read bandwidth by 8/9.
//   * Normal-store first block + NT-store remaining blocks: maximizes cache
//     utilization for replication while avoiding cache pollution.
//   * Added normal-store SIMD int copy (copy_i32) for cached paths.
//   * L2-sized blocking (16Ki rows) for id column replication.
//
// CRAN compliance: no PGO, no global -march=native, all intrinsics guarded,
// runtime feature detection, scalar fallbacks, no custom allocators.
// ============================================================================

#include <Rcpp.h>
#include <cstring>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <climits>
#include <string>
#include <cctype>
#include <utility>

#ifdef __linux__
#  include <unistd.h>
#  include <sys/mman.h>
#endif

#if defined(__GLIBC__)
#  include <malloc.h>
#endif

#ifdef _OPENMP
#  include <omp.h>
#endif

#if defined(__x86_64__) || defined(__i386__) || defined(_M_X64) || defined(_M_IX86)
#  define DATAPREP_ARCH_X86 1
#  include <immintrin.h>
#else
#  define DATAPREP_ARCH_X86 0
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define DATAPREP_HAS_GCC_ATTR 1
#  define DATAPREP_COLD          __attribute__((cold))
#  define DATAPREP_HOT           __attribute__((hot))
#  define DATAPREP_LIKELY(x)     __builtin_expect(!!(x), 1)
#  define DATAPREP_UNLIKELY(x)   __builtin_expect(!!(x), 0)
#  define DATAPREP_TARGET(x)     __attribute__((target(x)))
#else
#  define DATAPREP_HAS_GCC_ATTR 0
#  define DATAPREP_COLD
#  define DATAPREP_HOT
#  define DATAPREP_LIKELY(x)     (x)
#  define DATAPREP_UNLIKELY(x)   (x)
#  define DATAPREP_TARGET(x)
#endif

using namespace Rcpp;

namespace {

// ============================================================================
// Parallel THP trigger.
// ============================================================================
static inline void touch_2m_in_range(const void* col_base, size_t col_bytes,
                                     R_xlen_t elem_off, R_xlen_t elem_len,
                                     size_t elem_size) {
  if (col_bytes < (2ULL << 20)) return;
  if (elem_len <= 0 || elem_size == 0) return;
  const size_t STEP = 2ULL << 20;
  const size_t byte_start = (size_t)elem_off * elem_size;
  size_t byte_end = byte_start + (size_t)elem_len * elem_size;
  if (byte_end > col_bytes) byte_end = col_bytes;
  if (byte_start >= byte_end) return;
  size_t off = byte_start & ~(STEP - 1);
  volatile char* c = (volatile char*)col_base;
  for (; off < byte_end; off += STEP) {
    c[off] = 0;
  }
}

#if defined(__linux__)
static inline void hint_pages(void* ptr, size_t bytes) {
  if (bytes < (2ULL << 20)) return;
  long ps = sysconf(_SC_PAGESIZE);
  if (ps <= 0) ps = 4096;
  uintptr_t a = (uintptr_t)ptr;
  uintptr_t m = (uintptr_t)ps - 1;
  uintptr_t s = (a + m) & ~m;
  uintptr_t e = (a + bytes) & ~m;
  if (e <= s) return;
#  ifdef MADV_HUGEPAGE
  madvise((void*)s, (size_t)(e - s), MADV_HUGEPAGE);
#  endif
}
#else
static inline void hint_pages(void*, size_t) {}
#endif

// ============================================================================
// Allocation with huge-page hint.
// ============================================================================
static inline SEXP alloc_hint(SEXPTYPE t, R_xlen_t n) {
  SEXP s = Rf_allocVector(t, n);
  if (n <= 0) return s;
  size_t es = 0;
  void* p = nullptr;
  switch (t) {
  case INTSXP:  es = sizeof(int);    p = (void*)INTEGER(s);       break;
  case LGLSXP:  es = sizeof(int);    p = (void*)LOGICAL(s);       break;
  case REALSXP: es = sizeof(double); p = (void*)REAL(s);          break;
  case STRSXP:  es = sizeof(SEXP);   p = (void*)STRING_PTR_RO(s); break;
  default: return s;
  }
  size_t bytes = (size_t)n * es;
  if (p) hint_pages(p, bytes);
  return s;
}

// ============================================================================
// sfence (per-thread).
// ============================================================================
static inline void nt_flush() {
#if DATAPREP_ARCH_X86
  _mm_sfence();
#endif
}

// ============================================================================
// ISA primitive 1: nt_fill_i32 (non-temporal int32 fill)
// ============================================================================
static void nt_fill_i32_scalar(int* p, int v, R_xlen_t n) {
  for (R_xlen_t i = 0; i < n; ++i) p[i] = v;
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void nt_fill_i32_sse42(int* p, int v, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(p + r) & 15u) != 0) { p[r] = v; ++r; }
    const __m128i vv = _mm_set1_epi32(v);
    const R_xlen_t end = r + ((n - r) / 4) * 4;
    for (; r < end; r += 4) _mm_stream_si128((__m128i*)(p + r), vv);
    for (; r < n; ++r) p[r] = v;
  }

DATAPREP_TARGET("avx2")
  static void nt_fill_i32_avx2(int* p, int v, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(p + r) & 31u) != 0) { p[r] = v; ++r; }
    const __m256i vv = _mm256_set1_epi32(v);
    const R_xlen_t end = r + ((n - r) / 8) * 8;
    for (; r < end; r += 8) _mm256_stream_si256((__m256i*)(p + r), vv);
    for (; r < n; ++r) p[r] = v;
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void nt_fill_i32_avx512(int* p, int v, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(p + r) & 63u) != 0) { p[r] = v; ++r; }
    const __m512i vv = _mm512_set1_epi32(v);
    const R_xlen_t end = r + ((n - r) / 16) * 16;
    for (; r < end; r += 16) _mm512_stream_si512((__m512i*)(p + r), vv);
    for (; r < n; ++r) p[r] = v;
  }
#endif

static void (*nt_fill_i32_ptr)(int*, int, R_xlen_t) = nt_fill_i32_scalar;
static inline void nt_fill_i32(int* p, int v, R_xlen_t n) {
  nt_fill_i32_ptr(p, v, n);
}

// ============================================================================
// ISA primitive 2: nt_copy_pd (non-temporal 8-byte copy)
// ============================================================================
static void nt_copy_pd_scalar(double* dst, const double* src, R_xlen_t n) {
  std::memcpy(dst, src, (size_t)n * sizeof(double));
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void nt_copy_pd_sse42(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 15u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 2) * 2;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 2) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm_stream_pd(dst + r, _mm_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 2) _mm_stream_pd(dst + r, _mm_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx2")
  static void nt_copy_pd_avx2(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 31u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 4) * 4;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 4) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm256_stream_pd(dst + r, _mm256_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 4)
      _mm256_stream_pd(dst + r, _mm256_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void nt_copy_pd_avx512(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 63u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 8) * 8;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 8) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm512_stream_pd(dst + r, _mm512_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 8)
      _mm512_stream_pd(dst + r, _mm512_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }
#endif

static void (*nt_copy_pd_ptr)(double*, const double*, R_xlen_t) = nt_copy_pd_scalar;
static inline void nt_copy_pd(double* dst, const double* src, R_xlen_t n) {
  nt_copy_pd_ptr(dst, src, n);
}

// ============================================================================
// ISA primitive 3: copy_pd (normal-store 8-byte copy, cached)
// ============================================================================
static void copy_pd_scalar(double* dst, const double* src, R_xlen_t n) {
  std::memcpy(dst, src, (size_t)n * sizeof(double));
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void copy_pd_sse42(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 15u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 2) * 2;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 2) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm_store_pd(dst + r, _mm_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 2) _mm_store_pd(dst + r, _mm_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx2")
  static void copy_pd_avx2(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 31u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 4) * 4;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 4) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm256_store_pd(dst + r, _mm256_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 4)
      _mm256_store_pd(dst + r, _mm256_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void copy_pd_avx512(double* dst, const double* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 63u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 8) * 8;
    constexpr R_xlen_t PF = 64;
    if (n >= 4096) {
      for (; r + PF < end; r += 8) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm512_store_pd(dst + r, _mm512_loadu_pd(src + r));
      }
    }
    for (; r < end; r += 8)
      _mm512_store_pd(dst + r, _mm512_loadu_pd(src + r));
    for (; r < n; ++r) dst[r] = src[r];
  }
#endif

static void (*copy_pd_ptr)(double*, const double*, R_xlen_t) = copy_pd_scalar;

static inline void copy_pd(double* dst, const double* src, R_xlen_t n) {
  copy_pd_ptr(dst, src, n);
}

// Fast bulk copy of a SEXP vector via memcpy/SIMD. Safe ONLY when
// `dst` was produced by Rf_allocVector in this call, so every element
// is NEW from the GC's point of view and no write barrier is required.
// Do not use for in-place modification of a live vector.
static inline void copy_sexp(SEXP* dst, const SEXP* src, R_xlen_t n) {
  copy_pd_ptr((double*)dst, (const double*)src, n);
}

// ============================================================================
// ISA primitive 4: nt_copy_i32 (non-temporal int32 copy)
// ============================================================================
static void nt_copy_i32_scalar(int* dst, const int* src, R_xlen_t n) {
  std::memcpy(dst, src, (size_t)n * sizeof(int));
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void nt_copy_i32_sse42(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 15u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 4) * 4;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 4) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm_stream_si128((__m128i*)(dst + r),
                         _mm_loadu_si128((const __m128i*)(src + r)));
      }
    }
    for (; r < end; r += 4)
      _mm_stream_si128((__m128i*)(dst + r),
                       _mm_loadu_si128((const __m128i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx2")
  static void nt_copy_i32_avx2(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 31u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 8) * 8;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 8) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm256_stream_si256((__m256i*)(dst + r),
                            _mm256_loadu_si256((const __m256i*)(src + r)));
      }
    }
    for (; r < end; r += 8)
      _mm256_stream_si256((__m256i*)(dst + r),
                          _mm256_loadu_si256((const __m256i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void nt_copy_i32_avx512(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 63u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 16) * 16;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 16) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm512_stream_si512((__m512i*)(dst + r),
                            _mm512_loadu_si512((const __m512i*)(src + r)));
      }
    }
    for (; r < end; r += 16)
      _mm512_stream_si512((__m512i*)(dst + r),
                          _mm512_loadu_si512((const __m512i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }
#endif

static void (*nt_copy_i32_ptr)(int*, const int*, R_xlen_t) = nt_copy_i32_scalar;
static inline void nt_copy_i32(int* dst, const int* src, R_xlen_t n) {
  nt_copy_i32_ptr(dst, src, n);
}

// ============================================================================
// ISA primitive 5: copy_i32 (normal-store int32 copy, cached)
// ============================================================================
static void copy_i32_scalar(int* dst, const int* src, R_xlen_t n) {
  std::memcpy(dst, src, (size_t)n * sizeof(int));
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void copy_i32_sse42(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 15u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 4) * 4;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 4) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm_store_si128((__m128i*)(dst + r),
                        _mm_loadu_si128((const __m128i*)(src + r)));
      }
    }
    for (; r < end; r += 4)
      _mm_store_si128((__m128i*)(dst + r),
                      _mm_loadu_si128((const __m128i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx2")
  static void copy_i32_avx2(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 31u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 8) * 8;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 8) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm256_store_si256((__m256i*)(dst + r),
                           _mm256_loadu_si256((const __m256i*)(src + r)));
      }
    }
    for (; r < end; r += 8)
      _mm256_store_si256((__m256i*)(dst + r),
                         _mm256_loadu_si256((const __m256i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void copy_i32_avx512(int* dst, const int* src, R_xlen_t n) {
    if (n <= 0) return;
    R_xlen_t r = 0;
    while (r < n && ((uintptr_t)(dst + r) & 63u) != 0) { dst[r] = src[r]; ++r; }
    const R_xlen_t end = r + ((n - r) / 16) * 16;
    constexpr R_xlen_t PF = 128;
    if (n >= 4096) {
      for (; r + PF < end; r += 16) {
        __builtin_prefetch(src + r + PF, 0, 0);
        _mm512_store_si512((__m512i*)(dst + r),
                           _mm512_loadu_si512((const __m512i*)(src + r)));
      }
    }
    for (; r < end; r += 16)
      _mm512_store_si512((__m512i*)(dst + r),
                         _mm512_loadu_si512((const __m512i*)(src + r)));
    for (; r < n; ++r) dst[r] = src[r];
  }
#endif

static void (*copy_i32_ptr)(int*, const int*, R_xlen_t) = copy_i32_scalar;
static inline void copy_i32(int* dst, const int* src, R_xlen_t n) {
  copy_i32_ptr(dst, src, n);
}

// ============================================================================
// ISA primitive 6: int2double
// ============================================================================
static void int2double_scalar(const int* s, double* d, size_t n, bool is_logical) {
  for (size_t i = 0; i < n; ++i) {
    int v = s[i];
    if (is_logical) d[i] = (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
    else            d[i] = (v == NA_INTEGER) ? NA_REAL : (double)v;
  }
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static void int2double_sse42(const int* src, double* dst, size_t n, bool is_logical) {
    size_t i = 0;
    const __m128i na_i = _mm_set1_epi32(is_logical ? NA_LOGICAL : NA_INTEGER);
    const __m128d na_d = _mm_set1_pd(NA_REAL);
    const __m128d zero = _mm_setzero_pd();
    const __m128d one  = _mm_set1_pd(1.0);
    for (; i + 4 <= n; i += 4) {
      __m128i vi = _mm_loadu_si128((const __m128i*)(src + i));
      __m128i na_mask = _mm_cmpeq_epi32(vi, na_i);
      __m128d r;
      if (is_logical) {
        __m128i one_mask = _mm_cmpeq_epi32(vi, _mm_set1_epi32(1));
        r = _mm_or_pd(_mm_andnot_pd(_mm_castsi128_pd(one_mask), zero),
                      _mm_and_pd(_mm_castsi128_pd(one_mask), one));
      } else {
        r = _mm_cvtepi32_pd(vi);
      }
      r = _mm_or_pd(_mm_andnot_pd(_mm_castsi128_pd(na_mask), r),
                    _mm_and_pd(_mm_castsi128_pd(na_mask), na_d));
      _mm_storeu_pd(dst + i, r);
    }
    for (; i < n; ++i) {
      int v = src[i];
      if (is_logical) dst[i] = (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
      else            dst[i] = (v == NA_INTEGER) ? NA_REAL : (double)v;
    }
  }

DATAPREP_TARGET("avx2")
  static void int2double_avx2(const int* src, double* dst, size_t n, bool is_logical) {
    size_t i = 0;
    const __m128i na_i = _mm_set1_epi32(is_logical ? NA_LOGICAL : NA_INTEGER);
    const __m256d na_d = _mm256_set1_pd(NA_REAL);
    const __m256d zero = _mm256_setzero_pd();
    const __m256d one  = _mm256_set1_pd(1.0);
    for (; i + 8 <= n; i += 8) {
      __m256i vi = _mm256_loadu_si256((const __m256i*)(src + i));
      __m128i lo = _mm256_castsi256_si128(vi);
      __m128i hi = _mm256_extracti128_si256(vi, 1);
      __m256i na_lo = _mm256_broadcastsi128_si256(_mm_cmpeq_epi32(lo, na_i));
      __m256i na_hi = _mm256_broadcastsi128_si256(_mm_cmpeq_epi32(hi, na_i));
      __m256d r_lo, r_hi;
      if (is_logical) {
        __m256i one_lo = _mm256_broadcastsi128_si256(
          _mm_cmpeq_epi32(lo, _mm_set1_epi32(1)));
        __m256i one_hi = _mm256_broadcastsi128_si256(
          _mm_cmpeq_epi32(hi, _mm_set1_epi32(1)));
        r_lo = _mm256_blendv_pd(zero, one, _mm256_castsi256_pd(one_lo));
        r_hi = _mm256_blendv_pd(zero, one, _mm256_castsi256_pd(one_hi));
      } else {
        r_lo = _mm256_cvtepi32_pd(lo);
        r_hi = _mm256_cvtepi32_pd(hi);
      }
      r_lo = _mm256_blendv_pd(r_lo, na_d, _mm256_castsi256_pd(na_lo));
      r_hi = _mm256_blendv_pd(r_hi, na_d, _mm256_castsi256_pd(na_hi));
      _mm256_storeu_pd(dst + i,     r_lo);
      _mm256_storeu_pd(dst + i + 4, r_hi);
    }
    for (; i < n; ++i) {
      int v = src[i];
      if (is_logical) dst[i] = (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
      else            dst[i] = (v == NA_INTEGER) ? NA_REAL : (double)v;
    }
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static void int2double_avx512(const int* src, double* dst, size_t n, bool is_logical) {
    size_t i = 0;
    const __m256i na_i = _mm256_set1_epi32(is_logical ? NA_LOGICAL : NA_INTEGER);
    const __m512d na_d = _mm512_set1_pd(NA_REAL);
    const __m512d zero = _mm512_setzero_pd();
    const __m512d one  = _mm512_set1_pd(1.0);
    constexpr size_t PF = 64;
    if (n >= 4096) {
      for (; i + 16 <= n; i += 16) {
        __builtin_prefetch(src + i + PF, 0, 0);
        __m512i vi = _mm512_loadu_si512((const __m512i*)(src + i));
        __m256i lo = _mm512_extracti32x8_epi32(vi, 0);
        __m256i hi = _mm512_extracti32x8_epi32(vi, 1);
        __mmask8 na_lo = _mm256_cmpeq_epi32_mask(lo, na_i);
        __mmask8 na_hi = _mm256_cmpeq_epi32_mask(hi, na_i);
        __m512d r_lo, r_hi;
        if (is_logical) {
          __mmask8 on_lo = _mm256_cmpneq_epi32_mask(lo, _mm256_setzero_si256());
          __mmask8 on_hi = _mm256_cmpneq_epi32_mask(hi, _mm256_setzero_si256());
          r_lo = _mm512_mask_blend_pd(on_lo, zero, one);
          r_hi = _mm512_mask_blend_pd(on_hi, zero, one);
        } else {
          r_lo = _mm512_cvtepi32_pd(lo);
          r_hi = _mm512_cvtepi32_pd(hi);
        }
        r_lo = _mm512_mask_blend_pd(na_lo, r_lo, na_d);
        r_hi = _mm512_mask_blend_pd(na_hi, r_hi, na_d);
        _mm512_storeu_pd(dst + i,     r_lo);
        _mm512_storeu_pd(dst + i + 8, r_hi);
      }
    } else {
      for (; i + 16 <= n; i += 16) {
        __m512i vi = _mm512_loadu_si512((const __m512i*)(src + i));
        __m256i lo = _mm512_extracti32x8_epi32(vi, 0);
        __m256i hi = _mm512_extracti32x8_epi32(vi, 1);
        __mmask8 na_lo = _mm256_cmpeq_epi32_mask(lo, na_i);
        __mmask8 na_hi = _mm256_cmpeq_epi32_mask(hi, na_i);
        __m512d r_lo, r_hi;
        if (is_logical) {
          __mmask8 on_lo = _mm256_cmpneq_epi32_mask(lo, _mm256_setzero_si256());
          __mmask8 on_hi = _mm256_cmpneq_epi32_mask(hi, _mm256_setzero_si256());
          r_lo = _mm512_mask_blend_pd(on_lo, zero, one);
          r_hi = _mm512_mask_blend_pd(on_hi, zero, one);
        } else {
          r_lo = _mm512_cvtepi32_pd(lo);
          r_hi = _mm512_cvtepi32_pd(hi);
        }
        r_lo = _mm512_mask_blend_pd(na_lo, r_lo, na_d);
        r_hi = _mm512_mask_blend_pd(na_hi, r_hi, na_d);
        _mm512_storeu_pd(dst + i,     r_lo);
        _mm512_storeu_pd(dst + i + 8, r_hi);
      }
    }
    for (; i < n; ++i) {
      int v = src[i];
      if (is_logical) dst[i] = (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
      else            dst[i] = (v == NA_INTEGER) ? NA_REAL : (double)v;
    }
  }
#endif

static void (*int2double_ptr)(const int*, double*, size_t, bool) = int2double_scalar;

// ============================================================================
// ISA primitive 7: count_keep_double
// ============================================================================
static R_xlen_t count_keep_double_scalar(const double* src, R_xlen_t n) {
  R_xlen_t c = 0;
  for (R_xlen_t i = 0; i < n; ++i) c += !ISNAN(src[i]);
  return c;
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static R_xlen_t count_keep_double_sse42(const double* src, R_xlen_t n) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const __m128d na = _mm_set1_pd(NA_REAL);
    const size_t m = (size_t)n - (size_t)n % 2;
    for (; i < m; i += 2) {
      __m128d v = _mm_loadu_pd(src + i);
      c += __builtin_popcount((unsigned)_mm_movemask_pd(_mm_cmpneq_pd(v, na)));
    }
    for (; i < (size_t)n; ++i) c += !ISNAN(src[i]);
    return c;
  }

DATAPREP_TARGET("avx2")
  static R_xlen_t count_keep_double_avx2(const double* src, R_xlen_t n) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const __m256d na = _mm256_set1_pd(NA_REAL);
    const size_t m = (size_t)n - (size_t)n % 4;
    for (; i < m; i += 4) {
      __m256d v = _mm256_loadu_pd(src + i);
      c += __builtin_popcount(
        (unsigned)_mm256_movemask_pd(_mm256_cmp_pd(v, na, _CMP_NEQ_UQ)));
    }
    for (; i < (size_t)n; ++i) c += !ISNAN(src[i]);
    return c;
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static R_xlen_t count_keep_double_avx512(const double* src, R_xlen_t n) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const __m512d na = _mm512_set1_pd(NA_REAL);
    const size_t m = (size_t)n - (size_t)n % 8;
    for (; i < m; i += 8) {
      __m512d v = _mm512_loadu_pd(src + i);
      c += __builtin_popcount(
        (unsigned)_mm512_cmp_pd_mask(v, na, _CMP_NEQ_UQ));
    }
    for (; i < (size_t)n; ++i) c += !ISNAN(src[i]);
    return c;
  }
#endif

static R_xlen_t (*count_keep_double_ptr)(const double*, R_xlen_t) = count_keep_double_scalar;

// ============================================================================
// ISA primitive 8: count_keep_int
// ============================================================================
static R_xlen_t count_keep_int_scalar(const int* src, R_xlen_t n, bool is_logical) {
  R_xlen_t c = 0;
  int na = is_logical ? NA_LOGICAL : NA_INTEGER;
  for (R_xlen_t i = 0; i < n; ++i) c += (src[i] != na);
  return c;
}

#if DATAPREP_ARCH_X86
DATAPREP_TARGET("sse4.2")
  static R_xlen_t count_keep_int_sse42(const int* src, R_xlen_t n, bool is_logical) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const int na_val = is_logical ? NA_LOGICAL : NA_INTEGER;
    const __m128i na_vec = _mm_set1_epi32(na_val);
    const size_t m = (size_t)n - (size_t)n % 4;
    for (; i < m; i += 4) {
      __m128i v = _mm_loadu_si128((const __m128i*)(src + i));
      int mask = _mm_movemask_epi8(_mm_cmpeq_epi32(v, na_vec));
      c += 4 - __builtin_popcount((unsigned)mask) / 4;
    }
    for (; i < (size_t)n; ++i) c += (src[i] != na_val);
    return c;
  }

DATAPREP_TARGET("avx2")
  static R_xlen_t count_keep_int_avx2(const int* src, R_xlen_t n, bool is_logical) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const int na_val = is_logical ? NA_LOGICAL : NA_INTEGER;
    const __m256i na_vec = _mm256_set1_epi32(na_val);
    const size_t m = (size_t)n - (size_t)n % 8;
    for (; i < m; i += 8) {
      __m256i v = _mm256_loadu_si256((const __m256i*)(src + i));
      int mask = _mm256_movemask_epi8(_mm256_cmpeq_epi32(v, na_vec));
      c += 8 - __builtin_popcount((unsigned)mask) / 4;
    }
    for (; i < (size_t)n; ++i) c += (src[i] != na_val);
    return c;
  }

DATAPREP_TARGET("avx512f,avx512vl,avx512bw,avx512dq")
  static R_xlen_t count_keep_int_avx512(const int* src, R_xlen_t n, bool is_logical) {
    if (n <= 0) return 0;
    R_xlen_t c = 0; size_t i = 0;
    const int na_val = is_logical ? NA_LOGICAL : NA_INTEGER;
    const __m512i na_vec = _mm512_set1_epi32(na_val);
    const size_t m = (size_t)n - (size_t)n % 16;
    for (; i < m; i += 16) {
      __m512i v = _mm512_loadu_si512((const __m512i*)(src + i));
      c += 16 - __builtin_popcount(
        (unsigned)_mm512_cmpeq_epi32_mask(v, na_vec));
    }
    for (; i < (size_t)n; ++i) c += (src[i] != na_val);
    return c;
  }
#endif

static R_xlen_t (*count_keep_int_ptr)(const int*, R_xlen_t, bool) = count_keep_int_scalar;

static inline void fill_int_exact(int* dst, int v, int n) {
  for (int i = 0; i < n; ++i) dst[i] = v;
}
static inline void fill_double_exact(double* dst, double v, int n) {
  for (int i = 0; i < n; ++i) dst[i] = v;
}

// ============================================================================
// Runtime feature detection
// ============================================================================
static void init_cpu_features() DATAPREP_COLD;
static void init_cpu_features() {
  static bool done = false;
  if (done) return;
  done = true;
#if DATAPREP_HAS_GCC_ATTR && DATAPREP_ARCH_X86
  if (__builtin_cpu_supports("avx512f") &&
      __builtin_cpu_supports("avx512vl") &&
      __builtin_cpu_supports("avx512bw") &&
      __builtin_cpu_supports("avx512dq")) {
    nt_fill_i32_ptr         = nt_fill_i32_avx512;
    nt_copy_pd_ptr          = nt_copy_pd_avx512;
    copy_pd_ptr             = copy_pd_avx512;
    nt_copy_i32_ptr         = nt_copy_i32_avx512;
    copy_i32_ptr            = copy_i32_avx512;
    int2double_ptr          = int2double_avx512;
    count_keep_double_ptr   = count_keep_double_avx512;
    count_keep_int_ptr      = count_keep_int_avx512;
    return;
  }
  if (__builtin_cpu_supports("avx2")) {
    nt_fill_i32_ptr         = nt_fill_i32_avx2;
    nt_copy_pd_ptr          = nt_copy_pd_avx2;
    copy_pd_ptr             = copy_pd_avx2;
    nt_copy_i32_ptr         = nt_copy_i32_avx2;
    copy_i32_ptr            = copy_i32_avx2;
    int2double_ptr          = int2double_avx2;
    count_keep_double_ptr   = count_keep_double_avx2;
    count_keep_int_ptr      = count_keep_int_avx2;
    return;
  }
  if (__builtin_cpu_supports("sse4.2")) {
    nt_fill_i32_ptr         = nt_fill_i32_sse42;
    nt_copy_pd_ptr          = nt_copy_pd_sse42;
    copy_pd_ptr             = copy_pd_sse42;
    nt_copy_i32_ptr         = nt_copy_i32_sse42;
    copy_i32_ptr            = copy_i32_sse42;
    int2double_ptr          = int2double_sse42;
    count_keep_double_ptr   = count_keep_double_sse42;
    count_keep_int_ptr      = count_keep_int_sse42;
    return;
  }
#endif
}

// ============================================================================
// Cached CHARSXP
// ============================================================================
struct CachedStr {
  SEXP v = R_NilValue;
  SEXP operator()(const char* s) {
    if (DATAPREP_UNLIKELY(v == R_NilValue)) {
      v = Rf_mkString(s);
      R_PreserveObject(v);
    }
    return v;
  }
};
static CachedStr g_df_class, g_fac_class, g_var_name, g_val_name;

// ============================================================================
// L3 detection
// ============================================================================
static size_t g_l3_size = 32ULL << 20;
static void detect_l3_once() DATAPREP_COLD;
static void detect_l3_once() {
  static bool done = false;
  if (done) return;
  done = true;
#if defined(__linux__)
  FILE* f = fopen("/sys/devices/system/cpu/cpu0/cache/index3/size", "r");
  if (f) {
    char buf[32] = {0};
    if (fgets(buf, sizeof(buf), f)) {
      char* p = buf;
      while (*p && !isdigit((unsigned char)*p)) ++p;
      unsigned long long v = strtoull(p, nullptr, 10);
      if (strchr(p, 'M') || strchr(p, 'm')) v <<= 20;
      else if (strchr(p, 'K') || strchr(p, 'k')) v <<= 10;
      if (v >= (1ULL << 20)) g_l3_size = (size_t)v;
    }
    fclose(f);
  }
#endif
}

// ============================================================================
// Levels cache
// ============================================================================
struct LevelsCache {
  SEXP col_names = R_NilValue;
  SEXP levels    = R_NilValue;
  std::vector<int> idx;
  bool big = false;
};
static LevelsCache g_levels;

static SEXP get_or_make_levels(SEXP col_names, const std::vector<int>& meas_idx) {
  const size_t n = meas_idx.size();
  const bool big = n > 32;
  if (g_levels.col_names == col_names && g_levels.levels != R_NilValue &&
      g_levels.big == big && g_levels.idx == meas_idx) {
    return g_levels.levels;
  }
  SEXP lv = Rf_allocVector(STRSXP, (R_xlen_t)n);
  for (size_t k = 0; k < n; ++k)
    SET_STRING_ELT(lv, k, STRING_ELT(col_names, meas_idx[k]));
  R_PreserveObject(lv);
  g_levels.col_names = col_names;
  g_levels.levels    = lv;
  g_levels.idx       = meas_idx;
  g_levels.big       = big;
  return lv;
}

// ============================================================================
// Per-thread scratch
// ============================================================================
struct Scratch {
  std::vector<int>  id_idx, meas_idx;
  std::vector<char> is_id, is_meas;
  std::vector<const void*> meas_ptrs;
  std::vector<SEXPTYPE>    meas_types;
  std::vector<char>        meas_is_factor;
  std::vector<const void*> id_ptrs;
  std::vector<SEXPTYPE>    id_types;
  std::vector<void*>       out_id_ptrs;
  std::vector<size_t>      id_elem_sizes;
  std::vector<R_xlen_t>    keep_idx;
  std::vector<int>         var_pat8;
  Scratch() {
    id_idx.reserve(64); meas_idx.reserve(1024);
    is_id.reserve(1024); is_meas.reserve(1024);
    meas_ptrs.reserve(1024); meas_types.reserve(1024);
    meas_is_factor.reserve(1024);
    id_ptrs.reserve(64); id_types.reserve(64);
    out_id_ptrs.reserve(64); id_elem_sizes.reserve(64);
    keep_idx.reserve(4096); var_pat8.reserve(1024);
  }
};
static thread_local Scratch g_scratch;

// ============================================================================
// Thread cap: physical cores only for memory-bound workloads.
// Only defined when OpenMP is available: all call sites live inside
// #ifdef _OPENMP blocks, so a non-OpenMP build simply never sees this.
// ============================================================================
#ifdef _OPENMP
static int get_thread_cap() {
  static const int cap = []() DATAPREP_COLD {
    int n = omp_get_num_procs();
    if (n < 1) n = 1;
    if (n >= 8) n = n / 2;
    if (n < 1) n = 1;
    if (n > 256) n = 256;
    if (const char* e = getenv("DATAPREP_THREADS")) {
      int v = atoi(e); if (v > 0 && v < n) n = v;
    }
    if (const char* e = getenv("OMP_THREAD_LIMIT")) {
      int v = atoi(e); if (v > 0 && v < n) n = v;
    }
    return n;
  }();
  return cap;
}
#endif

static inline size_t sizeof_sexp(SEXPTYPE t) {
  switch (t) {
  case INTSXP:  return sizeof(int);
  case REALSXP: return sizeof(double);
  case LGLSXP:  return sizeof(int);
  case STRSXP:  return sizeof(SEXP);
  default:      return 0;
  }
}

// ============================================================================
// Lazy init
// ============================================================================
static void melt_lazy_init() {
  static bool done = false;
  if (DATAPREP_LIKELY(done)) return;
  done = true;
#if defined(__GLIBC__)
  const char* off = getenv("DATAPREP_NO_MMAP_POOL");
  if (!(off && off[0] == '1')) {
    mallopt(M_MMAP_THRESHOLD, 64 * 1024 * 1024);
    mallopt(M_TRIM_THRESHOLD, INT_MAX);
    mallopt(M_TOP_PAD, 64 * 1024 * 1024);
  }
#endif
  init_cpu_features();
  detect_l3_once();
#ifdef _OPENMP
#ifdef _WIN32
  if (getenv("OMP_PROC_BIND") == nullptr) _putenv_s("OMP_PROC_BIND", "spread");
  if (getenv("OMP_PLACES")   == nullptr) _putenv_s("OMP_PLACES",   "cores");
#else
  if (getenv("OMP_PROC_BIND") == nullptr) setenv("OMP_PROC_BIND", "spread", 0);
  if (getenv("OMP_PLACES")   == nullptr) setenv("OMP_PLACES",   "cores", 0);
#endif
  static bool omp_fixed = false;
  if (!omp_fixed) {
    omp_set_dynamic(0);
    if (getenv("OMP_NUM_THREADS") == nullptr)
      omp_set_num_threads(get_thread_cap());
    omp_fixed = true;
  }
#endif
}

} // anonymous namespace

// ============================================================================
// Column index parsing
// ============================================================================
static void infer_id_cols(SEXP df, std::vector<int>& id_idx, std::vector<int>& meas_idx) {
  int ncols = Rf_length(df);
  id_idx.clear(); meas_idx.clear();
  for (int i = 0; i < ncols; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    SEXPTYPE t = TYPEOF(col);
    if (t != REALSXP && t != INTSXP && t != LGLSXP) id_idx.push_back(i);
    else if (t == INTSXP && Rf_isFactor(col))        id_idx.push_back(i);
    else                                             meas_idx.push_back(i);
  }
}

static void resolve_id(SEXP df, SEXP id_spec,
                       std::vector<int>& id_idx, std::vector<int>& meas_idx) {
  int ncols = Rf_length(df);
  id_idx.clear(); meas_idx.clear();
  id_idx.reserve(ncols); meas_idx.reserve(ncols);
  if (Rf_isNull(id_spec)) { infer_id_cols(df, id_idx, meas_idx); return; }

  if (TYPEOF(id_spec) == INTSXP) {
    R_xlen_t len = XLENGTH(id_spec);
    const int* ptr = INTEGER(id_spec);
    if ((int)g_scratch.is_id.size() < ncols) g_scratch.is_id.resize(ncols);
    char* mask = g_scratch.is_id.data();
    std::memset(mask, 0, (size_t)ncols);
    bool has_neg = false;
    for (R_xlen_t i = 0; i < len; ++i) if (ptr[i] < 0) { has_neg = true; break; }
    if (has_neg) {
      if ((int)g_scratch.is_meas.size() < ncols) g_scratch.is_meas.resize(ncols);
      char* mm = g_scratch.is_meas.data();
      std::memset(mm, 0, (size_t)ncols);
      for (R_xlen_t i = 0; i < len; ++i) {
        int a = std::abs(ptr[i]) - 1;
        if (a >= 0 && a < ncols) mm[a] = 1;
      }
      for (int i = 0; i < ncols; ++i) if (!mm[i]) mask[i] = 1;
    } else {
      for (R_xlen_t i = 0; i < len; ++i) {
        int a = ptr[i] - 1;
        if (a >= 0 && a < ncols) mask[a] = 1;
      }
    }
    for (int i = 0; i < ncols; ++i) (mask[i] ? id_idx : meas_idx).push_back(i);
    return;
  }

  if (TYPEOF(id_spec) == STRSXP) {
    SEXP names = Rf_getAttrib(df, R_NamesSymbol);
    if (Rf_isNull(names)) stop("data frame has no column names");
    R_xlen_t len = XLENGTH(id_spec);
    if ((int)g_scratch.is_id.size() < ncols) g_scratch.is_id.resize(ncols);
    char* mask = g_scratch.is_id.data();
    std::memset(mask, 0, (size_t)ncols);
    for (R_xlen_t i = 0; i < len; ++i) {
      const char* tgt = CHAR(STRING_ELT(id_spec, i));
      for (int j = 0; j < ncols; ++j) {
        if (std::strcmp(CHAR(STRING_ELT(names, j)), tgt) == 0) {
          mask[j] = 1; break;
        }
      }
    }
    for (int i = 0; i < ncols; ++i) (mask[i] ? id_idx : meas_idx).push_back(i);
    return;
  }
  stop("invalid id argument");
}

// ============================================================================
// NA helpers
// ============================================================================
static inline R_xlen_t count_keep_col(const void* ptr, SEXPTYPE t, bool fac, R_xlen_t n) {
  if (DATAPREP_UNLIKELY(fac)) return 0;
  if (t == REALSXP) return count_keep_double_ptr((const double*)ptr, n);
  if (t == INTSXP || t == LGLSXP)
    return count_keep_int_ptr((const int*)ptr, n, t == LGLSXP);
  return 0;
}

static inline bool is_na_meas_at(const void* ptr, SEXPTYPE t, bool fac, R_xlen_t i) {
  if (t == REALSXP)        return ISNAN(((const double*)ptr)[i]);
  if (t == INTSXP && !fac) return ((const int*)ptr)[i] == NA_INTEGER;
  if (t == LGLSXP)         return ((const int*)ptr)[i] == NA_LOGICAL;
  return true;
}

static inline double meas_at_as_double(const void* ptr, SEXPTYPE t, bool fac, R_xlen_t i) {
  if (t == REALSXP) return ((const double*)ptr)[i];
  if (t == INTSXP && !fac) {
    int v = ((const int*)ptr)[i];
    return (v == NA_INTEGER) ? NA_REAL : (double)v;
  }
  if (t == LGLSXP) {
    int v = ((const int*)ptr)[i];
    return (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
  }
  return NA_REAL;
}

static inline void* writable_ptr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:  return (void*)INTEGER(x);
  case LGLSXP:  return (void*)LOGICAL(x);
  case REALSXP: return (void*)REAL(x);
  case STRSXP:  return (void*)const_cast<SEXP*>(STRING_PTR_RO(x));
  default:      return nullptr;
  }
}
static inline const void* readonly_ptr(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:  return (const void*)INTEGER_RO(x);
  case LGLSXP:  return (const void*)LOGICAL_RO(x);
  case REALSXP: return (const void*)REAL_RO(x);
  default:      return DATAPTR_RO(x);
  }
}

// ============================================================================
// variable column: factor -> character conversion (post-build)
// ============================================================================
static void maybe_make_var_character(SEXP out, int n_id, R_xlen_t total_out) {
  SEXP var_col = VECTOR_ELT(out, n_id);
  if (TYPEOF(var_col) != INTSXP) return;
  SEXP lv = Rf_getAttrib(var_col, R_LevelsSymbol);
  if (Rf_isNull(lv)) return;
  const R_xlen_t nl = XLENGTH(lv);
  SEXP new_var = PROTECT(Rf_allocVector(STRSXP, total_out));
  const int* pi = INTEGER_RO(var_col);
  for (R_xlen_t i = 0; i < total_out; ++i) {
    int v = pi[i];
    if (v >= 1 && (R_xlen_t)v <= nl) {
      SET_STRING_ELT(new_var, i, STRING_ELT(lv, v - 1));
    } else {
      SET_STRING_ELT(new_var, i, NA_STRING);
    }
  }
  SET_VECTOR_ELT(out, n_id, new_var);
  UNPROTECT(1);
}

// ============================================================================
// Small-shape fast paths
// ============================================================================
DATAPREP_HOT
static SEXP melt_tiny_cpp(SEXP df,
                          const std::vector<int>& id_idx,
                          const std::vector<int>& meas_idx,
                          SEXP col_names,
                          SEXP variable_name, SEXP value_name,
                          bool row_major, bool as_factor) {
  int n_id   = (int)id_idx.size();
  int n_meas = (int)meas_idx.size();
  R_xlen_t n = XLENGTH(VECTOR_ELT(df, 0));
  R_xlen_t total_out = n * (R_xlen_t)n_meas;

  SEXP out       = PROTECT(Rf_allocVector(VECSXP, n_id + 2));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, n_id + 2));

  for (int i = 0; i < n_id; ++i) {
    SEXP src = VECTOR_ELT(df, id_idx[i]);
    SEXPTYPE t = TYPEOF(src);
    SEXP dst = PROTECT(alloc_hint(t, total_out));
    Rf_copyMostAttrib(src, dst);
    SET_VECTOR_ELT(out, i, dst);
    SET_STRING_ELT(out_names, i, STRING_ELT(col_names, id_idx[i]));
    UNPROTECT(1);
  }
  SEXP var_col = PROTECT(alloc_hint(INTSXP, total_out));
  SET_VECTOR_ELT(out, n_id, var_col);
  SET_STRING_ELT(out_names, n_id,
                 (!Rf_isNull(variable_name) && TYPEOF(variable_name) == STRSXP) ?
                   STRING_ELT(variable_name, 0) : g_var_name("variable"));
  {
    SEXP lv = get_or_make_levels(col_names, meas_idx);
    Rf_setAttrib(var_col, R_LevelsSymbol, lv);
    Rf_setAttrib(var_col, R_ClassSymbol, g_fac_class("factor"));
  }
  SEXP val_col = PROTECT(alloc_hint(REALSXP, total_out));
  SET_VECTOR_ELT(out, n_id + 1, val_col);
  SET_STRING_ELT(out_names, n_id + 1,
                 (!Rf_isNull(value_name) && TYPEOF(value_name) == STRSXP) ?
                   STRING_ELT(value_name, 0) : g_val_name("value"));

  int*    pvar = INTEGER(var_col);
  double* pval = REAL(val_col);

  if (row_major) {
    // ---- Row-major: iterate rows outermost --------------------------------
    std::vector<const void*>  mp(n_meas);
    std::vector<SEXPTYPE>     mt(n_meas);
    std::vector<char>         mfac(n_meas);
    for (int k = 0; k < n_meas; ++k) {
      SEXP col = VECTOR_ELT(df, meas_idx[k]);
      mp[k]    = readonly_ptr(col);
      mt[k]    = TYPEOF(col);
      mfac[k]  = (char)(mt[k] == INTSXP && Rf_isFactor(col));
    }
    std::vector<const void*>  ip(n_id);
    std::vector<void*>        idp(n_id);
    std::vector<SEXPTYPE>     it(n_id);
    for (int i = 0; i < n_id; ++i) {
      SEXP s = VECTOR_ELT(df, id_idx[i]);
      ip[i]  = readonly_ptr(s);
      idp[i] = writable_ptr(VECTOR_ELT(out, i));
      it[i]  = TYPEOF(s);
    }

    for (R_xlen_t r = 0; r < n; ++r) {
      double* vr = pval + r * n_meas;
      int*    kr = pvar + r * n_meas;
      for (int k = 0; k < n_meas; ++k) {
        SEXPTYPE t = mt[k];
        const void* src = mp[k];
        double v = NA_REAL;
        if (t == REALSXP) {
          v = ((const double*)src)[r];
        } else if (t == INTSXP && !mfac[k]) {
          int x = ((const int*)src)[r];
          v = (x == NA_INTEGER) ? NA_REAL : (double)x;
        } else if (t == LGLSXP) {
          int x = ((const int*)src)[r];
          v = (x == NA_LOGICAL) ? NA_REAL : (x ? 1.0 : 0.0);
        }
        vr[k] = v;
        kr[k] = k + 1;
      }
      for (int i = 0; i < n_id; ++i) {
        SEXPTYPE t = it[i];
        const void* src = ip[i];
        if (t == INTSXP || t == LGLSXP) {
          int x = ((const int*)src)[r];
          int* d = (int*)idp[i] + r * n_meas;
          for (int k = 0; k < n_meas; ++k) d[k] = x;
        } else if (t == REALSXP) {
          double x = ((const double*)src)[r];
          double* d = (double*)idp[i] + r * n_meas;
          for (int k = 0; k < n_meas; ++k) d[k] = x;
        } else if (t == STRSXP) {
          SEXP x = ((const SEXP*)src)[r];
          SEXP d_col = VECTOR_ELT(out, i);
          R_xlen_t base = r * n_meas;
          for (int k = 0; k < n_meas; ++k) SET_STRING_ELT(d_col, base + k, x);
        }
      }
    }
  } else {
    // ---- Column-major: iterate measure columns outermost ------------------
    for (int k = 0; k < n_meas; ++k) {
      SEXP col = VECTOR_ELT(df, meas_idx[k]);
      SEXPTYPE t = TYPEOF(col);
      const int kk = k + 1;
      double* pd = pval + (R_xlen_t)k * n;
      int*    pv = pvar + (R_xlen_t)k * n;
      nt_fill_i32(pv, kk, n);
      if (t == REALSXP) {
        nt_copy_pd(pd, (const double*)readonly_ptr(col), n);
      } else if (t == INTSXP && !Rf_isFactor(col)) {
        int2double_ptr((const int*)readonly_ptr(col), pd, (size_t)n, false);
      } else if (t == LGLSXP) {
        int2double_ptr((const int*)readonly_ptr(col), pd, (size_t)n, true);
      } else {
        std::fill(pd, pd + n, NA_REAL);
      }
    }

    for (int i = 0; i < n_id; ++i) {
      SEXP s_col = VECTOR_ELT(df, id_idx[i]);
      SEXP d_col = VECTOR_ELT(out, i);
      SEXPTYPE t = TYPEOF(s_col);
      const char* src = (const char*)readonly_ptr(s_col);
      char* dst_base = (char*)writable_ptr(d_col);
      size_t es = sizeof_sexp(t);
      if (!es) continue;

      if (t == STRSXP) {
        copy_sexp((SEXP*)dst_base, (const SEXP*)src, n);
      } else if (t == REALSXP) {
        copy_pd((double*)dst_base, (const double*)src, n);
      } else if (t == INTSXP || t == LGLSXP) {
        copy_i32((int*)dst_base, (const int*)src, n);
      } else {
        std::memcpy(dst_base, src, (size_t)n * es);
      }

      size_t filled = 1;
      while (filled < (size_t)n_meas) {
        size_t tc = filled;
        if (tc > (size_t)n_meas - filled) tc = (size_t)n_meas - filled;
        char* dst_out = dst_base + filled * (size_t)n * es;

        if (t == STRSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                       (const double*)dst_base, n);
          }
        } else if (t == REALSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                       (const double*)dst_base, n);
          }
        } else if (t == INTSXP || t == LGLSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_i32((int*)(dst_out + b * (size_t)n * es),
                        (const int*)dst_base, n);
          }
        } else {
          std::memcpy(dst_out, dst_base, tc * (size_t)n * es);
        }
        filled += tc;
      }
    }
  }

  if (DATAPREP_UNLIKELY(!as_factor)) {
    maybe_make_var_character(out, n_id, total_out);
  }

  Rf_setAttrib(out, R_NamesSymbol, out_names);
  {
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = (total_out <= (R_xlen_t)INT_MAX) ? -(int)total_out : NA_INTEGER;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    Rf_setAttrib(out, R_ClassSymbol, g_df_class("data.frame"));
    UNPROTECT(1);
  }
  UNPROTECT(4);
  return out;
}

DATAPREP_HOT
static SEXP melt_small_cpp(SEXP df,
                           const std::vector<int>& id_idx,
                           const std::vector<int>& meas_idx,
                           SEXP col_names,
                           SEXP variable_name, SEXP value_name,
                           bool row_major, bool as_factor) {
  int n_id   = (int)id_idx.size();
  int n_meas = (int)meas_idx.size();
  R_xlen_t n = XLENGTH(VECTOR_ELT(df, 0));
  R_xlen_t total_out = n * (R_xlen_t)n_meas;

  SEXP out       = PROTECT(Rf_allocVector(VECSXP, n_id + 2));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, n_id + 2));
  for (int i = 0; i < n_id; ++i) {
    SEXP src = VECTOR_ELT(df, id_idx[i]);
    SEXPTYPE t = TYPEOF(src);
    SEXP dst = PROTECT(alloc_hint(t, total_out));
    Rf_copyMostAttrib(src, dst);
    SET_VECTOR_ELT(out, i, dst);
    SET_STRING_ELT(out_names, i, STRING_ELT(col_names, id_idx[i]));
    UNPROTECT(1);
  }
  SEXP var_col = PROTECT(alloc_hint(INTSXP, total_out));
  SET_VECTOR_ELT(out, n_id, var_col);
  SET_STRING_ELT(out_names, n_id,
                 (!Rf_isNull(variable_name) && TYPEOF(variable_name) == STRSXP) ?
                   STRING_ELT(variable_name, 0) : g_var_name("variable"));
  {
    SEXP lv = get_or_make_levels(col_names, meas_idx);
    Rf_setAttrib(var_col, R_LevelsSymbol, lv);
    Rf_setAttrib(var_col, R_ClassSymbol, g_fac_class("factor"));
  }
  SEXP val_col = PROTECT(alloc_hint(REALSXP, total_out));
  SET_VECTOR_ELT(out, n_id + 1, val_col);
  SET_STRING_ELT(out_names, n_id + 1,
                 (!Rf_isNull(value_name) && TYPEOF(value_name) == STRSXP) ?
                   STRING_ELT(value_name, 0) : g_val_name("value"));

  int* pvar = INTEGER(var_col);
  double* pval = REAL(val_col);

  std::vector<const void*>& src_ptrs      = g_scratch.meas_ptrs;
  std::vector<SEXPTYPE>&    src_types     = g_scratch.meas_types;
  std::vector<char>&        src_is_factor = g_scratch.meas_is_factor;
  std::vector<const void*>& id_src        = g_scratch.id_ptrs;
  std::vector<void*>&       id_dst        = g_scratch.out_id_ptrs;
  std::vector<size_t>&      id_es         = g_scratch.id_elem_sizes;
  std::vector<SEXPTYPE>&    id_type       = g_scratch.id_types;

  for (int k = 0; k < n_meas; ++k) {
    SEXP col = VECTOR_ELT(df, meas_idx[k]);
    SEXPTYPE t = TYPEOF(col);
    src_ptrs.push_back(readonly_ptr(col));
    src_types.push_back(t);
    src_is_factor.push_back((char)(t == INTSXP && Rf_isFactor(col)));
  }
  for (int i = 0; i < n_id; ++i) {
    SEXP s = VECTOR_ELT(df, id_idx[i]);
    SEXP d = VECTOR_ELT(out, i);
    id_src.push_back(readonly_ptr(s));
    id_dst.push_back(writable_ptr(d));
    id_es.push_back(sizeof_sexp(TYPEOF(s)));
    id_type.push_back(TYPEOF(s));
  }

  if (!row_major) {
    double* pv2 = pval;
    int*    pv  = pvar;
    for (int k = 0; k < n_meas; ++k, pv2 += n, pv += n) {
      SEXPTYPE t = src_types[k];
      int kk = k + 1;
      nt_fill_i32(pv, kk, n);
      if (t == REALSXP) {
        nt_copy_pd(pv2, (const double*)src_ptrs[k], n);
      } else if (t == INTSXP && !src_is_factor[k]) {
        int2double_ptr((const int*)src_ptrs[k], pv2, (size_t)n, false);
      } else if (t == LGLSXP) {
        int2double_ptr((const int*)src_ptrs[k], pv2, (size_t)n, true);
      } else {
        std::fill(pv2, pv2 + n, NA_REAL);
      }
    }

    // Id columns: doubling copy
    for (int i = 0; i < n_id; ++i) {
      SEXPTYPE t = id_type[i];
      size_t es = id_es[i];
      if (!es) continue;
      const char* s = (const char*)id_src[i];
      char* d_base = (char*)id_dst[i];

      // First copy (normal store)
      if (t == STRSXP) {
        copy_sexp((SEXP*)d_base, (const SEXP*)s, n);
      } else if (t == REALSXP) {
        copy_pd((double*)d_base, (const double*)s, n);
      } else if (t == INTSXP || t == LGLSXP) {
        copy_i32((int*)d_base, (const int*)s, n);
      } else {
        std::memcpy(d_base, s, (size_t)n * es);
      }

      // Doubling (NT stores)
      size_t filled = 1;
      while (filled < (size_t)n_meas) {
        size_t tc = filled;
        if (tc > (size_t)n_meas - filled) tc = (size_t)n_meas - filled;
        char* dst_out = d_base + filled * (size_t)n * es;

        if (t == STRSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                       (const double*)d_base, n);
          }
        } else if (t == REALSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                       (const double*)d_base, n);
          }
        } else if (t == INTSXP || t == LGLSXP) {
          for (size_t b = 0; b < tc; ++b) {
            nt_copy_i32((int*)(dst_out + b * (size_t)n * es),
                        (const int*)d_base, n);
          }
        } else {
          std::memcpy(dst_out, d_base, tc * (size_t)n * es);
        }
        filled += tc;
      }
    }
  } else {
    for (R_xlen_t r = 0; r < n; ++r) {
      double* vr = pval + r * n_meas;
      int*    kr = pvar + r * n_meas;
      for (int k = 0; k < n_meas; ++k) {
        SEXPTYPE t = src_types[k];
        double v = NA_REAL;
        if (t == REALSXP) v = ((const double*)src_ptrs[k])[r];
        else if (t == INTSXP && !src_is_factor[k]) {
          int x = ((const int*)src_ptrs[k])[r];
          v = (x == NA_INTEGER) ? NA_REAL : (double)x;
        } else if (t == LGLSXP) {
          int x = ((const int*)src_ptrs[k])[r];
          v = (x == NA_LOGICAL) ? NA_REAL : (x ? 1.0 : 0.0);
        }
        vr[k] = v;
        kr[k] = k + 1;
      }
      for (int i = 0; i < n_id; ++i) {
        SEXPTYPE t = id_type[i];
        if (t == INTSXP || t == LGLSXP) {
          int x = ((const int*)id_src[i])[r];
          int* d = (int*)id_dst[i] + r * n_meas;
          for (int k = 0; k < n_meas; ++k) d[k] = x;
        } else if (t == REALSXP) {
          double x = ((const double*)id_src[i])[r];
          double* d = (double*)id_dst[i] + r * n_meas;
          for (int k = 0; k < n_meas; ++k) d[k] = x;
        } else if (t == STRSXP) {
          SEXP x = ((const SEXP*)id_src[i])[r];
          SEXP d_col = VECTOR_ELT(out, i);
          R_xlen_t base = r * n_meas;
          for (int k = 0; k < n_meas; ++k) SET_STRING_ELT(d_col, base + k, x);
        }
      }
    }
  }
  if (DATAPREP_UNLIKELY(!as_factor)) {
    maybe_make_var_character(out, n_id, total_out);
  }

  Rf_setAttrib(out, R_NamesSymbol, out_names);
  {
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = (total_out <= (R_xlen_t)INT_MAX) ? -(int)total_out : NA_INTEGER;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    Rf_setAttrib(out, R_ClassSymbol, g_df_class("data.frame"));
    UNPROTECT(1);
  }
  UNPROTECT(4);
  return out;
}

// ============================================================================
// Fused column-major kernel for task-pool fallback
// ============================================================================
DATAPREP_HOT
static inline void fused_col_range(
    const std::vector<const void*>& meas_ptrs,
    const std::vector<const void*>& id_ptrs,
    const std::vector<SEXPTYPE>&    id_types,
    const std::vector<void*>&       out_id_ptrs,
    const std::vector<size_t>&      id_elem_sizes,
    int*    pvar,
    double* pval,
    int n_id, R_xlen_t n,
    int k, R_xlen_t r0, R_xlen_t r1)
{
  R_xlen_t len = r1 - r0;
  if (len <= 0) return;

  // Touch pages
  {
    touch_2m_in_range(pval + (R_xlen_t)k * n,
                      (size_t)n * sizeof(double),
                      r0, len, sizeof(double));
    touch_2m_in_range(pvar + (R_xlen_t)k * n,
                      (size_t)n * sizeof(int),
                      r0, len, sizeof(int));
    for (int i = 0; i < n_id; ++i) {
      const size_t es = id_elem_sizes[i];
      if (es == 0) continue;
      touch_2m_in_range((const char*)out_id_ptrs[i] + (R_xlen_t)k * n * es,
                        (size_t)n * es,
                        r0, len, es);
    }
  }

  // Measure + variable columns (NT stores)
  const double* src = (const double*)meas_ptrs[k] + r0;
  double* pd = pval + (R_xlen_t)k * n + r0;
  int*    pv = pvar + (R_xlen_t)k * n + r0;
  nt_fill_i32(pv, k + 1, len);
  nt_copy_pd (pd, src, len);

  // Numeric id columns first (NT stores)
  for (int i = 0; i < n_id; ++i) {
    SEXPTYPE it = id_types[i];
    if (it == STRSXP) continue;
    size_t es = id_elem_sizes[i];
    if (!es) continue;
    char* dst_i = (char*)out_id_ptrs[i] + ((R_xlen_t)k * n + r0) * es;
    const char* src_i = (const char*)id_ptrs[i] + r0 * es;
    if (it == REALSXP) {
      nt_copy_pd((double*)dst_i, (const double*)src_i, len);
    } else if (it == INTSXP || it == LGLSXP) {
      nt_copy_i32((int*)dst_i, (const int*)src_i, len);
    }
  }

  // String id columns last (normal stores, input still in cache)
  for (int i = 0; i < n_id; ++i) {
    if (id_types[i] != STRSXP) continue;
    size_t es = id_elem_sizes[i];
    if (!es) continue;
    SEXP* dst_i = (SEXP*)((char*)out_id_ptrs[i] + ((R_xlen_t)k * n + r0) * es);
    const SEXP* src_i = (const SEXP*)((const char*)id_ptrs[i] + r0 * es);
    copy_sexp(dst_i, src_i, len);
  }
}

// ============================================================================
// SPMD column-major kernel (blocked id doubling optimization)
// ============================================================================
DATAPREP_HOT
static void melt_cpp_spmd_col_major(
    const std::vector<const void*>& meas_ptrs,
    const std::vector<const void*>& id_ptrs,
    const std::vector<SEXPTYPE>&    id_types,
    const std::vector<void*>&       out_id_ptrs,
    const std::vector<size_t>&      id_elem_sizes,
    int*    pvar,
    double* pval,
    int n_id, int n_meas, R_xlen_t n,
    int n_threads)
{
  // Block size for id-column doubling (fits in L2 cache)
  constexpr R_xlen_t ID_BLOCK_ROWS = 16384;

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads) if(n_threads > 1)
#endif
{
  int tid = 0;
  int nth = 1;
#ifdef _OPENMP
  tid = omp_get_thread_num();
  nth = omp_get_num_threads();
#endif
  if (nth < 1) nth = 1;
  if (tid < 0) tid = 0;
  if (tid >= nth) tid = nth - 1;

  const R_xlen_t r0 = (R_xlen_t)tid * n / (R_xlen_t)nth;
  const R_xlen_t r1 = (R_xlen_t)(tid + 1) * n / (R_xlen_t)nth;
  const R_xlen_t len = r1 - r0;

  if (len > 0) {
    // Phase 1: parallel THP fault-in for all columns
    {
      for (int k = 0; k < n_meas; ++k) {
        touch_2m_in_range(pval + (R_xlen_t)k * n,
                          (size_t)n * sizeof(double),
                          r0, len, sizeof(double));
        touch_2m_in_range(pvar + (R_xlen_t)k * n,
                          (size_t)n * sizeof(int),
                          r0, len, sizeof(int));
      }
      for (int i = 0; i < n_id; ++i) {
        const size_t es = id_elem_sizes[i];
        if (es == 0) continue;
        for (int k = 0; k < n_meas; ++k) {
          touch_2m_in_range((const char*)out_id_ptrs[i] + (R_xlen_t)k * n * es,
                            (size_t)n * es,
                            r0, len, es);
        }
      }
    }

    // Phase 2: measure columns + variable column (all NT stores, no cache pollution)
    for (int k = 0; k < n_meas; ++k) {
      const double* src = (const double*)meas_ptrs[k] + r0;
      double* pd = pval + (R_xlen_t)k * n + r0;
      int*    pv = pvar + (R_xlen_t)k * n + r0;
      nt_fill_i32(pv, k + 1, len);
      nt_copy_pd (pd, src, len);
    }

    // Phase 3: numeric id columns (blocked doubling)
    for (int i = 0; i < n_id; ++i) {
      SEXPTYPE it = id_types[i];
      if (it == STRSXP) continue;
      const size_t es = id_elem_sizes[i];
      if (es == 0) continue;

      char* dst_base = (char*)out_id_ptrs[i];
      const char* src_i = (const char*)id_ptrs[i] + r0 * es;

      // Process in L2-sized blocks
      for (R_xlen_t b = 0; b < len; b += ID_BLOCK_ROWS) {
        R_xlen_t blen = std::min(ID_BLOCK_ROWS, len - b);
        const char* src_block = src_i + b * es;
        char* dst0_block = dst_base + (r0 + b) * es;

        // Step 1: copy input to first segment (normal store, into cache)
        if (it == REALSXP) {
          copy_pd((double*)dst0_block, (const double*)src_block, blen);
        } else if (it == INTSXP || it == LGLSXP) {
          copy_i32((int*)dst0_block, (const int*)src_block, blen);
        }

        // Step 2: replicate to remaining segments (NT stores, from cache)
        for (int k = 1; k < n_meas; ++k) {
          char* dstk_block = dst_base + ((R_xlen_t)k * n + r0 + b) * es;
          if (it == REALSXP) {
            nt_copy_pd((double*)dstk_block, (const double*)dst0_block, blen);
          } else if (it == INTSXP || it == LGLSXP) {
            nt_copy_i32((int*)dstk_block, (const int*)dst0_block, blen);
          }
        }
      }
    }

    // Phase 4: string id columns (blocked doubling, same strategy)
    for (int i = 0; i < n_id; ++i) {
      if (id_types[i] != STRSXP) continue;
      const size_t es = id_elem_sizes[i];
      if (es == 0) continue;

      char* dst_base = (char*)out_id_ptrs[i];
      const SEXP* src_i = (const SEXP*)((const char*)id_ptrs[i] + r0 * es);

      for (R_xlen_t b = 0; b < len; b += ID_BLOCK_ROWS) {
        R_xlen_t blen = std::min(ID_BLOCK_ROWS, len - b);
        const SEXP* src_block = src_i + b;
        SEXP* dst0_block = (SEXP*)(dst_base + (r0 + b) * es);

        // First copy (normal store, cached)
        copy_sexp(dst0_block, src_block, blen);

        // Replicate (NT stores)
        for (int k = 1; k < n_meas; ++k) {
          SEXP* dstk_block = (SEXP*)(dst_base + ((R_xlen_t)k * n + r0 + b) * es);
          nt_copy_pd((double*)dstk_block, (const double*)dst0_block, blen);
        }
      }
    }
  }

  nt_flush();
}
}

// ----------------------------------------------------------------------------
// Compute sub-block count for task pool
// ----------------------------------------------------------------------------
static inline int compute_num_subs(int n_meas, R_xlen_t n, int threads) {
  if (n_meas >= threads * 4) return 1;
  constexpr int64_t TARGET_ROWS = 25000;
  int64_t want        = (int64_t)threads * 32;
  int64_t needed      = (want + n_meas - 1) / n_meas;
  int64_t max_by_work = std::max<int64_t>(1, (int64_t)(n / TARGET_ROWS));
  int64_t s = std::min<int64_t>(needed, max_by_work);
  if (s > 512) s = 512;
  if (s < 1)   s = 1;
  return (int)s;
}

// ============================================================================
// Main entry point
// ============================================================================
// [[Rcpp::export]]
SEXP melt_cpp(SEXP df, SEXP id = R_NilValue,
              SEXP variable_name = R_NilValue, SEXP value_name = R_NilValue,
              SEXP major = R_NilValue, bool as_factor = true,
              int n_threads = 0, bool na_rm = false) {
  melt_lazy_init();

  g_scratch.meas_ptrs.clear();
  g_scratch.meas_types.clear();
  g_scratch.meas_is_factor.clear();
  g_scratch.id_ptrs.clear();
  g_scratch.id_types.clear();
  g_scratch.out_id_ptrs.clear();
  g_scratch.id_elem_sizes.clear();

  const size_t l3_size = g_l3_size;
  std::vector<int>& id_idx   = g_scratch.id_idx;
  std::vector<int>& meas_idx = g_scratch.meas_idx;
  resolve_id(df, id, id_idx, meas_idx);

  const int n_id   = (int)id_idx.size();
  const int n_meas = (int)meas_idx.size();
  if (DATAPREP_UNLIKELY(n_meas == 0)) stop("no measure variables");

  SEXP col_names = Rf_getAttrib(df, R_NamesSymbol);
  const R_xlen_t n = XLENGTH(VECTOR_ELT(df, 0));
  const R_xlen_t total = n * n_meas;

  // Layout dispatch: default is column-major (reshape2-compatible).
  // Only an explicit `major = "row"` triggers the row-major (tidyr-compatible)
  // path. There is no automatic switching based on input shape.
  bool row_major = false;
  if (!Rf_isNull(major) && TYPEOF(major) == STRSXP && XLENGTH(major) > 0) {
    std::string s = CHAR(STRING_ELT(major, 0));
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    row_major = (s == "row");
  }

  // Tiny fast path
  if (DATAPREP_LIKELY(!na_rm) && n <= 2048 && n_meas <= 64 && n_id <= 8) {
    return melt_tiny_cpp(df, id_idx, meas_idx, col_names,
                         variable_name, value_name, row_major, as_factor);
  }
  // Small fast path
  if (!na_rm && total <= 131072 && n_meas <= 256) {
    return melt_small_cpp(df, id_idx, meas_idx, col_names,
                          variable_name, value_name, row_major, as_factor);
  }

  std::vector<const void*>& meas_ptrs      = g_scratch.meas_ptrs;
  std::vector<SEXPTYPE>&    meas_types     = g_scratch.meas_types;
  std::vector<char>&        meas_is_factor = g_scratch.meas_is_factor;
  meas_ptrs.reserve(n_meas);
  meas_types.reserve(n_meas);
  meas_is_factor.reserve(n_meas);
  bool all_real_meas = true;
  for (int k = 0; k < n_meas; ++k) {
    SEXP col = VECTOR_ELT(df, meas_idx[k]);
    SEXPTYPE t = TYPEOF(col);
    meas_ptrs.push_back(readonly_ptr(col));
    meas_types.push_back(t);
    char fac = (char)Rf_isFactor(col);
    meas_is_factor.push_back(fac);
    if (t != REALSXP || fac) all_real_meas = false;
  }

  R_xlen_t total_out = total;
  R_xlen_t* col_off = nullptr;
  R_xlen_t* row_off = nullptr;
  if (na_rm) {
    const R_xlen_t work = (R_xlen_t)n * (R_xlen_t)n_meas;
    const bool par_count = work >= 50000LL;
    int cnt_threads = 1;
    if (par_count) {
#ifdef _OPENMP
      cnt_threads = std::min(omp_get_max_threads(), get_thread_cap());
      if (cnt_threads < 1) cnt_threads = 1;
#endif
    }
    if (!row_major) {
      col_off = (R_xlen_t*)R_alloc((size_t)(n_meas + 1), sizeof(R_xlen_t));
      col_off[0] = 0;
      if (par_count && cnt_threads > 1) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(cnt_threads)
#endif
        for (int k = 0; k < n_meas; ++k) {
          col_off[k + 1] = count_keep_col(meas_ptrs[k], meas_types[k],
                                          meas_is_factor[k], n);
        }
        // Prefix sum stays serial: only the counting loop above is parallel.
        for (int k = 1; k <= n_meas; ++k) col_off[k] += col_off[k - 1];
      } else {
        for (int k = 0; k < n_meas; ++k)
          col_off[k + 1] = col_off[k] + count_keep_col(meas_ptrs[k],
                                                       meas_types[k], meas_is_factor[k], n);
      }
      total_out = col_off[n_meas];
    } else {
      row_off = (R_xlen_t*)R_alloc((size_t)(n + 1), sizeof(R_xlen_t));
      row_off[0] = 0;
      if (par_count && cnt_threads > 1) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(cnt_threads)
#endif
        for (R_xlen_t r = 0; r < n; ++r) {
          R_xlen_t cnt = 0;
          for (int k = 0; k < n_meas; ++k) {
            if (!is_na_meas_at(meas_ptrs[k], meas_types[k],
                               meas_is_factor[k], r)) ++cnt;
          }
          row_off[r + 1] = cnt;
        }
        for (R_xlen_t r = 0; r < n; ++r) row_off[r + 1] += row_off[r];
      } else {
        for (R_xlen_t r = 0; r < n; ++r) {
          R_xlen_t cnt = 0;
          for (int k = 0; k < n_meas; ++k) {
            if (!is_na_meas_at(meas_ptrs[k], meas_types[k],
                               meas_is_factor[k], r)) ++cnt;
          }
          row_off[r + 1] = row_off[r] + cnt;
        }
      }
      total_out = row_off[n];
    }
  }

  SEXP out       = PROTECT(Rf_allocVector(VECSXP, n_id + 2));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, n_id + 2));

  std::vector<const void*>& id_ptrs       = g_scratch.id_ptrs;
  std::vector<SEXPTYPE>&    id_types      = g_scratch.id_types;
  std::vector<void*>&       out_id_ptrs   = g_scratch.out_id_ptrs;
  std::vector<size_t>&      id_elem_sizes = g_scratch.id_elem_sizes;
  for (int i = 0; i < n_id; ++i) {
    SEXP src = VECTOR_ELT(df, id_idx[i]);
    SEXPTYPE t = TYPEOF(src);
    SEXP dst = PROTECT(alloc_hint(t, total_out));
    Rf_copyMostAttrib(src, dst);
    SET_VECTOR_ELT(out, i, dst);
    SET_STRING_ELT(out_names, i, STRING_ELT(col_names, id_idx[i]));
    id_ptrs.push_back(readonly_ptr(src));
    id_types.push_back(t);
    out_id_ptrs.push_back(writable_ptr(dst));
    id_elem_sizes.push_back(sizeof_sexp(t));
    UNPROTECT(1);
  }

  SEXP var_col = PROTECT(alloc_hint(INTSXP, total_out));
  int* pvar = INTEGER(var_col);
  SET_VECTOR_ELT(out, n_id, var_col);
  SET_STRING_ELT(out_names, n_id,
                 (!Rf_isNull(variable_name) && TYPEOF(variable_name) == STRSXP) ?
                   STRING_ELT(variable_name, 0) : g_var_name("variable"));
  {
    SEXP lv = get_or_make_levels(col_names, meas_idx);
    Rf_setAttrib(var_col, R_LevelsSymbol, lv);
    Rf_setAttrib(var_col, R_ClassSymbol, g_fac_class("factor"));
  }

  SEXP val_col = PROTECT(alloc_hint(REALSXP, total_out));
  double* pval = REAL(val_col);
  SET_VECTOR_ELT(out, n_id + 1, val_col);
  SET_STRING_ELT(out_names, n_id + 1,
                 (!Rf_isNull(value_name) && TYPEOF(value_name) == STRSXP) ?
                   STRING_ELT(value_name, 0) : g_val_name("value"));

  bool use_parallel = false;
  int actual_threads = 1;
#ifdef _OPENMP
  constexpr R_xlen_t MIN_PARALLEL_WORK = 50000LL;
  constexpr R_xlen_t ELEMS_PER_THREAD  = 5000LL;
  const int thread_cap = get_thread_cap();
  if (n_threads == 1) {
    use_parallel = false;
  } else if (n_threads > 1) {
    int hw = omp_get_max_threads();
    actual_threads = std::max(1, std::min(n_threads, hw));
    use_parallel = actual_threads > 1;
  } else {
    R_xlen_t eff = (R_xlen_t)total_out + (R_xlen_t)n * (R_xlen_t)n_id;
    if (eff >= MIN_PARALLEL_WORK) {
      int hw = omp_get_max_threads();
      R_xlen_t calc = (eff + ELEMS_PER_THREAD - 1) / ELEMS_PER_THREAD;
      R_xlen_t cap_x = (R_xlen_t)thread_cap;
      actual_threads = (int)std::max<R_xlen_t>(1,
                        std::min<R_xlen_t>(cap_x, std::min<R_xlen_t>(calc, hw)));
      use_parallel = actual_threads > 1;
    }
  }
#else
  (void)n_threads; (void)l3_size;
#endif

  // ---- Column-major path ----
  if (!row_major) {
    if (!na_rm) {
      const bool fused_ok = all_real_meas;

      if (fused_ok) {
        const R_xlen_t rows_per_thread =
          (actual_threads > 0) ? (n / (R_xlen_t)actual_threads) : n;
        const bool spmd_ok =
          use_parallel &&
          (actual_threads > 1) &&
          (rows_per_thread >= 4096);

        if (spmd_ok) {
          melt_cpp_spmd_col_major(meas_ptrs, id_ptrs, id_types,
                                  out_id_ptrs, id_elem_sizes,
                                  pvar, pval, n_id, n_meas, n,
                                  actual_threads);
        } else {
          const int num_subs = use_parallel
          ? compute_num_subs(n_meas, n, actual_threads)
            : 1;
          const int total_tasks = n_meas * num_subs;

          if (use_parallel && total_tasks > 1) {
#ifdef _OPENMP
#pragma omp parallel num_threads(actual_threads)
{
#pragma omp for schedule(static) nowait
  for (int t = 0; t < total_tasks; ++t) {
    int k = t / num_subs;
    int s = t % num_subs;
    R_xlen_t r0 = (R_xlen_t)s * n / (R_xlen_t)num_subs;
    R_xlen_t r1 = (R_xlen_t)(s + 1) * n / (R_xlen_t)num_subs;
    fused_col_range(meas_ptrs, id_ptrs, id_types,
                    out_id_ptrs, id_elem_sizes,
                    pvar, pval, n_id, n, k, r0, r1);
  }
  nt_flush();
}
#endif
          } else {
            for (int k = 0; k < n_meas; ++k) {
              fused_col_range(meas_ptrs, id_ptrs, id_types,
                              out_id_ptrs, id_elem_sizes,
                              pvar, pval, n_id, n, k, 0, n);
            }
            nt_flush();
          }
        }
      } else {
        // Non-fused path: measure columns first
        const int num_subs = use_parallel
        ? compute_num_subs(n_meas, n, actual_threads)
          : 1;
        const int total_tasks = n_meas * num_subs;

        auto process_range = [&](int k, R_xlen_t r0, R_xlen_t r1) {
          R_xlen_t len = r1 - r0;
          if (len <= 0) return;
          const void* mp = meas_ptrs[k];
          SEXPTYPE t = meas_types[k];
          bool fac = meas_is_factor[k];
          nt_fill_i32(pvar + (R_xlen_t)k * n + r0, k + 1, len);
          double* pd = pval + (R_xlen_t)k * n + r0;
          if (t == REALSXP) {
            nt_copy_pd(pd, (const double*)mp + r0, len);
          } else if (t == INTSXP && !fac) {
            int2double_ptr((const int*)mp + r0, pd, (size_t)len, false);
          } else if (t == LGLSXP) {
            int2double_ptr((const int*)mp + r0, pd, (size_t)len, true);
          } else {
            std::fill(pd, pd + len, NA_REAL);
          }
        };

        if (use_parallel && total_tasks > 1) {
#ifdef _OPENMP
#pragma omp parallel num_threads(actual_threads)
{
#pragma omp for schedule(static) nowait
  for (int t = 0; t < total_tasks; ++t) {
    int k = t / num_subs;
    int s = t % num_subs;
    R_xlen_t r0 = (R_xlen_t)s * n / (R_xlen_t)num_subs;
    R_xlen_t r1 = (R_xlen_t)(s + 1) * n / (R_xlen_t)num_subs;
    process_range(k, r0, r1);
  }
  nt_flush();
}
#endif
        } else {
          for (int k = 0; k < n_meas; ++k) process_range(k, 0, n);
          nt_flush();
        }

        // Id columns: blocked doubling
        for (int pass = 0; pass < 2; ++pass) {
          bool do_str = (pass == 1);
          for (int i = 0; i < n_id; ++i) {
            bool is_str = (id_types[i] == STRSXP);
            if (is_str != do_str) continue;
            SEXPTYPE it = id_types[i];
            size_t es = id_elem_sizes[i];
            if (!es) continue;
            const char* s = (const char*)id_ptrs[i];
            char* d_base = (char*)out_id_ptrs[i];

            // First copy (normal store)
            if (is_str) {
              copy_sexp((SEXP*)d_base, (const SEXP*)s, n);
            } else if (it == REALSXP) {
              copy_pd((double*)d_base, (const double*)s, n);
            } else if (it == INTSXP || it == LGLSXP) {
              copy_i32((int*)d_base, (const int*)s, n);
            }

            // Doubling (NT stores)
            size_t filled = 1;
            while (filled < (size_t)n_meas) {
              size_t tc = filled;
              if (tc > (size_t)n_meas - filled) tc = (size_t)n_meas - filled;
              char* dst_out = d_base + filled * (size_t)n * es;

              if (is_str) {
                for (size_t b = 0; b < tc; ++b) {
                  nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                             (const double*)d_base, n);
                }
              } else if (it == REALSXP) {
                for (size_t b = 0; b < tc; ++b) {
                  nt_copy_pd((double*)(dst_out + b * (size_t)n * es),
                             (const double*)d_base, n);
                }
              } else if (it == INTSXP || it == LGLSXP) {
                for (size_t b = 0; b < tc; ++b) {
                  nt_copy_i32((int*)(dst_out + b * (size_t)n * es),
                              (const int*)d_base, n);
                }
              }
              filled += tc;
            }
          }
        }
      }
    } else {
      // NA removal path (column-major)
      std::vector<R_xlen_t>& tl_keep = g_scratch.keep_idx;
      auto process_col_na_rm = [&](int k) {
        R_xlen_t off = col_off[k];
        R_xlen_t cnt = col_off[k + 1] - off;
        if (!cnt) return;
        SEXPTYPE t = meas_types[k];
        bool fac = meas_is_factor[k];
        const void* mp = meas_ptrs[k];
        int* pv = pvar + off;
        double* pd = pval + off;
        tl_keep.resize((size_t)cnt);
        R_xlen_t* kidx = tl_keep.data();
        R_xlen_t j = 0;
        for (R_xlen_t i = 0; i < n; ++i) {
          if (is_na_meas_at(mp, t, fac, i)) continue;
          kidx[j] = i;
          pd[j] = meas_at_as_double(mp, t, fac, i);
          pv[j] = k + 1;
          ++j;
        }
        const R_xlen_t m = j;
        for (int ii = 0; ii < n_id; ++ii) {
          SEXPTYPE it = id_types[ii];
          if (it == INTSXP || it == LGLSXP) {
            const int* s = (const int*)id_ptrs[ii];
            int* d = (int*)out_id_ptrs[ii] + off;
            for (R_xlen_t jj = 0; jj < m; ++jj) d[jj] = s[kidx[jj]];
          } else if (it == REALSXP) {
            const double* s = (const double*)id_ptrs[ii];
            double* d = (double*)out_id_ptrs[ii] + off;
            for (R_xlen_t jj = 0; jj < m; ++jj) d[jj] = s[kidx[jj]];
          } else {
            SEXP dst_col = VECTOR_ELT(out, ii);
            SEXP src_col = VECTOR_ELT(df, id_idx[ii]);
            for (R_xlen_t jj = 0; jj < m; ++jj)
              SET_STRING_ELT(dst_col, off + jj, STRING_ELT(src_col, kidx[jj]));
          }
        }
      };
      const int team = std::min(actual_threads, n_meas);
      if (use_parallel && team > 1) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(team)
#endif
        for (int k = 0; k < n_meas; ++k) process_col_na_rm(k);
      } else {
        for (int k = 0; k < n_meas; ++k) process_col_na_rm(k);
      }
    }
  }
  // ---- Row-major path ----
  else {
    size_t bytes_per_row = (size_t)n_meas * (sizeof(double) + sizeof(int));
    for (size_t es : id_elem_sizes) bytes_per_row += es;
    if (!bytes_per_row) bytes_per_row = 1;
    R_xlen_t calc_block = (R_xlen_t)((l3_size / 2) / bytes_per_row);
    calc_block = std::max<R_xlen_t>(256, std::min<R_xlen_t>(65536, calc_block));
    R_xlen_t BLOCK_SIZE = calc_block;
    if (use_parallel) {
      R_xlen_t by_t = n / (R_xlen_t)(4 * actual_threads);
      BLOCK_SIZE = std::max<R_xlen_t>(calc_block / 4, by_t);
      BLOCK_SIZE = std::min<R_xlen_t>(BLOCK_SIZE, calc_block);
    }

    std::vector<int>& tl_var_pat8 = g_scratch.var_pat8;
    if ((int)tl_var_pat8.size() < 8 * n_meas) tl_var_pat8.resize((size_t)8 * n_meas);
    for (int p = 0; p < 8; ++p)
      for (int k = 0; k < n_meas; ++k)
        tl_var_pat8[(size_t)p * n_meas + k] = k + 1;
    const int* var_pat8 = tl_var_pat8.data();

    if (!na_rm) {
      auto process_block = [&](R_xlen_t ib, R_xlen_t iend) {
        for (R_xlen_t r = ib; r < iend; ++r) {
          double* vr = pval + r * n_meas;
          for (int k = 0; k < n_meas; ++k) {
            SEXPTYPE t = meas_types[k];
            const void* mp = meas_ptrs[k];
            double v = NA_REAL;
            if (t == REALSXP) v = ((const double*)mp)[r];
            else if (t == INTSXP && !meas_is_factor[k]) {
              int x = ((const int*)mp)[r];
              v = (x == NA_INTEGER) ? NA_REAL : (double)x;
            } else if (t == LGLSXP) {
              int x = ((const int*)mp)[r];
              v = (x == NA_LOGICAL) ? NA_REAL : (x ? 1.0 : 0.0);
            }
            vr[k] = v;
          }
          std::memcpy(pvar + (size_t)r * n_meas, var_pat8,
                      (size_t)n_meas * sizeof(int));
        }
        // Id columns
        for (int ii = 0; ii < n_id; ++ii) {
          SEXPTYPE it = id_types[ii];
          if (it == STRSXP) {
            SEXP dst_col = VECTOR_ELT(out, ii);
            SEXP src_col = VECTOR_ELT(df, id_idx[ii]);
            for (R_xlen_t r = ib; r < iend; ++r) {
              SEXP v = STRING_ELT(src_col, r);
              R_xlen_t base = r * n_meas;
              for (int k = 0; k < n_meas; ++k) SET_STRING_ELT(dst_col, base + k, v);
            }
          } else if (it == REALSXP) {
            const double* s = (const double*)id_ptrs[ii];
            double* d = (double*)out_id_ptrs[ii];
            for (R_xlen_t r = ib; r < iend; ++r) {
              double v = s[r];
              double* p = d + r * n_meas;
              for (int k = 0; k < n_meas; ++k) p[k] = v;
            }
          } else {
            const int* s = (const int*)id_ptrs[ii];
            int* d = (int*)out_id_ptrs[ii];
            for (R_xlen_t r = ib; r < iend; ++r) {
              int v = s[r];
              int* p = d + r * n_meas;
              for (int k = 0; k < n_meas; ++k) p[k] = v;
            }
          }
        }
      };
      if (use_parallel) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(actual_threads)
#endif
        for (R_xlen_t i = 0; i < n; i += BLOCK_SIZE) {
          R_xlen_t iend = std::min(i + BLOCK_SIZE, n);
          process_block(i, iend);
        }
      } else {
        for (R_xlen_t i = 0; i < n; i += BLOCK_SIZE) {
          R_xlen_t iend = std::min(i + BLOCK_SIZE, n);
          process_block(i, iend);
        }
      }
    } else {
      auto process_block = [&](R_xlen_t ib, R_xlen_t iend) {
        for (R_xlen_t r = ib; r < iend; ++r) {
          R_xlen_t o = row_off[r];
          R_xlen_t cnt = row_off[r + 1] - o;
          if (!cnt) continue;
          R_xlen_t j = o;
          for (int k = 0; k < n_meas; ++k) {
            if (is_na_meas_at(meas_ptrs[k], meas_types[k],
                              meas_is_factor[k], r)) continue;
            pval[j] = meas_at_as_double(meas_ptrs[k], meas_types[k],
                                        meas_is_factor[k], r);
            pvar[j] = k + 1;
            ++j;
          }
          for (int ii = 0; ii < n_id; ++ii) {
            SEXPTYPE it = id_types[ii];
            if (it == INTSXP || it == LGLSXP) {
              fill_int_exact((int*)out_id_ptrs[ii] + o,
                             ((const int*)id_ptrs[ii])[r], (int)cnt);
            } else if (it == REALSXP) {
              fill_double_exact((double*)out_id_ptrs[ii] + o,
                                ((const double*)id_ptrs[ii])[r], (int)cnt);
            } else {
              SEXP v = STRING_ELT(VECTOR_ELT(df, id_idx[ii]), r);
              SEXP dst_col = VECTOR_ELT(out, ii);
              for (R_xlen_t jj = 0; jj < cnt; ++jj)
                SET_STRING_ELT(dst_col, o + jj, v);
            }
          }
        }
      };
      if (use_parallel) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(actual_threads)
#endif
        for (R_xlen_t i = 0; i < n; i += BLOCK_SIZE) {
          R_xlen_t iend = std::min(i + BLOCK_SIZE, n);
          process_block(i, iend);
        }
      } else {
        for (R_xlen_t i = 0; i < n; i += BLOCK_SIZE) {
          R_xlen_t iend = std::min(i + BLOCK_SIZE, n);
          process_block(i, iend);
        }
      }
    }
  }

  nt_flush();

  if (DATAPREP_UNLIKELY(!as_factor)) {
    maybe_make_var_character(out, n_id, total_out);
  }

  Rf_setAttrib(out, R_NamesSymbol, out_names);
  {
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = (total_out <= (R_xlen_t)INT_MAX) ? -(int)total_out : NA_INTEGER;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    Rf_setAttrib(out, R_ClassSymbol, g_df_class("data.frame"));
    UNPROTECT(1);
  }
  UNPROTECT(4);
  return out;
}
