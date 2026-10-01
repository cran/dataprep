// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]
//
// dcast_cpp — CRAN-compliant high-performance wide cast.
//
// Dispatch:
//   8 <= period <= 32  AND  g_have_avx512 : 8x8 AVX-512 SIMD, 32-row outer tile
//   otherwise                             : TR=128 tile transpose
//
// THP strategy (validated on 12-channel DDR5 single-socket):
//   * Output : MADV_HUGEPAGE + MADV_POPULATE_WRITE (or touch_2m fallback)
//              forces the kernel to commit 2 MB pages at allocation time.
//   * Input  : MADV_HUGEPAGE + MADV_COLLAPSE promotes existing 4 KB pages.
//
// GCC 15 constraint:
//   No #pragma omp inside any target-attributed function. All SIMD kernels
//   are noinline + target; OpenMP regions live in plain driver functions.
//
#include <Rcpp.h>
#include <cstring>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <vector>
#include <algorithm>
#include <numeric>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <limits>

#if defined(__x86_64__) || defined(_M_X64) || \
    defined(__i386__)   || defined(_M_IX86)
#  define DCAST_X86 1
#  include <immintrin.h>
#else
#  define DCAST_X86 0
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define DCAST_GNUC 1
#  define DCAST_TARGET(x)      __attribute__((target(x)))
#  define DCAST_NOINLINE       __attribute__((noinline))
#  define DCAST_ALWAYS_INLINE  __attribute__((always_inline))
#  define DCAST_LIKELY(x)      __builtin_expect(!!(x), 1)
#  define DCAST_UNLIKELY(x)    __builtin_expect(!!(x), 0)
#else
#  define DCAST_GNUC 0
#  define DCAST_TARGET(x)
#  define DCAST_NOINLINE
#  define DCAST_ALWAYS_INLINE
#  define DCAST_LIKELY(x)      (x)
#  define DCAST_UNLIKELY(x)    (x)
#endif

#if defined(__linux__)
#  include <sys/mman.h>
#  include <unistd.h>
#  ifndef MADV_HUGEPAGE
#    define DCAST_NO_THP 1
#  else
#    define DCAST_NO_THP 0
#  endif
#else
#  define DCAST_NO_THP 1
#endif

#ifdef _OPENMP
#  include <omp.h>
#endif

using namespace Rcpp;

#if defined(R_VERSION) && R_VERSION >= R_Version(4, 5, 0)
#  define DCAST_STRING_PTR_RO(x) STRING_PTR_RO(x)
#else
#  define DCAST_STRING_PTR_RO(x) STRING_PTR(x)
#endif

// =====================================================================
// [1] Memory hints
// =====================================================================
static inline size_t sizeof_sexpvec(SEXPTYPE t) {
  switch (t) {
  case INTSXP:  return sizeof(int);
  case REALSXP: return sizeof(double);
  case LGLSXP:  return sizeof(int);
  case STRSXP:  return sizeof(SEXP);
  case VECSXP:  return sizeof(SEXP);
  default:      return 0;
  }
}

// Touch every 2 MB boundary. Forces the kernel to commit 2 MB pages
// immediately instead of falling back to 4 KB. On single-socket machines
// (like the 12-channel DDR5 target) this has no NUMA downside.
static inline void touch_2m_fast(void* p, size_t bytes) {
#if !DCAST_NO_THP
  if (bytes < (2ULL << 20)) return;
  volatile char* c = (volatile char*)p;
  const size_t step = 2ULL << 20;
  for (size_t i = 0; i < bytes; i += step) c[i] = 0;
  c[bytes - 1] = 0;
#else
  (void)p; (void)bytes;
#endif
}

// Output hint: ask for THP, optionally interleave (multi-socket), then
// materialise the page table entries via POPULATE_WRITE (fast syscall)
// or touch_2m fallback.
static inline void hint_alloc(void* p, size_t bytes) {
#if !DCAST_NO_THP
  if (!p || bytes < (512ULL << 10)) return;
  madvise(p, bytes, MADV_HUGEPAGE);
#  ifdef MADV_INTERLEAVE
  if (bytes >= (32ULL << 20)) madvise(p, bytes, MADV_INTERLEAVE);
#  endif
#  if defined(MADV_POPULATE_WRITE)
  if (bytes >= (16ULL << 20) &&
      madvise(p, bytes, MADV_POPULATE_WRITE) == 0) {
    return;
  }
#  endif
  if (bytes >= (32ULL << 20)) touch_2m_fast(p, bytes);
#else
  (void)p; (void)bytes;
#endif
}

// Input hint: R already faulted these columns with 4 KB pages. MADV_COLLAPSE
// (Linux 6.1+) promotes them into 2 MB. This gave 10-12% on 100-lvl shapes.
static inline void hint_readonly(SEXP col) {
#if !DCAST_NO_THP
  if (!col || col == R_NilValue) return;
  SEXPTYPE t = TYPEOF(col);
  size_t es = sizeof_sexpvec(t);
  if (es == 0) {
    if (t != STRSXP) return;
    es = sizeof(SEXP);
  }
  R_xlen_t n = XLENGTH(col);
  if (n <= 0) return;
  size_t bytes = (size_t)n * es;
  if (bytes < (2ULL << 20)) return;
  void* p = (void*)DATAPTR_RO(col);
  if (!p) return;
  madvise(p, bytes, MADV_HUGEPAGE);
#  ifdef MADV_COLLAPSE
  madvise(p, bytes, MADV_COLLAPSE);
#  endif
#else
  (void)col;
#endif
}

// API-compliant writable pointer. DATAPTR is non-API since R 4.5;
// we dispatch on SEXPTYPE instead. For STRSXP / VECSXP we return
// nullptr: the element data lives in the CHARSXP pool or in
// separately-allocated SEXPs, so madvise on the pointer vector has
// little effect anyway.
static inline void* writable_ptr_sexp(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:  return (void*)INTEGER(x);
  case LGLSXP:  return (void*)LOGICAL(x);
  case REALSXP: return (void*)REAL(x);
  default:      return nullptr;
  }
}

static inline SEXP alloc_smart(SEXPTYPE type, R_xlen_t n) {
  SEXP v = Rf_allocVector(type, n);
  size_t es = sizeof_sexpvec(type);
  if (es != 0 && n > 0) {
    void* p = writable_ptr_sexp(v);
    if (p) hint_alloc(p, (size_t)n * es);
  }
  return v;
}

static inline SEXP cached_df_class() {
  static SEXP s = R_NilValue;
  if (s == R_NilValue) { s = Rf_mkString("data.frame"); R_PreserveObject(s); }
  return s;
}

// =====================================================================
// [2] CPU feature detection
// =====================================================================
static void fill_scalar(double* d, double v, size_t n) {
  for (size_t i = 0; i < n; ++i) d[i] = v;
}

#if DCAST_GNUC && DCAST_X86
DCAST_TARGET("avx2") DCAST_NOINLINE
static void fill_avx2(double* __restrict__ d, double v, size_t n) {
  size_t i = 0;
  __m256d vv = _mm256_set1_pd(v);
  for (; i + 16 <= n; i += 16) {
    _mm256_storeu_pd(d + i,      vv);
    _mm256_storeu_pd(d + i + 4,  vv);
    _mm256_storeu_pd(d + i + 8,  vv);
    _mm256_storeu_pd(d + i + 12, vv);
  }
  for (; i + 4 <= n; i += 4) _mm256_storeu_pd(d + i, vv);
  for (; i < n; ++i) d[i] = v;
}

DCAST_TARGET("avx512f,avx512vl") DCAST_NOINLINE
static void fill_avx512(double* __restrict__ d, double v, size_t n) {
  size_t i = 0;
  __m512d vv = _mm512_set1_pd(v);
  for (; i + 32 <= n; i += 32) {
    _mm512_storeu_pd(d + i,      vv);
    _mm512_storeu_pd(d + i + 8,  vv);
    _mm512_storeu_pd(d + i + 16, vv);
    _mm512_storeu_pd(d + i + 24, vv);
  }
  for (; i + 8 <= n; i += 8) _mm512_storeu_pd(d + i, vv);
  for (; i < n; ++i) d[i] = v;
}
#endif

static void (*fill_double_ptr)(double*, double, size_t) = fill_scalar;
static bool g_have_avx512 = false;

static void init_cpu_features() {
  static bool done = false;
  if (done) return;
  done = true;
#if DCAST_GNUC && DCAST_X86
  if (__builtin_cpu_supports("avx512f") &&
      __builtin_cpu_supports("avx512vl") &&
      __builtin_cpu_supports("avx512bw") &&
      __builtin_cpu_supports("avx512dq")) {
    fill_double_ptr = fill_avx512;
    g_have_avx512 = true;
  } else if (__builtin_cpu_supports("avx2")) {
    fill_double_ptr = fill_avx2;
  }
#endif
}

// =====================================================================
// [3] AVX-512 kernels — target-attributed, noinline, NO OpenMP inside.
// =====================================================================
#if DCAST_GNUC && DCAST_X86

DCAST_TARGET("avx512f,avx512vl") DCAST_ALWAYS_INLINE
static inline void transpose8x8_avx512(
    __m512d r0, __m512d r1, __m512d r2, __m512d r3,
    __m512d r4, __m512d r5, __m512d r6, __m512d r7,
    __m512d& c0, __m512d& c1, __m512d& c2, __m512d& c3,
    __m512d& c4, __m512d& c5, __m512d& c6, __m512d& c7)
{
  __m512d t0 = _mm512_unpacklo_pd(r0, r1);
  __m512d t1 = _mm512_unpackhi_pd(r0, r1);
  __m512d t2 = _mm512_unpacklo_pd(r2, r3);
  __m512d t3 = _mm512_unpackhi_pd(r2, r3);
  __m512d t4 = _mm512_unpacklo_pd(r4, r5);
  __m512d t5 = _mm512_unpackhi_pd(r4, r5);
  __m512d t6 = _mm512_unpacklo_pd(r6, r7);
  __m512d t7 = _mm512_unpackhi_pd(r6, r7);
  __m512d s0 = _mm512_shuffle_f64x2(t0, t2, 0x88);
  __m512d s1 = _mm512_shuffle_f64x2(t0, t2, 0xDD);
  __m512d s2 = _mm512_shuffle_f64x2(t1, t3, 0x88);
  __m512d s3 = _mm512_shuffle_f64x2(t1, t3, 0xDD);
  __m512d s4 = _mm512_shuffle_f64x2(t4, t6, 0x88);
  __m512d s5 = _mm512_shuffle_f64x2(t4, t6, 0xDD);
  __m512d s6 = _mm512_shuffle_f64x2(t5, t7, 0x88);
  __m512d s7 = _mm512_shuffle_f64x2(t5, t7, 0xDD);
  c0 = _mm512_shuffle_f64x2(s0, s4, 0x88);
  c1 = _mm512_shuffle_f64x2(s0, s4, 0xDD);
  c2 = _mm512_shuffle_f64x2(s1, s5, 0x88);
  c3 = _mm512_shuffle_f64x2(s1, s5, 0xDD);
  c4 = _mm512_shuffle_f64x2(s2, s6, 0x88);
  c5 = _mm512_shuffle_f64x2(s2, s6, 0xDD);
  c6 = _mm512_shuffle_f64x2(s3, s7, 0x88);
  c7 = _mm512_shuffle_f64x2(s3, s7, 0xDD);
}

// 32-row × kk-column tile transpose. 32 rows are read in one pass; 4 SIMD
// 8×8 blocks share the same tile buffer. period ∈ [8, 32] guaranteed by
// the caller, so the tile is at most 32*32*8 = 8 KB (fits L1).
DCAST_TARGET("avx512f,avx512vl") DCAST_NOINLINE
static void transpose_32row_tile_avx512(
    const int32_t* __restrict__ first_src,
    const double*  __restrict__ vs,
    double* const* __restrict__ out_val,
    R_xlen_t bs, int kk, int period, int nr_rows,
    bool na_rm, double fill_val)
{
  alignas(64) double tile[32 * 32];

  for (int i = 0; i < nr_rows; ++i) {
    R_xlen_t io = (R_xlen_t)first_src[(size_t)(bs + i)];
    double* row = tile + (size_t)i * period;
    std::memcpy(row, vs + io, (size_t)period * sizeof(double));
    if (na_rm) {
      for (int k = 0; k < period; ++k)
        if (ISNAN(row[k])) row[k] = fill_val;
    }
  }

  const int kk8 = (kk / 8) * 8;
  const int nr8 = (nr_rows / 8) * 8;

  for (int r = 0; r < nr8; r += 8) {
    for (int k = 0; k < kk8; k += 8) {
      __m512d r0 = _mm512_loadu_pd(tile + (size_t)(r + 0) * period + k);
      __m512d r1 = _mm512_loadu_pd(tile + (size_t)(r + 1) * period + k);
      __m512d r2 = _mm512_loadu_pd(tile + (size_t)(r + 2) * period + k);
      __m512d r3 = _mm512_loadu_pd(tile + (size_t)(r + 3) * period + k);
      __m512d r4 = _mm512_loadu_pd(tile + (size_t)(r + 4) * period + k);
      __m512d r5 = _mm512_loadu_pd(tile + (size_t)(r + 5) * period + k);
      __m512d r6 = _mm512_loadu_pd(tile + (size_t)(r + 6) * period + k);
      __m512d r7 = _mm512_loadu_pd(tile + (size_t)(r + 7) * period + k);
      __m512d c0, c1, c2, c3, c4, c5, c6, c7;
      transpose8x8_avx512(r0, r1, r2, r3, r4, r5, r6, r7,
                          c0, c1, c2, c3, c4, c5, c6, c7);
      _mm512_storeu_pd(out_val[(size_t)k + 0] + bs + r, c0);
      _mm512_storeu_pd(out_val[(size_t)k + 1] + bs + r, c1);
      _mm512_storeu_pd(out_val[(size_t)k + 2] + bs + r, c2);
      _mm512_storeu_pd(out_val[(size_t)k + 3] + bs + r, c3);
      _mm512_storeu_pd(out_val[(size_t)k + 4] + bs + r, c4);
      _mm512_storeu_pd(out_val[(size_t)k + 5] + bs + r, c5);
      _mm512_storeu_pd(out_val[(size_t)k + 6] + bs + r, c6);
      _mm512_storeu_pd(out_val[(size_t)k + 7] + bs + r, c7);
    }
    for (int k = kk8; k < kk; ++k) {
      for (int t = 0; t < 8; ++t)
        out_val[(size_t)k][bs + r + t] = tile[(size_t)(r + t) * period + k];
    }
  }
  for (int i = nr8; i < nr_rows; ++i) {
    const double* row = tile + (size_t)i * period;
    for (int k = 0; k < kk; ++k) out_val[(size_t)k][bs + i] = row[k];
  }
}

#endif // DCAST_GNUC && DCAST_X86

// =====================================================================
// [4] OpenMP drivers
// =====================================================================
static void transpose_8x8_rows(
    const int32_t* __restrict__ first_src,
    const double*  __restrict__ vs,
    double* const* __restrict__ out_val,
    R_xlen_t n_blocks, int kk, int period, int n_threads,
    bool na_rm, double fill_val)
{
#if DCAST_GNUC && DCAST_X86
  if (n_threads < 1) n_threads = 1;

  constexpr int NR = 32;
  const R_xlen_t n_tiles = (n_blocks + NR - 1) / NR;

  if (n_tiles > 0) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads) \
    if(n_threads > 1 && n_tiles >= 2)
#endif
    for (R_xlen_t t = 0; t < n_tiles; ++t) {
      const R_xlen_t bs = (R_xlen_t)t * NR;
      const int tr = (int)std::min<R_xlen_t>((R_xlen_t)NR, n_blocks - bs);
      transpose_32row_tile_avx512(first_src, vs, out_val, bs, kk, period, tr,
                                   na_rm, fill_val);
    }
  }
#else
  (void)first_src; (void)vs; (void)out_val; (void)n_blocks;
  (void)kk; (void)period; (void)n_threads;
  (void)na_rm; (void)fill_val;
#endif
}

static void transpose_tile(
    const int32_t* __restrict__ first_src,
    const void*    __restrict__ vs_raw,
    double* const* __restrict__ out_val,
    R_xlen_t n_blocks, int kk, int period, int n_threads, int kind,
    bool na_rm, double fill_val)
{
  if (n_threads < 1) n_threads = 1;
  constexpr int TR = 128;
  const int pad = kk;
  const bool use_stack = (pad * TR <= 16384);

  const double* vs_d = (kind == 0) ? (const double*)vs_raw : nullptr;
  const int*    vs_i = (kind != 0) ? (const int*)vs_raw    : nullptr;

  bool fs_is_arith = false;
  int32_t fs_step = 0;
  if (n_blocks >= 2 && kind == 0) {
    fs_step = first_src[1] - first_src[0];
    fs_is_arith = (fs_step == (int32_t)period);
    if (fs_is_arith) {
      const R_xlen_t m = std::min<R_xlen_t>(n_blocks, (R_xlen_t)64);
      for (R_xlen_t i = 2; i < m; ++i)
        if (first_src[(size_t)i] - first_src[(size_t)(i - 1)] != fs_step) {
          fs_is_arith = false; break;
        }
    }
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads) if(n_threads > 1)
#endif
{
  std::vector<double> heap_tile;
  if (!use_stack) heap_tile.resize((size_t)TR * (size_t)pad);
  alignas(64) double stack_tile[128 * 128];
  double* tile = use_stack ? stack_tile : heap_tile.data();

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
  for (R_xlen_t bs = 0; bs < n_blocks; bs += TR) {
    const int tr = (int)std::min<R_xlen_t>((R_xlen_t)TR, n_blocks - bs);

    if (kind == 0 && fs_is_arith) {
      std::memcpy(tile, vs_d + (R_xlen_t)first_src[(size_t)bs],
                  (size_t)tr * (size_t)period * sizeof(double));
      if (na_rm) {
        const size_t total = (size_t)tr * (size_t)period;
        for (size_t x = 0; x < total; ++x)
          if (ISNAN(tile[x])) tile[x] = fill_val;
      }
    } else if (kind == 0) {
      for (int t = 0; t < tr; ++t) {
        R_xlen_t io = (R_xlen_t)first_src[(size_t)(bs + t)];
        const double* src = vs_d + io;
        __builtin_prefetch((const void*)src, 0, 3);
        double* row = tile + (size_t)t * pad;
        std::memcpy(row, src, (size_t)kk * sizeof(double));
        if (na_rm) {
          for (int k = 0; k < kk; ++k)
            if (ISNAN(row[k])) row[k] = fill_val;
        }
      }
    } else if (kind == 1) {
      for (int t = 0; t < tr; ++t) {
        R_xlen_t io = (R_xlen_t)first_src[(size_t)(bs + t)];
        const int* src = vs_i + io;
        __builtin_prefetch((const void*)src, 0, 3);
        double* row = tile + (size_t)t * pad;
        for (int k = 0; k < kk; ++k) {
          int v = src[k];
          row[k] = (v == NA_INTEGER)
                     ? (na_rm ? fill_val : NA_REAL)
                     : (double)v;
        }
      }
    } else {
      for (int t = 0; t < tr; ++t) {
        R_xlen_t io = (R_xlen_t)first_src[(size_t)(bs + t)];
        const int* src = vs_i + io;
        __builtin_prefetch((const void*)src, 0, 3);
        double* row = tile + (size_t)t * pad;
        for (int k = 0; k < kk; ++k) {
          int v = src[k];
          row[k] = (v == NA_LOGICAL)
                     ? (na_rm ? fill_val : NA_REAL)
                     : (v ? 1.0 : 0.0);
        }
      }
    }

    for (int k = 0; k < kk; ++k) {
      double* dst = out_val[(size_t)k] + bs;
      const double* src = tile + k;
      for (int t = 0; t < tr; ++t) dst[t] = src[(size_t)t * pad];
    }
  }
}
}

// =====================================================================
// [5] Utilities
// =====================================================================
static inline int fast_itoa(int v, char* out) {
  if (v == NA_INTEGER) { out[0] = 'N'; out[1] = 'A'; return 2; }
  if (v == 0) { out[0] = '0'; return 1; }
  bool neg = (v < 0);
  unsigned u = neg ? (unsigned)(-(int64_t)v) : (unsigned)v;
  char tmp[12]; int k = 0;
  while (u) { tmp[k++] = char('0' + (u % 10)); u /= 10; }
  int pos = 0;
  if (neg) out[pos++] = '-';
  for (int i = k - 1; i >= 0; --i) out[pos++] = tmp[i];
  return pos;
}

static inline uint64_t mix64(uint64_t x) {
  x ^= x >> 30; x *= 0xBF58476D1CE4E5B9ULL;
  x ^= x >> 27; x *= 0x94D049BB133111EBULL;
  x ^= x >> 31;
  return x;
}
static inline uint64_t fmix64(uint64_t x) {
  x ^= x >> 33; x *= 0xff51afd7ed558ccdULL;
  x ^= x >> 33; x *= 0xc4ceb9fe1a85ec53ULL;
  x ^= x >> 33;
  return x;
}

struct ColDesc {
  const int32_t* raw  = nullptr;
  const int32_t* code = nullptr;
  int32_t  mn = 0;
  uint32_t na_code = 0;
  uint32_t units   = 1;
  bool     has_na  = false;
};

static inline uint32_t code_of(const ColDesc& d, R_xlen_t i) {
  if (DCAST_LIKELY(d.code != nullptr)) return (uint32_t)d.code[i];
  int32_t v = d.raw[i];
  if (DCAST_UNLIKELY(v == NA_INTEGER)) return d.na_code;
  return (uint32_t)((int64_t)v - (int64_t)d.mn);
}

template <int N>
struct KeyFnN {
  const ColDesc* c;
  const int* sh;
  inline uint64_t operator()(R_xlen_t i) const {
    uint64_t k = 0;
#if DCAST_GNUC
#  pragma GCC unroll 8
#endif
    for (int j = 0; j < N; ++j)
      k |= (uint64_t)code_of(c[j], i) << sh[j];
    return k;
  }
};

struct KeyFnDyn {
  const ColDesc* c;
  const int* sh;
  int n;
  inline uint64_t operator()(R_xlen_t i) const {
    uint64_t k = 0;
    for (int j = 0; j < n; ++j)
      k |= (uint64_t)code_of(c[j], i) << sh[j];
    return k;
  }
};

static inline void hash_codes_96(const int32_t* __restrict__ codes, int n,
                                 uint64_t& h1_out, uint32_t& h2_out) {
  constexpr uint64_t P0 = 0x100000001b3ULL;
  constexpr uint64_t P1 = 0x9E3779B97F4A7C15ULL;
  constexpr uint64_t P2 = 0xBF58476D1CE4E5B9ULL;
  constexpr uint64_t P3 = 0x94D049BB133111EBULL;
  uint64_t a0 = 0xcbf29ce484222325ULL, a1 = 0x84222325cbf29ce4ULL;
  uint64_t a2 = 0x9E3779B97F4A7C15ULL, a3 = 0xBF58476D1CE4E5B9ULL;
  int j = 0;
  for (; j + 4 <= n; j += 4) {
    a0 = (a0 ^ (uint64_t)codes[j + 0]) * P0;
    a1 = (a1 ^ (uint64_t)codes[j + 1]) * P1;
    a2 = (a2 ^ (uint64_t)codes[j + 2]) * P2;
    a3 = (a3 ^ (uint64_t)codes[j + 3]) * P3;
  }
  for (; j < n; ++j) a0 = (a0 ^ (uint64_t)codes[j]) * P0;
  uint64_t x = fmix64(a0) ^ fmix64(a1) ^ fmix64(a2) ^ fmix64(a3);
  uint64_t y = fmix64(a0 + P1) + fmix64(a1 + P2) +
               fmix64(a2 + P3) + fmix64(a3 + P0);
  h1_out = x ^ (y * 0x9E3779B97F4A7C15ULL);
  h2_out = (uint32_t)(fmix64(x + y) >> 32);
}

struct Slot96 { uint64_t h1; int32_t val; uint32_t h2; };
static_assert(sizeof(Slot96) == 16, "Slot96 must be 16 bytes");

struct VerifyTable96 {
  std::unique_ptr<Slot96[]> slots;
  size_t mask = 0, count = 0;
  static inline uint64_t hash_of(uint64_t h1) {
    return mix64(h1 ^ 0x9E3779B97F4A7C15ULL);
  }
  void allocate(size_t cap) {
    size_t c = 1024; while (c < cap) c <<= 1;
    slots.reset(new Slot96[c]);
    for (size_t i = 0; i < c; ++i) slots[i].val = -1;
    mask = c - 1; count = 0;
  }
  inline int32_t find_or_insert(uint64_t h1, uint32_t h2, int32_t nv) {
    size_t idx = (size_t)hash_of(h1) & mask;
    while (slots[idx].val >= 0) {
      if (slots[idx].h1 == h1 && slots[idx].h2 == h2) return slots[idx].val;
      idx = (idx + 1) & mask;
    }
    slots[idx].h1 = h1; slots[idx].h2 = h2; slots[idx].val = nv;
    ++count; return nv;
  }
};

static void radix_sort_u64_idx(const uint64_t* keys, std::vector<int32_t>& idx,
                               R_xlen_t n) {
  if (n < 2) return;
  std::vector<int32_t> tmp((size_t)n);
  std::vector<int32_t> cnt(1 << 16);
  for (int pass = 0; pass < 4; ++pass) {
    int shift = pass * 16;
    std::fill(cnt.begin(), cnt.end(), 0);
    for (R_xlen_t i = 0; i < n; ++i)
      ++cnt[(keys[(size_t)idx[(size_t)i]] >> shift) & 0xFFFF];
    int32_t s = 0;
    for (int k = 0; k < (1 << 16); ++k) { int32_t c = cnt[k]; cnt[k] = s; s += c; }
    for (R_xlen_t i = 0; i < n; ++i)
      tmp[(size_t)cnt[(keys[(size_t)idx[(size_t)i]] >> shift) & 0xFFFF]++] =
        idx[(size_t)i];
    idx.swap(tmp);
  }
}

template <class KF>
static void build_block_phase1_sorted(KF kf,
                                      R_xlen_t n_blocks, R_xlen_t period,
                                      int n_threads,
                                      std::vector<int32_t>& first_src,
                                      int32_t& n_out)
{
  if (n_blocks <= 0) { n_out = 0; return; }

  bool sorted = true;
  if (n_blocks >= 2) {
    uint64_t prev = kf(0);
    for (R_xlen_t b = 1; b < n_blocks; ++b) {
      uint64_t k = kf(b);
      if (k < prev) { sorted = false; break; }
      prev = k;
    }
  }
  if (sorted) {
    first_src.clear();
    first_src.reserve((size_t)std::min<R_xlen_t>(n_blocks, (R_xlen_t)1 << 22));
    uint64_t prev = kf(0);
    for (R_xlen_t b = 1; b < n_blocks; ++b) {
      uint64_t k = kf(b);
      if (k != prev) {
        first_src.push_back((int32_t)((int64_t)(b - 1) * (int64_t)period));
        prev = k;
      }
    }
    first_src.push_back((int32_t)((int64_t)(n_blocks - 1) * (int64_t)period));
    n_out = (int32_t)first_src.size();
    return;
  }

  std::vector<uint64_t> keys((size_t)n_blocks);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads) \
    if(n_threads > 1 && n_blocks >= 200000)
#endif
  for (R_xlen_t b = 0; b < n_blocks; ++b)
    keys[(size_t)b] = kf(b);

  std::vector<int32_t> idx((size_t)n_blocks);
  std::iota(idx.begin(), idx.end(), 0);
  radix_sort_u64_idx(keys.data(), idx, n_blocks);

  first_src.clear();
  first_src.reserve((size_t)std::min<R_xlen_t>(n_blocks, (R_xlen_t)1 << 22));
  uint64_t prev = ~0ull;
  int32_t prev_bi = -1;
  for (R_xlen_t t = 0; t < n_blocks; ++t) {
    int32_t bi = idx[(size_t)t];
    uint64_t k = keys[(size_t)bi];
    if (t > 0 && k != prev) {
      first_src.push_back((int32_t)((int64_t)prev_bi * (int64_t)period));
    }
    prev = k;
    prev_bi = bi;
  }
  if (n_blocks > 0) {
    first_src.push_back((int32_t)((int64_t)prev_bi * (int64_t)period));
  }
  n_out = (int32_t)first_src.size();
}

template <class KF>
static void build_general_phase1_sorted(KF kf,
                                        R_xlen_t nlong, int n_threads,
                                        std::vector<int32_t>& row_of,
                                        std::vector<int32_t>& first_src,
                                        int32_t& n_out)
{
  if (nlong <= 0) { n_out = 0; return; }

  bool sorted = true;
  if (nlong >= 2) {
    uint64_t prev = kf(0);
    for (R_xlen_t i = 1; i < nlong; ++i) {
      uint64_t k = kf(i);
      if (k < prev) { sorted = false; break; }
      prev = k;
    }
  }
  if (sorted) {
    first_src.clear();
    first_src.reserve((size_t)std::min<R_xlen_t>(nlong, (R_xlen_t)1 << 22));
    uint64_t p = kf(0);
    int32_t r = 0;
    first_src.push_back(0);
    row_of[0] = 0;
    for (R_xlen_t i = 1; i < nlong; ++i) {
      uint64_t k = kf(i);
      if (k != p) { ++r; first_src.push_back((int32_t)i); p = k; }
      row_of[(size_t)i] = r;
    }
    n_out = r + 1;
    return;
  }

  std::vector<uint64_t> keys((size_t)nlong);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads) \
    if(n_threads > 1 && nlong >= 200000)
#endif
  for (R_xlen_t i = 0; i < nlong; ++i)
    keys[(size_t)i] = kf(i);

  std::vector<int32_t> idx((size_t)nlong);
  std::iota(idx.begin(), idx.end(), 0);
  radix_sort_u64_idx(keys.data(), idx, nlong);

  std::vector<int32_t> sorted_rows((size_t)nlong);
  uint64_t prev = ~0ull;
  int32_t row = -1;
  first_src.clear();
  first_src.reserve((size_t)std::min<R_xlen_t>(nlong, (R_xlen_t)1 << 22));
  for (R_xlen_t t = 0; t < nlong; ++t) {
    int32_t src = idx[(size_t)t];
    uint64_t k = keys[(size_t)src];
    if (t == 0 || k != prev) {
      first_src.push_back(src); prev = k; ++row;
    }
    sorted_rows[(size_t)t] = row;
  }
  n_out = row + 1;

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads) \
    if(n_threads > 1 && nlong >= 200000)
#endif
  for (R_xlen_t t = 0; t < nlong; ++t)
    row_of[(size_t)idx[(size_t)t]] = sorted_rows[(size_t)t];
}

// =====================================================================
// [6] Arg resolution
// =====================================================================
static int resolve_col(SEXP data, SEXP spec, int default_idx) {
  if (Rf_isNull(spec)) return default_idx;
  int ncols = Rf_length(data);
  SEXP names = Rf_getAttrib(data, R_NamesSymbol);
  if (TYPEOF(spec) == INTSXP && XLENGTH(spec) >= 1) {
    int idx = INTEGER(spec)[0] - 1;
    if (idx >= 0 && idx < ncols) return idx;
    stop("Column index out of range");
  }
  if (TYPEOF(spec) == STRSXP && XLENGTH(spec) >= 1) {
    const char* target = CHAR(STRING_ELT(spec, 0));
    for (int i = 0; i < ncols; ++i)
      if (strcmp(CHAR(STRING_ELT(names, i)), target) == 0) return i;
    stop("Column '%s' not found", target);
  }
  stop("Invalid column spec");
  return -1;
}

static std::vector<int> resolve_id_cols(SEXP data, SEXP id_spec,
                                        int var_idx, int val_idx) {
  int ncols = Rf_length(data);
  SEXP names = Rf_getAttrib(data, R_NamesSymbol);
  std::vector<int> id_cols;
  if (Rf_isNull(id_spec)) {
    for (int i = 0; i < ncols; ++i)
      if (i != var_idx && i != val_idx) id_cols.push_back(i);
    if (id_cols.empty())
      stop("dcast_cpp: no id columns inferred; specify id explicitly");
    return id_cols;
  }
  if (TYPEOF(id_spec) == INTSXP) {
    R_xlen_t len = XLENGTH(id_spec);
    std::vector<char> seen(ncols, 0);
    for (R_xlen_t i = 0; i < len; ++i) {
      int a = INTEGER(id_spec)[i] - 1;
      if (a < 0 || a >= ncols) stop("id index out of range");
      if (!seen[a]) { seen[a] = 1; id_cols.push_back(a); }
    }
    return id_cols;
  }
  if (TYPEOF(id_spec) == STRSXP) {
    R_xlen_t len = XLENGTH(id_spec);
    std::vector<char> seen(ncols, 0);
    for (R_xlen_t i = 0; i < len; ++i) {
      const char* tgt = CHAR(STRING_ELT(id_spec, i));
      bool found = false;
      for (int j = 0; j < ncols; ++j)
        if (strcmp(CHAR(STRING_ELT(names, j)), tgt) == 0) {
          if (!seen[j]) { seen[j] = 1; id_cols.push_back(j); }
          found = true; break;
        }
      if (!found) stop("Column '%s' not found", tgt);
    }
    return id_cols;
  }
  stop("Invalid 'id' argument");
  return id_cols;
}

static int infer_var_val_idx(SEXP data, SEXP spec, const char* fallback_name,
                             int fallback_idx) {
  if (!Rf_isNull(spec)) return resolve_col(data, spec, fallback_idx);
  SEXP names = Rf_getAttrib(data, R_NamesSymbol);
  int ncols = Rf_length(data);
  for (int i = 0; i < ncols; ++i)
    if (strcmp(CHAR(STRING_ELT(names, i)), fallback_name) == 0) return i;
  return -1;
}

static bool parse_fill(SEXP fill, double& out) {
  if (Rf_isNull(fill)) return false;
  if (TYPEOF(fill) == REALSXP && XLENGTH(fill) >= 1) {
    double v = REAL(fill)[0];
    if (ISNA(v) || ISNAN(v)) return false;
    out = v; return true;
  }
  if (TYPEOF(fill) == INTSXP && XLENGTH(fill) >= 1) {
    int v = INTEGER(fill)[0];
    if (v == NA_INTEGER) return false;
    out = (double)v; return true;
  }
  if (TYPEOF(fill) == LGLSXP && XLENGTH(fill) >= 1) {
    int v = LOGICAL(fill)[0];
    if (v == NA_LOGICAL) return false;
    out = v ? 1.0 : 0.0; return true;
  }
  return false;
}

static bool try_dense_radix_partition(
    SEXP data, const std::vector<int>& id_cols,
    R_xlen_t n_blocks, R_xlen_t period,
    std::vector<int32_t>& first_src, int32_t& n_out)
{
  const int n_id = (int)id_cols.size();
  if (n_id < 1 || n_blocks < 2) return false;
  std::vector<int32_t> mins(n_id), ranges(n_id);
  std::vector<const int32_t*> vs(n_id);
  for (int j = 0; j < n_id; ++j) {
    SEXP col = VECTOR_ELT(data, id_cols[j]);
    SEXPTYPE t = TYPEOF(col);
    if (t != INTSXP && t != LGLSXP) return false;
    const int32_t* v = (t == INTSXP) ? (const int32_t*)INTEGER(col)
                                     : (const int32_t*)LOGICAL(col);
    vs[j] = v;
    int32_t lo = INT32_MAX, hi = INT32_MIN;
    for (R_xlen_t b = 0; b < n_blocks; ++b) {
      int32_t x = v[b * period];
      if (x == NA_INTEGER) return false;
      if (x < lo) lo = x;
      if (x > hi) hi = x;
    }
    int64_t r = (int64_t)hi - (int64_t)lo + 1;
    if (r <= 0 || r > (int64_t)n_blocks * 2) return false;
    mins[j] = lo; ranges[j] = (int32_t)r;
  }
  std::vector<int32_t> perm((size_t)n_blocks);
  std::iota(perm.begin(), perm.end(), 0);
  std::vector<int32_t> tmp((size_t)n_blocks);
  std::vector<int32_t> cnt;
  for (int j = n_id - 1; j >= 0; --j) {
    const int32_t* v = vs[j];
    int32_t lo = mins[j], rng = ranges[j];
    cnt.assign((size_t)rng + 1, 0);
    for (R_xlen_t b = 0; b < n_blocks; ++b) {
      int32_t x = v[(R_xlen_t)perm[(size_t)b] * period] - lo;
      ++cnt[(size_t)x + 1];
    }
    for (int32_t k = 1; k <= rng; ++k) cnt[(size_t)k] += cnt[(size_t)k - 1];
    for (R_xlen_t b = 0; b < n_blocks; ++b) {
      int32_t x = v[(R_xlen_t)perm[(size_t)b] * period] - lo;
      tmp[(size_t)cnt[(size_t)x]++] = perm[(size_t)b];
    }
    perm.swap(tmp);
  }
  std::vector<int32_t> reps;
  reps.reserve((size_t)std::min<R_xlen_t>(n_blocks, (R_xlen_t)1 << 20));
  auto keys_equal = [&](int32_t a, int32_t b) -> bool {
    for (int j = 0; j < n_id; ++j)
      if (vs[j][(R_xlen_t)a * period] != vs[j][(R_xlen_t)b * period]) return false;
    return true;
  };
  int32_t run_last = perm[0];
  for (R_xlen_t b = 1; b < n_blocks; ++b) {
    int32_t cur = perm[(size_t)b];
    if (!keys_equal(cur, run_last)) {
      reps.push_back(run_last);
    }
    run_last = cur;
  }
  reps.push_back(run_last);
  std::sort(reps.begin(), reps.end());
  first_src.resize(reps.size());
  for (size_t i = 0; i < reps.size(); ++i)
    first_src[i] = (int32_t)((int64_t)reps[i] * (int64_t)period);
  n_out = (int32_t)reps.size();
  return true;
}

// =====================================================================
// [7] Main entry
// =====================================================================
// [[Rcpp::export]]
SEXP dcast_cpp(SEXP data, SEXP id = R_NilValue,
               SEXP variable = R_NilValue, SEXP value = R_NilValue,
               SEXP variable_name = R_NilValue,
               int cores = 0,
               SEXP fill = R_NilValue,
               bool na_rm = false) {
  // variable_name is retained for ABI/Rcpp-generated R-call signature
  // stability; the R layer resolves the actual name and does not use it here.
  (void)variable_name;
  init_cpu_features();

  if (TYPEOF(data) != VECSXP) stop("data must be a data.frame");
  int ncols = Rf_length(data);
  if (ncols < 2) stop("data must have at least 2 columns");

  int var_idx = infer_var_val_idx(data, variable, "variable", ncols - 2);
  int val_idx = infer_var_val_idx(data, value,    "value",    ncols - 1);
  if (var_idx < 0) stop("cannot infer 'variable' column; specify explicitly");
  if (val_idx < 0) stop("cannot infer 'value' column; specify explicitly");
  if (var_idx == val_idx) stop("variable and value must be distinct columns");

  std::vector<int> id_cols = resolve_id_cols(data, id, var_idx, val_idx);
  int n_id = (int)id_cols.size();
  if (n_id == 0) stop("no id columns");

  SEXP id_names = Rf_getAttrib(data, R_NamesSymbol);
  SEXP var_col  = VECTOR_ELT(data, var_idx);
  SEXP val_col  = VECTOR_ELT(data, val_idx);
  R_xlen_t nlong = XLENGTH(var_col);
  for (int idx : id_cols)
    if (XLENGTH(VECTOR_ELT(data, idx)) != nlong)
      stop("id columns must have the same length as variable/value");
  if (nlong == 0) stop("data has no rows");
  if (nlong > (R_xlen_t)INT32_MAX)
    stop("dcast_cpp: input too large (nlong > INT32_MAX)");

  double fill_val = NA_REAL;
  bool use_fill = parse_fill(fill, fill_val);

  // Promote input columns to 2 MB pages (MADV_COLLAPSE for existing pages).
  hint_readonly(var_col);
  hint_readonly(val_col);
  for (int idx : id_cols) hint_readonly(VECTOR_ELT(data, idx));

  int  actual_threads = 1;
#ifdef _OPENMP
  bool use_parallel = false;
  int hw = omp_get_max_threads();
  int target = 1;
  if (cores > 0) {
    target = std::max(1, std::min(cores, hw));
  } else if (nlong >= (R_xlen_t)200000 && hw > 1) {
    R_xlen_t calc = nlong / 200000;
    target = (int)std::min<R_xlen_t>((R_xlen_t)hw, calc);
    target = std::max(1, target);
  }
  actual_threads = target;
  use_parallel = (actual_threads > 1) && (nlong > 50000);
  if (use_parallel) {
    omp_set_dynamic(0);
    omp_set_num_threads(actual_threads);
  }
#else
  (void)cores;
#endif

  bool var_is_factor = Rf_isFactor(var_col);
  SEXP var_levels = var_is_factor
                    ? Rf_getAttrib(var_col, R_LevelsSymbol) : R_NilValue;
  SEXPTYPE var_type = TYPEOF(var_col);
  const int*    var_int = (var_type == INTSXP)  ? INTEGER(var_col) : nullptr;
  const int*    var_lgl = (var_type == LGLSXP)  ? LOGICAL(var_col) : nullptr;
  const double* var_dbl = (var_type == REALSXP) ? REAL(var_col)    : nullptr;
  const int*    var_fac = var_is_factor ? INTEGER(var_col) : nullptr;

  R_xlen_t period = 0;
  {
    auto veq = [&](R_xlen_t a, R_xlen_t b) -> bool {
      if (var_type == STRSXP)
        return STRING_ELT(var_col, a) == STRING_ELT(var_col, b);
      if (var_type == REALSXP) {
        double x = var_dbl[a], y = var_dbl[b];
        if (ISNAN(x) || ISNAN(y)) return ISNAN(x) && ISNAN(y);
        return x == y;
      }
      if (var_is_factor) return var_fac[a] == var_fac[b];
      if (var_type == INTSXP) return var_int[a] == var_int[b];
      if (var_type == LGLSXP) return var_lgl[a] == var_lgl[b];
      return false;
    };
    R_xlen_t lim = std::min<R_xlen_t>(nlong, (R_xlen_t)1024);
    for (R_xlen_t i = 1; i < lim; ++i)
      if (veq(i, 0)) { period = i; break; }
    if (period > 0 && (nlong % period) == 0) {
      R_xlen_t step = std::max<R_xlen_t>(1, nlong / 4096);
      for (R_xlen_t i = 0; i < nlong; i += step)
        if (!veq(i, i % period)) { period = 0; break; }
    } else period = 0;
  }
  bool block_path = (period > 0);

  if (block_path) {
    R_xlen_t n_blocks = nlong / period;
    for (int j = 0; j < n_id && period > 0; ++j) {
      SEXP col = VECTOR_ELT(data, id_cols[j]);
      SEXPTYPE t = TYPEOF(col);
      if (t == INTSXP || t == LGLSXP) {
        const int* v = (t == INTSXP) ? INTEGER(col) : LOGICAL(col);
        for (R_xlen_t b = 0; b < n_blocks && period > 0; ++b) {
          int first = v[b * period];
          if (period > 1 && v[b * period + period - 1] != first) { period = 0; break; }
          if (period > 2 && v[b * period + period / 2] != first) { period = 0; break; }
        }
      } else if (t == STRSXP) {
        const SEXP* s = DCAST_STRING_PTR_RO(col);
        for (R_xlen_t b = 0; b < n_blocks && period > 0; ++b) {
          SEXP first = s[b * period];
          if (period > 1 && s[b * period + period - 1] != first) { period = 0; break; }
          if (period > 2 && s[b * period + period / 2] != first) { period = 0; break; }
        }
      } else if (t == REALSXP) {
        const double* s = REAL(col);
        for (R_xlen_t b = 0; b < n_blocks && period > 0; ++b) {
          double first = s[b * period];
          if (period > 1 && s[b * period + period - 1] != first) { period = 0; break; }
          if (period > 2 && s[b * period + period / 2] != first) { period = 0; break; }
        }
      } else period = 0;
    }
    if (period > 0) {
      std::unordered_set<std::string> seen;
      char buf[64];
      for (R_xlen_t k = 0; k < period; ++k) {
        std::string key;
        if (var_type == STRSXP) {
          SEXP s = STRING_ELT(var_col, k);
          key = (s == NA_STRING) ? std::string("\x01NA") : std::string(CHAR(s));
        } else if (var_is_factor) {
          int code = var_fac[k];
          if (code == NA_INTEGER) key = "\x01NA";
          else key = std::string(CHAR(STRING_ELT(var_levels, code - 1)));
        } else if (var_type == INTSXP) {
          int len = fast_itoa(var_int[k], buf); key.assign(buf, len);
        } else if (var_type == LGLSXP) {
          int v = var_lgl[k];
          key = (v == NA_LOGICAL) ? "\x01NA" : (v ? "TRUE" : "FALSE");
        } else if (var_type == REALSXP) {
          double x = var_dbl[k];
          if (ISNA(x)) key = "\x01NA";
          else {
            int l = std::snprintf(buf, sizeof(buf), "%.17g", x);
            if (l < 0) l = 0;
            if ((size_t)l >= sizeof(buf)) l = (int)sizeof(buf) - 1;
            key.assign(buf, l);
          }
        }
        if (!seen.insert(key).second) { period = 0; break; }
      }
    }
    block_path = (period > 0);
  }

  bool perm_shortcut = false;
  std::vector<int32_t> perm_first_src;
  int32_t perm_n_out = 0;

  if (block_path) {
    R_xlen_t n_blk = nlong / period;
    for (int j = 0; j < n_id && !perm_shortcut; ++j) {
      SEXP col = VECTOR_ELT(data, id_cols[j]);
      SEXPTYPE t = TYPEOF(col);
      if (t != INTSXP && t != LGLSXP) continue;
      const int32_t* v = (t == INTSXP) ? (const int32_t*)INTEGER(col)
                                       : (const int32_t*)LOGICAL(col);
      int32_t mn = INT32_MAX, mx = INT32_MIN;
      bool has_na = false;
      for (R_xlen_t b = 0; b < n_blk; ++b) {
        int32_t x = v[b * period];
        if (x == NA_INTEGER) { has_na = true; break; }
        if (x < mn) mn = x;
        if (x > mx) mx = x;
      }
      if (has_na) continue;
      int64_t range = (int64_t)mx - (int64_t)mn + 1;
      if (range <= 0 || range > (int64_t)n_blk * 4) continue;
      std::vector<int32_t> lut((size_t)range, -1);
      std::vector<int32_t> codes((size_t)n_blk);
      int32_t next_code = 0;
      bool ok = true;
      for (R_xlen_t b = 0; b < n_blk; ++b) {
        int32_t x = v[b * period] - mn;
        if (x < 0 || x >= range) { ok = false; break; }
        int32_t& slot = lut[(size_t)x];
        if (slot < 0) slot = next_code++;
        codes[(size_t)b] = slot;
      }
      if (!ok || (R_xlen_t)next_code != n_blk) continue;
      perm_first_src.assign((size_t)n_blk, 0);
      for (R_xlen_t b = 0; b < n_blk; ++b)
        perm_first_src[(size_t)codes[(size_t)b]] =
          (int32_t)((int64_t)b * (int64_t)period);
      perm_n_out = (int32_t)n_blk;
      perm_shortcut = true;
    }
    if (!perm_shortcut && n_blk >= 1024) {
      if (try_dense_radix_partition(data, id_cols, n_blk, period,
                                    perm_first_src, perm_n_out))
        perm_shortcut = true;
    }
  }

  std::vector<ColDesc> desc(n_id);
  std::vector<std::vector<int32_t>> owned(n_id);
  const R_xlen_t id_n    = block_path ? (nlong / period) : nlong;
  const R_xlen_t id_step = block_path ? period : 1;
  auto id_index = [&](R_xlen_t b) -> R_xlen_t { return b * id_step; };

  if (!perm_shortcut) {
    for (int j = 0; j < n_id; ++j) {
      SEXP col = VECTOR_ELT(data, id_cols[j]);
      SEXPTYPE t = TYPEOF(col);
      ColDesc& d = desc[j];
      if (t == INTSXP || t == LGLSXP) {
        const int32_t* v = (t == INTSXP) ? (const int32_t*)INTEGER(col)
                                         : (const int32_t*)LOGICAL(col);
        int32_t mn = INT32_MAX, mx = INT32_MIN;
        bool has_na = false;
        for (R_xlen_t b = 0; b < id_n; ++b) {
          int32_t x = v[id_index(b)];
          if (x == NA_INTEGER) { has_na = true; continue; }
          if (x < mn) mn = x;
          if (x > mx) mx = x;
        }
        if (mn > mx) { mn = 0; mx = 0; }
        uint64_t range = (uint64_t)((int64_t)mx - (int64_t)mn) + 1;
        uint64_t units = range + (has_na ? 1 : 0);
        if (units <= ((uint64_t)1 << 26) || units <= (uint64_t)id_n * 4) {
          if (block_path) {
            owned[j].resize((size_t)id_n);
            for (R_xlen_t b = 0; b < id_n; ++b) {
              int32_t x = v[id_index(b)];
              int32_t code;
              if (x == NA_INTEGER) code = has_na ? (int32_t)(units - 1) : 0;
              else                 code = (int32_t)((int64_t)x - (int64_t)mn);
              owned[j][b] = code;
            }
            d.code = owned[j].data();
          } else {
            d.raw = v; d.mn = mn; d.has_na = has_na;
            d.na_code = has_na ? (uint32_t)(units - 1) : 0;
          }
          d.units = (uint32_t)units;
        } else {
          owned[j].resize((size_t)id_n);
          std::unordered_map<int32_t, int32_t> m; m.reserve(4096);
          int32_t card = 0;
          for (R_xlen_t b = 0; b < id_n; ++b) {
            int32_t x = v[id_index(b)];
            auto it = m.find(x);
            if (it == m.end()) m.emplace(x, card), owned[j][b] = card++;
            else               owned[j][b] = it->second;
          }
          d.code = owned[j].data(); d.units = (uint32_t)card;
        }
      } else if (t == STRSXP) {
        owned[j].resize((size_t)id_n);
        std::unordered_map<SEXP, int32_t> m; m.reserve(4096);
        int32_t card = 0;
        for (R_xlen_t b = 0; b < id_n; ++b) {
          SEXP s = STRING_ELT(col, id_index(b));
          auto it = m.find(s);
          if (it == m.end()) m.emplace(s, card), owned[j][b] = card++;
          else               owned[j][b] = it->second;
        }
        d.code = owned[j].data(); d.units = (uint32_t)card;
      } else if (t == REALSXP) {
        owned[j].resize((size_t)id_n);
        std::unordered_map<uint64_t, int32_t> m; m.reserve(4096);
        int32_t card = 0;
        for (R_xlen_t b = 0; b < id_n; ++b) {
          double x = REAL(col)[id_index(b)];
          uint64_t bits; std::memcpy(&bits, &x, sizeof(bits));
          if (x == 0.0) bits = 0;
          auto it = m.find(bits);
          if (it == m.end()) m.emplace(bits, card), owned[j][b] = card++;
          else               owned[j][b] = it->second;
        }
        d.code = owned[j].data(); d.units = (uint32_t)card;
      } else stop("unsupported id column type");
    }
  }

  std::vector<std::string> col_keys;
  col_keys.reserve(256);
  std::vector<int32_t> col_of;
  int32_t k_out = 0;

  if (block_path) {
    char buf[64];
    for (R_xlen_t k = 0; k < period; ++k) {
      if (var_is_factor) {
        int code = var_fac[k];
        col_keys.emplace_back(code == NA_INTEGER
                              ? "NA" : CHAR(STRING_ELT(var_levels, code - 1)));
      } else if (var_type == INTSXP) {
        char b[16]; int l = fast_itoa(var_int[k], b);
        col_keys.emplace_back(b, l);
      } else if (var_type == LGLSXP) {
        int v = var_lgl[k];
        col_keys.emplace_back(v == NA_LOGICAL ? "NA" : (v ? "TRUE" : "FALSE"));
      } else if (var_type == REALSXP) {
        double x = var_dbl[k];
        int l;
        if (ISNA(x)) { buf[0]='N'; buf[1]='A'; l = 2; }
        else {
          l = std::snprintf(buf, sizeof(buf), "%.17g", x);
          if (l < 0) l = 0;
          if ((size_t)l >= sizeof(buf)) l = (int)sizeof(buf) - 1;
        }
        col_keys.emplace_back(buf, l);
      } else {
        SEXP s = STRING_ELT(var_col, k);
        col_keys.emplace_back(s == NA_STRING ? "NA" : CHAR(s));
      }
    }
    k_out = (int32_t)period;
  } else {
    col_of.assign((size_t)nlong, -1);
    if (var_is_factor && Rf_length(var_levels) < 100000) {
      int L = Rf_length(var_levels);
      std::vector<int32_t> c2c((size_t)L + 1, -1);
      for (R_xlen_t i = 0; i < nlong; ++i) {
        int code = var_fac[i];
        size_t slot = (code == NA_INTEGER) ? 0 : (size_t)code;
        int32_t c = c2c[slot];
        if (c < 0) {
          c = k_out++; c2c[slot] = c;
          col_keys.emplace_back(code == NA_INTEGER
                                ? "NA" : CHAR(STRING_ELT(var_levels, code - 1)));
        }
        col_of[i] = c;
      }
    } else if (var_type == STRSXP) {
      std::unordered_map<SEXP, int32_t> m; m.reserve(256);
      for (R_xlen_t i = 0; i < nlong; ++i) {
        SEXP s = STRING_ELT(var_col, i);
        auto it = m.find(s);
        if (it == m.end()) {
          int32_t c = k_out++; m.emplace(s, c);
          col_keys.emplace_back(s == NA_STRING ? "NA" : CHAR(s));
          col_of[i] = c;
        } else col_of[i] = it->second;
      }
    } else if (var_type == INTSXP) {
      int32_t mn = INT32_MAX, mx = INT32_MIN;
      for (R_xlen_t i = 0; i < nlong; ++i) {
        int32_t v = var_int[i];
        if (v == NA_INTEGER) continue;
        if (v < mn) mn = v;
        if (v > mx) mx = v;
      }
      if (mx >= mn && (int64_t)mx - (int64_t)mn < 1000000) {
        std::vector<int32_t> lut((size_t)((int64_t)mx - (int64_t)mn) + 1, -1);
        int32_t na_col = -1;
        for (R_xlen_t i = 0; i < nlong; ++i) {
          int32_t v = var_int[i];
          if (v == NA_INTEGER) {
            if (na_col < 0) { na_col = k_out++; col_keys.emplace_back("NA"); }
            col_of[i] = na_col;
          } else {
            int32_t& s = lut[(size_t)((int64_t)v - (int64_t)mn)];
            if (s < 0) {
              s = k_out++;
              char b[16]; int l = fast_itoa(v, b);
              col_keys.emplace_back(b, l);
            }
            col_of[i] = s;
          }
        }
      } else {
        std::unordered_map<int32_t, int32_t> m; m.reserve(256);
        for (R_xlen_t i = 0; i < nlong; ++i) {
          int32_t v = var_int[i];
          auto it = m.find(v);
          if (it == m.end()) {
            int32_t c = k_out++; m.emplace(v, c);
            char b[16]; int l = fast_itoa(v, b);
            col_keys.emplace_back(b, l); col_of[i] = c;
          } else col_of[i] = it->second;
        }
      }
    } else if (var_type == REALSXP) {
      std::unordered_map<uint64_t, int32_t> m; m.reserve(256);
      char buf[64];
      for (R_xlen_t i = 0; i < nlong; ++i) {
        double x = var_dbl[i];
        uint64_t bits; std::memcpy(&bits, &x, sizeof(bits));
        if (x == 0.0) bits = 0;
        auto it = m.find(bits);
        if (it == m.end()) {
          int32_t c = k_out++; m.emplace(bits, c);
          int l;
          if (ISNA(x)) { buf[0]='N'; buf[1]='A'; l = 2; }
          else {
            l = std::snprintf(buf, sizeof(buf), "%.17g", x);
            if (l < 0) l = 0;
            if ((size_t)l >= sizeof(buf)) l = (int)sizeof(buf) - 1;
          }
          col_keys.emplace_back(buf, l); col_of[i] = c;
        } else col_of[i] = it->second;
      }
    } else if (var_type == LGLSXP) {
      int32_t F = -1, T = -1, N = -1;
      for (R_xlen_t i = 0; i < nlong; ++i) {
        int v = var_lgl[i];
        if (v == NA_LOGICAL) {
          if (N < 0) { N = k_out++; col_keys.emplace_back("NA"); }
          col_of[i] = N;
        } else if (v) {
          if (T < 0) { T = k_out++; col_keys.emplace_back("TRUE"); }
          col_of[i] = T;
        } else {
          if (F < 0) { F = k_out++; col_keys.emplace_back("FALSE"); }
          col_of[i] = F;
        }
      }
    } else stop("unsupported variable column type");
  }
  if (k_out == 0) stop("empty output");

  std::vector<int32_t> row_of;
  std::vector<int32_t> first_src;
  int32_t n_out = 0;

  std::vector<int> shifts(n_id);
  bool bits_overflow = false;
  {
    int s = 0;
    for (int j = n_id - 1; j >= 0; --j) {
      uint32_t u = desc[j].units;
      int b = (u <= 1) ? 1 : (int)(64 - __builtin_clzll((uint64_t)(u - 1)));
      if (b < 1) b = 1;
      shifts[j] = s; s += b;
      if (s > 64) { bits_overflow = true; break; }
    }
    if (s > 64) bits_overflow = true;
  }

  if (block_path) {
    R_xlen_t n_blocks = nlong / period;
    first_src.reserve((size_t)n_blocks);
    if (perm_shortcut) {
      first_src = std::move(perm_first_src);
      n_out = perm_n_out;
    } else if (!bits_overflow) {
      switch (n_id) {
      case 1: build_block_phase1_sorted(KeyFnN<1>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 2: build_block_phase1_sorted(KeyFnN<2>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 3: build_block_phase1_sorted(KeyFnN<3>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 4: build_block_phase1_sorted(KeyFnN<4>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 5: build_block_phase1_sorted(KeyFnN<5>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 6: build_block_phase1_sorted(KeyFnN<6>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 7: build_block_phase1_sorted(KeyFnN<7>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      case 8: build_block_phase1_sorted(KeyFnN<8>{desc.data(), shifts.data()},
          n_blocks, period, actual_threads, first_src, n_out); break;
      default: build_block_phase1_sorted(
          KeyFnDyn{desc.data(), shifts.data(), n_id},
          n_blocks, period, actual_threads, first_src, n_out); break;
      }
    } else {
      constexpr int BATCH = 64;
      std::unique_ptr<uint64_t[]> h1s(new uint64_t[(size_t)n_blocks]);
      std::unique_ptr<uint32_t[]> h2s(new uint32_t[(size_t)n_blocks]);
      std::vector<int32_t> code_buf((size_t)BATCH * (size_t)n_id);
      for (R_xlen_t base = 0; base < n_blocks; base += BATCH) {
        int tb = (int)std::min<R_xlen_t>((R_xlen_t)BATCH, n_blocks - base);
        for (int c = 0; c < n_id; ++c) {
          const ColDesc& d = desc[c];
          int32_t* dst = code_buf.data() + c;
          if (d.code) {
            const int32_t* src = d.code + base;
            for (int r = 0; r < tb; ++r) dst[(size_t)r * n_id] = src[r];
          } else {
            const int32_t* src = d.raw + base;
            const int32_t mn = d.mn;
            const uint32_t na_code = d.na_code;
            for (int r = 0; r < tb; ++r) {
              int32_t v = src[r];
              dst[(size_t)r * n_id] = (v == NA_INTEGER)
                ? (int32_t)na_code : (int32_t)((int64_t)v - (int64_t)mn);
            }
          }
        }
        for (int r = 0; r < tb; ++r) {
          uint64_t h1; uint32_t h2;
          hash_codes_96(code_buf.data() + (size_t)r * n_id, n_id, h1, h2);
          h1s[(size_t)(base + r)] = h1;
          h2s[(size_t)(base + r)] = h2;
        }
      }
      VerifyTable96 ht;
      size_t cap = (size_t)std::max<uint64_t>(4096,
        2 * std::min<R_xlen_t>(n_blocks, (R_xlen_t)8e6));
      ht.allocate(cap);
      uint64_t pk_h1 = ~0ull; uint32_t pk_h2 = 0; bool hp = false;
      for (R_xlen_t b = 0; b < n_blocks; ++b) {
        uint64_t h1 = h1s[(size_t)b];
        uint32_t h2 = h2s[(size_t)b];
        if (hp && h1 == pk_h1 && h2 == pk_h2) continue;
        int32_t r = ht.find_or_insert(h1, h2, n_out);
        if (r == n_out) { first_src.push_back((int32_t)(b * period)); ++n_out; }
        pk_h1 = h1; pk_h2 = h2; hp = true;
      }
    }
  } else {
    row_of.assign((size_t)nlong, -1);
    first_src.reserve((size_t)std::min<R_xlen_t>(nlong, (R_xlen_t)1 << 22));
    if (!bits_overflow) {
      switch (n_id) {
      case 1: build_general_phase1_sorted(KeyFnN<1>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 2: build_general_phase1_sorted(KeyFnN<2>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 3: build_general_phase1_sorted(KeyFnN<3>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 4: build_general_phase1_sorted(KeyFnN<4>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 5: build_general_phase1_sorted(KeyFnN<5>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 6: build_general_phase1_sorted(KeyFnN<6>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 7: build_general_phase1_sorted(KeyFnN<7>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      case 8: build_general_phase1_sorted(KeyFnN<8>{desc.data(), shifts.data()},
          nlong, actual_threads, row_of, first_src, n_out); break;
      default: build_general_phase1_sorted(
          KeyFnDyn{desc.data(), shifts.data(), n_id},
          nlong, actual_threads, row_of, first_src, n_out); break;
      }
    } else {
      constexpr int BATCH = 64;
      std::unique_ptr<uint64_t[]> h1s(new uint64_t[(size_t)nlong]);
      std::unique_ptr<uint32_t[]> h2s(new uint32_t[(size_t)nlong]);
      std::vector<int32_t> code_buf((size_t)BATCH * (size_t)n_id);
      for (R_xlen_t base = 0; base < nlong; base += BATCH) {
        int tb = (int)std::min<R_xlen_t>((R_xlen_t)BATCH, nlong - base);
        for (int c = 0; c < n_id; ++c) {
          const ColDesc& d = desc[c];
          int32_t* dst = code_buf.data() + c;
          if (d.code) {
            const int32_t* src = d.code + base;
            for (int r = 0; r < tb; ++r) dst[(size_t)r * n_id] = src[r];
          } else {
            const int32_t* src = d.raw + base;
            const int32_t mn = d.mn;
            const uint32_t na_code = d.na_code;
            for (int r = 0; r < tb; ++r) {
              int32_t v = src[r];
              dst[(size_t)r * n_id] = (v == NA_INTEGER)
                ? (int32_t)na_code : (int32_t)((int64_t)v - (int64_t)mn);
            }
          }
        }
        for (int r = 0; r < tb; ++r) {
          uint64_t h1; uint32_t h2;
          hash_codes_96(code_buf.data() + (size_t)r * n_id, n_id, h1, h2);
          h1s[(size_t)(base + r)] = h1;
          h2s[(size_t)(base + r)] = h2;
        }
      }
      VerifyTable96 ht;
      size_t cap = (size_t)std::max<uint64_t>(4096,
        2 * std::min<R_xlen_t>(nlong, (R_xlen_t)8e6));
      ht.allocate(cap);
      uint64_t pk_h1 = ~0ull; uint32_t pk_h2 = 0; bool hp = false;
      for (R_xlen_t i = 0; i < nlong; ++i) {
        uint64_t h1 = h1s[(size_t)i];
        uint32_t h2 = h2s[(size_t)i];
        if (hp && h1 == pk_h1 && h2 == pk_h2) {
          row_of[(size_t)i] = n_out - 1; continue;
        }
        int32_t r = ht.find_or_insert(h1, h2, n_out);
        if (r == n_out) { first_src.push_back((int32_t)i); ++n_out; }
        row_of[(size_t)i] = r; pk_h1 = h1; pk_h2 = h2; hp = true;
      }
    }
  }
  if (n_out == 0) stop("empty output");

  int total_out_cols = n_id + k_out;
  SEXP out       = PROTECT(Rf_allocVector(VECSXP, total_out_cols));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, total_out_cols));
  const int32_t* fs = first_src.data();

  std::vector<SEXP> id_dst_sext(n_id);
  std::vector<const void*> id_src_ptr(n_id);
  std::vector<void*>       id_dst_ptr(n_id);
  for (int j = 0; j < n_id; ++j) {
    SEXP src = VECTOR_ELT(data, id_cols[j]);
    SEXPTYPE t = TYPEOF(src);
    SEXP dst = PROTECT(alloc_smart(t, n_out));
    Rf_copyMostAttrib(src, dst);
    id_dst_sext[j] = dst;
    switch (t) {
    case INTSXP:  id_src_ptr[j] = (const void*)INTEGER(src);
                  id_dst_ptr[j] = (void*)INTEGER(dst); break;
    case LGLSXP:  id_src_ptr[j] = (const void*)LOGICAL(src);
                  id_dst_ptr[j] = (void*)LOGICAL(dst); break;
    case REALSXP: id_src_ptr[j] = (const void*)REAL(src);
                  id_dst_ptr[j] = (void*)REAL(dst); break;
    case STRSXP:  id_src_ptr[j] = (const void*)DCAST_STRING_PTR_RO(src);
                  id_dst_ptr[j] = nullptr; break;
    default:      id_src_ptr[j] = nullptr; id_dst_ptr[j] = nullptr;
    }
    SET_VECTOR_ELT(out, j, dst);
    SET_STRING_ELT(out_names, j, STRING_ELT(id_names, id_cols[j]));
    UNPROTECT(1);
  }

  SEXPTYPE val_type = TYPEOF(val_col);
  if (val_type != REALSXP && val_type != INTSXP && val_type != LGLSXP)
    stop("dcast_cpp only supports numeric/logical value columns");

  const bool dense = ((R_xlen_t)n_out * (R_xlen_t)k_out == nlong);

  std::vector<double*> out_val((size_t)k_out);
  for (int k = 0; k < k_out; ++k) {
    SEXP vc = PROTECT(alloc_smart(REALSXP, n_out));
    out_val[(size_t)k] = REAL(vc);
    SET_VECTOR_ELT(out, n_id + k, vc);
    SEXP ch = PROTECT(Rf_mkCharLen(col_keys[(size_t)k].data(),
                                   (int)col_keys[(size_t)k].size()));
    SET_STRING_ELT(out_names, n_id + k, ch);
    UNPROTECT(2);
  }

  const double* vs = (val_type == REALSXP) ? REAL(val_col) : nullptr;
  const int*    vi = (val_type == INTSXP)  ? INTEGER(val_col) : nullptr;
  const int*    vl = (val_type == LGLSXP)  ? LOGICAL(val_col) : nullptr;

  auto do_id_gather = [&]() {
    if (n_id >= 2 && n_out >= 20000 && actual_threads > 1 &&
        id_src_ptr[0] &&
        (TYPEOF(VECTOR_ELT(data, id_cols[0])) == INTSXP ||
         TYPEOF(VECTOR_ELT(data, id_cols[0])) == LGLSXP)) {
      std::vector<const int*> s(n_id); std::vector<int*> d(n_id);
      for (int j = 0; j < n_id; ++j) {
        s[j] = (const int*)id_src_ptr[j];
        d[j] = (int*)id_dst_ptr[j];
      }
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(actual_threads)
#endif
      for (int j = 0; j < n_id; ++j) {
        const int* __restrict__ sj = s[j];
        int* __restrict__ dj = d[j];
        if (!sj || !dj) continue;
        for (int32_t i = 0; i < n_out; ++i) dj[i] = sj[fs[i]];
      }
      return;
    }
    if (n_id >= 2 && n_out >= 20000 && actual_threads > 1 &&
        id_src_ptr[0] &&
        TYPEOF(VECTOR_ELT(data, id_cols[0])) == REALSXP) {
      std::vector<const double*> s(n_id); std::vector<double*> d(n_id);
      for (int j = 0; j < n_id; ++j) {
        s[j] = (const double*)id_src_ptr[j];
        d[j] = (double*)id_dst_ptr[j];
      }
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(actual_threads)
#endif
      for (int j = 0; j < n_id; ++j) {
        const double* __restrict__ sj = s[j];
        double* __restrict__ dj = d[j];
        if (!sj || !dj) continue;
        for (int32_t i = 0; i < n_out; ++i) dj[i] = sj[fs[i]];
      }
      return;
    }
    for (int j = 0; j < n_id; ++j) {
      SEXP src = VECTOR_ELT(data, id_cols[j]);
      SEXPTYPE t = TYPEOF(src);
      if (t == STRSXP) {
        const SEXP* s = (const SEXP*)id_src_ptr[j];
        for (int32_t i = 0; i < n_out; ++i)
          SET_STRING_ELT(id_dst_sext[j], i, s[fs[i]]);
      } else if (t == INTSXP || t == LGLSXP) {
        const int* s = (const int*)id_src_ptr[j];
        int* d = (int*)id_dst_ptr[j];
        for (int32_t i = 0; i < n_out; ++i) d[i] = s[fs[i]];
      } else if (t == REALSXP) {
        const double* s = (const double*)id_src_ptr[j];
        double* d = (double*)id_dst_ptr[j];
        for (int32_t i = 0; i < n_out; ++i) d[i] = s[fs[i]];
      }
    }
  };

  if (block_path) {
    const R_xlen_t n_blocks = (R_xlen_t)n_out;
    const int kk = (int)k_out;
    const int period_i = (int)period;
    const bool merged_direct = dense && !na_rm && !use_fill;

    if (merged_direct && val_type == REALSXP) {
      do_id_gather();
      const bool use_8x8 = (period_i >= 8) && (period_i <= 32) &&
                           g_have_avx512;
      if (use_8x8) {
        transpose_8x8_rows(fs, vs, out_val.data(), n_blocks, kk,
                           period_i, actual_threads, false, NA_REAL);
      } else {
        transpose_tile(fs, (const void*)vs, out_val.data(),
                       n_blocks, kk, period_i, actual_threads, 0,
                       false, NA_REAL);
      }
    } else {
      bool need_fill = (!dense) || na_rm;
      double fv = use_fill ? fill_val : NA_REAL;
      if (need_fill)
        for (int k = 0; k < k_out; ++k)
          fill_double_ptr(out_val[(size_t)k], fv, (size_t)n_out);
      do_id_gather();
      if (val_type == REALSXP) {
        transpose_tile(fs, (const void*)vs, out_val.data(),
                       n_blocks, kk, period_i, actual_threads, 0, na_rm, fv);
      } else if (val_type == INTSXP) {
        transpose_tile(fs, (const void*)vi, out_val.data(),
                       n_blocks, kk, period_i, actual_threads, 1, na_rm, fv);
      } else {
        transpose_tile(fs, (const void*)vl, out_val.data(),
                       n_blocks, kk, period_i, actual_threads, 2, na_rm, fv);
      }
    }
  } else {
    do_id_gather();
    bool need_fill = (!dense) || na_rm;
    double fv = use_fill ? fill_val : NA_REAL;
    if (need_fill)
      for (int k = 0; k < k_out; ++k)
        fill_double_ptr(out_val[(size_t)k], fv, (size_t)n_out);

    const int32_t* rp = row_of.data();
    const int32_t* cp = col_of.data();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(actual_threads) if(use_parallel)
#endif
    for (R_xlen_t i = 0; i < nlong; ++i) {
      int32_t c = cp[i];
      int32_t r = rp[i];
      if (val_type == REALSXP) {
        double v = vs[i];
        if (na_rm && ISNAN(v)) continue;
        out_val[(size_t)c][r] = v;
      } else if (val_type == INTSXP) {
        int v = vi[i];
        if (na_rm && v == NA_INTEGER) continue;
        out_val[(size_t)c][r] = (v == NA_INTEGER) ? NA_REAL : (double)v;
      } else {
        int v = vl[i];
        if (na_rm && v == NA_LOGICAL) continue;
        out_val[(size_t)c][r] = (v == NA_LOGICAL) ? NA_REAL : (v ? 1.0 : 0.0);
      }
    }
  }

  Rf_setAttrib(out, R_NamesSymbol, out_names);
  {
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = -n_out;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    UNPROTECT(1);
  }
  Rf_setAttrib(out, R_ClassSymbol, cached_df_class());
  UNPROTECT(2);
  return out;
}
