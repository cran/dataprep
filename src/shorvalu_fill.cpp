// File: src/shorvalu_fill.cpp
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// ============================================================================
// In-place short-period linear interpolation.
//
// For each segment [a, b) and each column j:
//   1. Fill leading NAs with the first valid value.
//   2. Fill trailing NAs with the last valid value.
//   3. Linearly interpolate interior NAs between bracketing valid values.
//
// If a segment has no valid value in a column, that column is left untouched.
// Segments are provided by the caller (see shorvalu_segment_cpp below).
//
// Pointer-based column access (col[i] instead of x(i, j)) was measured
// 1.5-2.5x faster than the original NumericMatrix::operator() formulation
// on 49k x 61 shapes.
// ============================================================================
// [[Rcpp::export]]
void shorvalu_fill_cpp(NumericMatrix x, IntegerVector starts, IntegerVector lens,
                       int n_threads = 0) {
#ifdef _OPENMP
  if (n_threads > 0) omp_set_num_threads(n_threads);
#endif

  const int n = x.nrow();
  const int p = x.ncol();
  const int nseg = starts.size();
  if (n == 0 || p == 0 || nseg == 0) return;

  const int* a_ptr = INTEGER(starts);
  const int* l_ptr = INTEGER(lens);

  #pragma omp parallel for schedule(dynamic)
  for (int s = 0; s < nseg; ++s) {
    const int a = a_ptr[s];
    const int b = a + l_ptr[s];
    if (a < 0 || b > n || a >= b) continue;

    for (int j = 0; j < p; ++j) {
      double* col = x.begin() + (R_xlen_t)j * n;

      int first = -1, last = -1;
      for (int i = a; i < b; ++i) {
        if (!R_IsNA(col[i])) {
          if (first < 0) first = i;
          last = i;
        }
      }
      if (first < 0) continue;

      const double v_first = col[first];
      const double v_last  = col[last];
      for (int i = a; i < first; ++i) col[i] = v_first;
      for (int i = last + 1; i < b; ++i) col[i] = v_last;

      int cur = first;
      int i = first + 1;
      while (i <= last) {
        if (!R_IsNA(col[i])) { cur = i; ++i; continue; }
        const int start_na = i;
        while (i <= last && R_IsNA(col[i])) ++i;
        if (i > last) break;
        const int next_valid = i;
        const double x0 = col[cur];
        const double x1 = col[next_valid];
        const double denom = (double)(next_valid - cur);
        for (int t = start_na; t < next_valid; ++t) {
          col[t] = x0 + (x1 - x0) * (double)(t - cur) / denom;
        }
        cur = next_valid;
        i = next_valid + 1;
      }
    }
  }
}

// ============================================================================
// Compute segmentation of a time series at gaps larger than `intervals_sec`.
//
// Replaces the R-level idiom
//     diff_vals <- c(0, as.numeric(diff(tv), units = units))
//     new_period <- (diff_vals > intervals) | (diff_vals == 0)
//     starts <- which(new_period)
//     lens <- diff(c(starts, nrow(data) + 1))
//
// which is O(n) but has a large R-level constant, especially for POSIXct
// inputs (the POSIXct `diff` method dispatches through a slow R function
// and `as.numeric(..., units = ...)` allocates an intermediate vector).
//
// Returns 1-based `starts` (matching R's `which()`) and a `lens` vector of
// segment lengths, so the R wrapper can forward them to shorvalu_fill_cpp
// after subtracting 1.
// ============================================================================
// [[Rcpp::export]]
List shorvalu_segment_cpp(NumericVector time_sec, double intervals_sec) {
  const R_xlen_t n = time_sec.size();
  if (n == 0) {
    return List::create(Named("starts") = IntegerVector(0),
                        Named("lens")   = IntegerVector(0));
  }

  const double* t = REAL(time_sec);

  std::vector<int> starts;
  std::vector<int> lens;
  starts.reserve((size_t)std::min<R_xlen_t>(n, (R_xlen_t)65536));
  lens.reserve((size_t)std::min<R_xlen_t>(n, (R_xlen_t)65536));

  int cur_start = 0;            // 0-based cursor
  for (R_xlen_t i = 1; i < n; ++i) {
    const double d = t[i] - t[i - 1];
    // Split at a new period (gap > threshold) or at a repeated timestamp.
    // Matches the original R condition exactly.
    if (d > intervals_sec || d == 0.0) {
      starts.push_back(cur_start + 1);            // store as 1-based
      lens.push_back((int)(i - cur_start));
      cur_start = (int)i;
    }
  }
  starts.push_back(cur_start + 1);
  lens.push_back((int)(n - cur_start));

  return List::create(Named("starts") = wrap(starts),
                      Named("lens")   = wrap(lens));
}
