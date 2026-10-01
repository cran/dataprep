// File: src/lin_interp.cpp
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector lin_interp_cpp(NumericVector x) {
  int n = x.size();
  NumericVector y = clone(x);

  double last = NA_REAL;
  for (int i = 0; i < n; ++i) {
    if (!R_IsNA(y[i])) last = y[i];
    else if (!R_IsNA(last)) y[i] = last;
  }
  double next_val = NA_REAL;
  for (int i = n-1; i >= 0; --i) {
    if (!R_IsNA(x[i])) next_val = x[i];
    else if (!R_IsNA(next_val) && R_IsNA(y[i])) y[i] = next_val;
  }
  for (int i = 0; i < n; ++i) {
    if (!R_IsNA(x[i]) || i==0 || i==n-1) continue;
    int j = i-1;
    while (j >= 0 && R_IsNA(x[j])) j--;
    int k = i+1;
    while (k < n && R_IsNA(x[k])) k++;
    if (j >= 0 && k < n) {
      double slope = (x[k] - x[j]) / (k - j);
      y[i] = x[j] + slope * (i - j);
    }
  }
  return y;
}

// Matrix version. The OpenMP plugin declaration is inherited from the
// top of the file; no need to repeat it here.
// [[Rcpp::export]]
NumericMatrix lin_interp_matrix_cpp(NumericMatrix x) {
  int n = x.nrow(), p = x.ncol();
  NumericMatrix y = clone(x);

  #pragma omp parallel for schedule(dynamic) if(n * p > 100000)
  for (int j = 0; j < p; ++j) {
    NumericVector col = y(_, j);
    NumericVector orig = x(_, j);

    double last = NA_REAL;
    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(col[i])) last = col[i];
      else if (!R_IsNA(last)) col[i] = last;
    }

    double next_val = NA_REAL;
    for (int i = n-1; i >= 0; --i) {
      if (!R_IsNA(orig[i])) next_val = orig[i];
      else if (!R_IsNA(next_val) && R_IsNA(col[i])) col[i] = next_val;
    }

    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(orig[i]) || i==0 || i==n-1) continue;
      int jp = i-1;
      while (jp >= 0 && R_IsNA(orig[jp])) jp--;
      int k = i+1;
      while (k < n && R_IsNA(orig[k])) k++;
      if (jp >= 0 && k < n) {
        double slope = (orig[k] - orig[jp]) / (k - jp);
        col[i] = orig[jp] + slope * (i - jp);
      }
    }

    y(_, j) = col;
  }
  return y;
}
