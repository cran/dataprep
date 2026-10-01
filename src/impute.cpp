// File: src/impute.cpp
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector impute_cpp(NumericVector x, std::string method) {
  int n = x.size();
  NumericVector y = clone(x);

  if (method == "locf") {
    double last = NA_REAL;
    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(y[i])) last = y[i];
      else if (!R_IsNA(last)) y[i] = last;
    }
  } else if (method == "nocb") {
    double next_val = NA_REAL;
    for (int i = n-1; i >= 0; --i) {
      if (!R_IsNA(y[i])) next_val = y[i];
      else if (!R_IsNA(next_val)) y[i] = next_val;
    }
  } else if (method == "linear") {
    double last = NA_REAL;
    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(y[i])) last = y[i];
      else if (!R_IsNA(last)) y[i] = last;
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
  } else if (method == "mean") {
    double sum = 0.0;
    int cnt = 0;
    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(x[i])) { sum += x[i]; cnt++; }
    }
    if (cnt > 0) {
      double mean_val = sum / cnt;
      for (int i = 0; i < n; ++i) {
        if (R_IsNA(y[i])) y[i] = mean_val;
      }
    }
  } else if (method == "median") {
    std::vector<double> vals;
    vals.reserve(n);
    for (int i = 0; i < n; ++i) {
      if (!R_IsNA(x[i])) vals.push_back(x[i]);
    }
    if (!vals.empty()) {
      std::sort(vals.begin(), vals.end());
      int m = vals.size();
      double med = (m % 2 == 0) ?
        (vals[m/2 - 1] + vals[m/2]) / 2.0 : vals[m/2];
      for (int i = 0; i < n; ++i) {
        if (R_IsNA(y[i])) y[i] = med;
      }
    }
  }
  return y;
}

// Matrix version. The OpenMP plugin declaration is inherited from the
// top of the file; no need to repeat it here.
// [[Rcpp::export]]
NumericMatrix impute_matrix_cpp(NumericMatrix x, std::string method) {
  int n = x.nrow(), p = x.ncol();
  NumericMatrix y = clone(x);

  #pragma omp parallel for schedule(dynamic) if(n * p > 100000)
  for (int j = 0; j < p; ++j) {
    NumericVector col = y(_, j);

    if (method == "locf") {
      double last = NA_REAL;
      for (int i = 0; i < n; ++i) {
        if (!R_IsNA(col[i])) last = col[i];
        else if (!R_IsNA(last)) col[i] = last;
      }
    } else if (method == "nocb") {
      double next_val = NA_REAL;
      for (int i = n-1; i >= 0; --i) {
        if (!R_IsNA(col[i])) next_val = col[i];
        else if (!R_IsNA(next_val)) col[i] = next_val;
      }
    } else if (method == "linear") {
      double last = NA_REAL;
      for (int i = 0; i < n; ++i) {
        if (!R_IsNA(col[i])) last = col[i];
        else if (!R_IsNA(last)) col[i] = last;
      }
      for (int i = 0; i < n; ++i) {
        if (!R_IsNA(x(i,j)) || i==0 || i==n-1) continue;
        int jp = i-1;
        while (jp >= 0 && R_IsNA(x(jp,j))) jp--;
        int k = i+1;
        while (k < n && R_IsNA(x(k,j))) k++;
        if (jp >= 0 && k < n) {
          double slope = (x(k,j) - x(jp,j)) / (k - jp);
          col[i] = x(jp,j) + slope * (i - jp);
        }
      }
    } else if (method == "mean") {
      double sum = 0.0;
      int cnt = 0;
      for (int i = 0; i < n; ++i) {
        double v = x(i,j);
        if (!R_IsNA(v)) { sum += v; cnt++; }
      }
      if (cnt > 0) {
        double mean_val = sum / cnt;
        for (int i = 0; i < n; ++i) {
          if (R_IsNA(col[i])) col[i] = mean_val;
        }
      }
    } else if (method == "median") {
      std::vector<double> vals;
      vals.reserve(n);
      for (int i = 0; i < n; ++i) {
        double v = x(i,j);
        if (!R_IsNA(v)) vals.push_back(v);
      }
      if (!vals.empty()) {
        std::sort(vals.begin(), vals.end());
        int m = vals.size();
        double med = (m % 2 == 0) ?
          (vals[m/2 - 1] + vals[m/2]) / 2.0 : vals[m/2];
        for (int i = 0; i < n; ++i) {
          if (R_IsNA(col[i])) col[i] = med;
        }
      }
    }

    y(_, j) = col;
  }
  return y;
}
