// File: src/log_returns.cpp
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector log_returns_cpp(NumericVector x) {
  int n = x.size();
  NumericVector res(n, NA_REAL);
  for (int i = 1; i < n; ++i) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(x[i-1]) &&
        x[i] > 0 && x[i-1] > 0) {
      res[i] = log(x[i] / x[i-1]);
    }
  }
  return res;
}
