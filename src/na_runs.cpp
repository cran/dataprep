// File: src/na_runs.cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List na_runs_cpp(NumericVector x) {
  const int n = x.size();
  int n_na = 0, n_runs = 0, max_run = 0, current_run = 0;
  for (int i = 0; i < n; ++i) {
    double v = x[i];
    if (R_IsNA(v) || R_IsNaN(v)) {
      ++n_na;
      ++current_run;
      if (current_run == 1) ++n_runs;
      if (current_run > max_run) max_run = current_run;
    } else {
      current_run = 0;
    }
  }
  return List::create(
    Named("n_na")    = n_na,
    Named("n_runs")  = n_runs,
    Named("max_run") = max_run
  );
}
