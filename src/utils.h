#ifndef DATAPREP_UTILS_H
#define DATAPREP_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

inline bool same_value(double a, double b) {
    if (Rcpp::NumericVector::is_na(a) && Rcpp::NumericVector::is_na(b)) return true;
    if (Rcpp::NumericVector::is_na(a) || Rcpp::NumericVector::is_na(b)) return false;
    return std::fabs(a - b) < 1e-12;
}

inline double quantile7_vec(const std::vector<double>& sorted, double p) {
    int n = sorted.size();
    if (n == 0) return NA_REAL;
    if (n == 1) return sorted[0];
    double index = (n - 1.0) * p;
    int lo = static_cast<int>(std::floor(index));
    int hi = static_cast<int>(std::ceil(index));
    double h = index - lo;
    if (lo == hi) return sorted[lo];
    return sorted[lo] + h * (sorted[hi] - sorted[lo]);
}

#endif
