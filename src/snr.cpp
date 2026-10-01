// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector snr_cpp(NumericMatrix x) {
    int n = x.nrow();
    int p = x.ncol();
    NumericVector snr(p);

    #pragma omp parallel for schedule(dynamic) if(n * p > 100000)
    for (int j = 0; j < p; ++j) {
        double sum = 0.0, sum_sq = 0.0;
        int m = 0;
        for (int i = 0; i < n; ++i) {
            double v = x(i, j);
            if (!R_IsNA(v)) {
                sum += v;
                sum_sq += v * v;
                ++m;
            }
        }
        if (m < 2) { snr[j] = NA_REAL; continue; }
        double mean = sum / m;
        double var = (sum_sq - m * mean * mean) / (m - 1);
        if (var < 0) var = 0;   // FIX: numerical safety
        double sd = std::sqrt(var);
        snr[j] = mean / sd;
    }
    return snr;
}
