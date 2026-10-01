// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector filter_low_var_cpp(NumericMatrix x, double cutoff, 
                                 bool use_sd = false) {
    int n = x.nrow();
    int p = x.ncol();
    LogicalVector keep(p, true);
    
    #pragma omp parallel for schedule(dynamic) if(p > 50)
    for (int j = 0; j < p; ++j) {
        NumericVector col = x(_, j);
        double sum = 0.0;
        double sum_sq = 0.0;
        int cnt = 0;
        for (int i = 0; i < n; ++i) {
            if (!NumericVector::is_na(col[i])) {
                double v = col[i];
                sum += v;
                sum_sq += v * v;
                cnt++;
            }
        }
        if (cnt < 2) {
            keep[j] = false;
            continue;
        }
        double mean = sum / cnt;
        double var = (sum_sq - cnt * mean * mean) / (cnt - 1);
        if (var < 0) var = 0; // due to rounding
        double stat = use_sd ? sqrt(var) : var;
        if (stat <= cutoff) keep[j] = false;
    }
    return keep;
}
