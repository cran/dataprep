// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector na_frac_cpp(NumericMatrix x) {
    int n = x.nrow();
    int p = x.ncol();
    NumericVector frac(p);

    #pragma omp parallel for schedule(static) if(n * p > 100000)
    for (int j = 0; j < p; ++j) {
        int na_count = 0;
        for (int i = 0; i < n; ++i) {
            double v = x(i, j);
            // FIX: also count NaN, not only NA.
            if (R_IsNA(v) || R_IsNaN(v)) ++na_count;
        }
        frac[j] = (double)na_count / n;
    }
    return frac;
}
