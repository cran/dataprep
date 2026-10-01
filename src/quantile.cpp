// ==================== quantile.cpp ====================
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector quantile_cpp(NumericVector x, NumericVector probs) {
    std::vector<double> vals;
    vals.reserve(x.size());
    for (int i = 0; i < x.size(); ++i) {
        double v = x[i];
        if (!R_IsNA(v) && !R_IsNaN(v)) vals.push_back(v);
    }
    const int n = (int)vals.size();
    const int k = (int)probs.size();

    NumericVector res(k, NA_REAL);
    if (n == 0) return res;

    std::sort(vals.begin(), vals.end());

    for (int i = 0; i < k; ++i) {
        double p = probs[i];
        // FIX: validate the probability range. Out-of-range p would make
        // lo/hi go past the vector bounds and cause OOB reads.
        if (ISNAN(p) || p < 0.0 || p > 1.0) continue;
        if (n == 1) { res[i] = vals[0]; continue; }
        double index = (n - 1.0) * p;
        int lo = (int)std::floor(index);
        int hi = (int)std::ceil(index);
        double h = index - lo;
        if (lo == hi) res[i] = vals[lo];
        else          res[i] = vals[lo] + h * (vals[hi] - vals[lo]);
    }
    return res;
}
