#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector bin_data_cpp(NumericVector x, NumericVector breaks,
                           bool include_lowest = true) {
    const int n = x.size();
    IntegerVector out(n, NA_INTEGER);

    std::vector<double> b = Rcpp::as<std::vector<double>>(breaks);
    std::sort(b.begin(), b.end());
    b.erase(std::unique(b.begin(), b.end()), b.end());
    const int m = (int)b.size();
    if (m < 2) return out;

    for (int i = 0; i < n; ++i) {
        if (NumericVector::is_na(x[i])) continue;
        const double val = x[i];
        int idx = -1;
        if (include_lowest) {
            if (val < b[0] || val > b[m - 1]) continue;
            auto it = std::upper_bound(b.begin(), b.end(), val);
            idx = (int)std::distance(b.begin(), it) - 1;
        } else {
            if (val <= b[0] || val > b[m - 1]) continue;
            auto it = std::lower_bound(b.begin(), b.end(), val);
            idx = (int)std::distance(b.begin(), it) - 1;
        }
        if (idx >= 0 && idx < m - 1) out[i] = idx + 1;
    }
    return out;
}
