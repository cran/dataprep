#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector transform_elem_cpp(NumericVector x, std::string method,
                                 double lambda = 1.0) {
    int n = x.size();
    NumericVector y(n);
    for (int i = 0; i < n; ++i) {
        if (NumericVector::is_na(x[i])) {
            y[i] = NA_REAL;
            continue;
        }
        double v = x[i];
        if (method == "log") {
            y[i] = (v > 0) ? std::log(v) : NA_REAL;
        } else if (method == "log1p") {
            y[i] = std::log1p(v);
        } else if (method == "sqrt") {
            y[i] = (v >= 0) ? std::sqrt(v) : NA_REAL;
        } else if (method == "inverse") {
            y[i] = (v != 0) ? 1.0 / v : NA_REAL;
        } else if (method == "boxcox") {
            // FIX: standard Box-Cox defines lambda == 0 as log(v).
            if (v > 0) {
                if (std::fabs(lambda) < 1e-12) {
                    y[i] = std::log(v);
                } else {
                    y[i] = (std::pow(v, lambda) - 1.0) / lambda;
                }
            } else {
                y[i] = NA_REAL;
            }
        } else if (method == "yeojohnson") {
            if (v >= 0) {
                y[i] = (std::fabs(lambda) > 1e-12)
                    ? (std::pow(v + 1.0, lambda) - 1.0) / lambda
                    : std::log1p(v);
            } else {
                y[i] = (std::fabs(lambda - 2.0) > 1e-12)
                    ? -(std::pow(-v + 1.0, 2.0 - lambda) - 1.0) / (2.0 - lambda)
                    : -std::log1p(-v);
            }
        } else {
            y[i] = v;
        }
    }
    return y;
}
