// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace Rcpp;

inline bool same_value(double a, double b) {
    return (R_IsNA(a) && R_IsNA(b)) ||
           (!R_IsNA(a) && !R_IsNA(b) && std::fabs(a - b) < 1e-12);
}

static thread_local std::vector<double> scratch_vals;

inline double quantile_nth(std::vector<double>& vals, double p) {
    int n = (int)vals.size();
    if (n == 0) return NA_REAL;
    double index = (n - 1.0) * p;
    int lo = (int)std::floor(index);
    int hi = (int)std::ceil(index);
    double h = index - lo;
    std::nth_element(vals.begin(), vals.begin() + lo, vals.end());
    double vlo = vals[lo];
    if (lo == hi) return vlo;
    std::nth_element(vals.begin(), vals.begin() + hi, vals.end());
    double vhi = vals[hi];
    return vlo + h * (vhi - vlo);
}

inline void mark_outliers_inplace(double* y, int n,
                                  double top, double toperr, double topmag,
                                  double bottom, double boterr, double botmag,
                                  bool use_threshold_error) {
    scratch_vals.clear();
    scratch_vals.reserve(n);

    double min_val = std::numeric_limits<double>::max();
    double max_val = -std::numeric_limits<double>::max();

    for (int i = 0; i < n; ++i) {
        if (!ISNAN(y[i])) {
            const double v = y[i];
            scratch_vals.push_back(v);
            if (v < min_val) min_val = v;
            if (v > max_val) max_val = v;
        }
    }
    if (scratch_vals.empty()) return;

    double top_quant    = quantile_nth(scratch_vals, top);
    double bottom_quant = quantile_nth(scratch_vals, bottom);

    if (use_threshold_error) {
        // FIX: log10 is undefined for non-positive values. Guard both
        // top and bottom thresholds so that an unusual column with
        // non-positive quantiles does not produce NaN thresholds.
        double log_base_top = (top_quant > 0)
            ? std::pow(10.0, std::floor(std::log10(top_quant))) : 0.0;
        double log_base_bottom = (bottom_quant > 0)
            ? std::pow(10.0, std::floor(std::log10(bottom_quant))) : 0.0;

        double thresh_top = top_quant * (1 + toperr) + log_base_top * topmag;
        double thresh_bottom = bottom_quant * (1 - boterr) -
                               log_base_bottom * botmag;

        if (max_val > thresh_top) {
            for (int i = 0; i < n; ++i) {
                if (!ISNAN(y[i]) && same_value(y[i], max_val))
                    y[i] = NA_REAL;
            }
        }
        if (min_val < thresh_bottom) {
            for (int i = 0; i < n; ++i) {
                if (!ISNAN(y[i]) && same_value(y[i], min_val))
                    y[i] = NA_REAL;
            }
        }
    } else {
        for (int i = 0; i < n; ++i) {
            if (ISNAN(y[i])) continue;
            if (y[i] > top_quant || y[i] < bottom_quant) y[i] = NA_REAL;
        }
    }
}

// [[Rcpp::export]]
NumericVector mark_outliers_cpp(NumericVector x, double top, double toperr,
                                double topmag, double bottom, double boterr,
                                double botmag, bool use_threshold_error) {
    NumericVector y = clone(x);
    mark_outliers_inplace(REAL(y), y.size(), top, toperr, topmag,
                          bottom, boterr, botmag, use_threshold_error);
    return y;
}

// [[Rcpp::export]]
NumericMatrix mark_outliers_matrix_cpp(NumericMatrix mat,
                                       double top, double toperr,
                                       double topmag,
                                       double bottom, double boterr,
                                       double botmag,
                                       bool use_threshold_error,
                                       int n_threads = 0) {
    NumericMatrix res = clone(mat);
    int n = res.nrow();
    int p = res.ncol();

    #pragma omp parallel for schedule(dynamic) \
        if(n_threads > 1 && n * p > 100000)
    for (int j = 0; j < p; ++j) {
        double* col_ptr = res.begin() + (R_xlen_t)j * n;
        mark_outliers_inplace(col_ptr, n, top, toperr, topmag,
                              bottom, boterr, botmag, use_threshold_error);
    }
    return res;
}
