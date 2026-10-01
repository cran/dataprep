#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include "utils.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector transform_scale_cpp(NumericVector x, std::string method) {
    int n = x.size();
    NumericVector y(n);
    std::vector<double> vals;
    vals.reserve(n);
    for (int i = 0; i < n; ++i) {
        if (!NumericVector::is_na(x[i])) vals.push_back(x[i]);
    }
    int m = vals.size();
    if (m == 0) {
        std::fill(y.begin(), y.end(), NA_REAL);
        return y;
    }

    if (method == "zscore") {
        double mean = 0.0;
        for (double v : vals) mean += v;
        mean /= m;
        // FIX: guard m == 1 so that sd is not sqrt(0 / 0).
        double sd = 0.0;
        if (m > 1) {
            for (double v : vals) sd += (v - mean) * (v - mean);
            sd = std::sqrt(sd / (m - 1));
        }
        if (sd == 0) {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : 0.0;
        } else {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : (x[i] - mean) / sd;
        }
    } else if (method == "center") {
        double mean = 0.0;
        for (double v : vals) mean += v;
        mean /= m;
        for (int i = 0; i < n; ++i)
            y[i] = NumericVector::is_na(x[i]) ? NA_REAL : x[i] - mean;
    } else if (method == "scale") {
        double mean = 0.0;
        for (double v : vals) mean += v;
        mean /= m;
        double sd = 0.0;
        if (m > 1) {
            for (double v : vals) sd += (v - mean) * (v - mean);
            sd = std::sqrt(sd / (m - 1));
        }
        if (sd == 0) {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : 1.0;
        } else {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : x[i] / sd;
        }
    } else if (method == "minmax") {
        double min_val = *std::min_element(vals.begin(), vals.end());
        double max_val = *std::max_element(vals.begin(), vals.end());
        double range = max_val - min_val;
        if (range == 0) {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : 0.5;
        } else {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : (x[i] - min_val) / range;
        }
    } else if (method == "robust") {
        std::vector<double> sorted = vals;
        std::sort(sorted.begin(), sorted.end());
        double med = quantile7_vec(sorted, 0.5);
        double q1  = quantile7_vec(sorted, 0.25);
        double q3  = quantile7_vec(sorted, 0.75);
        double iqr = q3 - q1;
        if (iqr == 0) {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : 0.0;
        } else {
            for (int i = 0; i < n; ++i)
                y[i] = NumericVector::is_na(x[i]) ? NA_REAL : (x[i] - med) / iqr;
        }
    } else {
        for (int i = 0; i < n; ++i) y[i] = x[i];
    }
    return y;
}
