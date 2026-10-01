// [[Rcpp::plugins(openmp)]]
// ==================== detect_outliers.cpp ====================
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

inline double quantile_inline(std::vector<double>& sorted, double p) {
    // 注意：此函数假定 sorted 已排序，用于保留旧接口
    int n = sorted.size();
    if (n == 0) return NA_REAL;
    double index = (n - 1.0) * p;
    int lo = (int)std::floor(index);
    int hi = (int)std::ceil(index);
    double h = index - lo;
    std::nth_element(sorted.begin(), sorted.begin() + lo, sorted.end());
    double vlo = sorted[lo];
    if (lo == hi) return vlo;
    std::nth_element(sorted.begin() + lo + 1, sorted.begin() + hi, sorted.end());
    double vhi = sorted[hi];
    return vlo + h * (vhi - vlo);
}

// 新增辅助：使用 nth_element 精确计算分位数
inline double quantile_nth_scratch(std::vector<double>& vals, double p) {
    int n = vals.size();
    if (n == 0) return NA_REAL;
    double index = (n - 1.0) * p;
    int lo = (int)std::floor(index);
    int hi = (int)std::ceil(index);
    double h = index - lo;
    std::nth_element(vals.begin(), vals.begin() + lo, vals.end());
    double vlo = vals[lo];
    if (lo == hi) return vlo;
    std::nth_element(vals.begin() + lo + 1, vals.begin() + hi, vals.end());
    double vhi = vals[hi];
    return vlo + h * (vhi - vlo);
}

// [[Rcpp::export]]
LogicalVector detect_outliers_cpp(NumericVector x, std::string method,
                                   double top, double bottom, double coef) {
    int n = x.size();
    LogicalVector mask(n, false);

    std::vector<double> vals;
    vals.reserve(n);
    for (int i = 0; i < n; ++i) {
        if (!R_IsNA(x[i])) vals.push_back(x[i]);
    }
    if (vals.empty()) return mask;

    if (method == "iqr") {
        double q1 = quantile_nth_scratch(vals, 0.25);
        double q3 = quantile_nth_scratch(vals, 0.75);
        double iqr = q3 - q1;
        double lower = q1 - coef * iqr;
        double upper = q3 + coef * iqr;
        for (int i = 0; i < n; ++i) {
            if (R_IsNA(x[i])) continue;
            mask[i] = (x[i] < lower || x[i] > upper);
        }
    } else if (method == "mad") {
        int m = vals.size();
        // 使用 nth_element 计算中位数
        std::nth_element(vals.begin(), vals.begin() + m/2, vals.end());
        double med = vals[m/2];
        if (m % 2 == 0) {
            // 偶数个：需要两个中间值的平均
            std::nth_element(vals.begin(), vals.begin() + m/2 - 1, vals.begin() + m/2);
            med = (med + vals[m/2 - 1]) / 2.0;
        }
        std::vector<double> dev;
        dev.reserve(m);
        for (double v : vals) dev.push_back(std::fabs(v - med));
        std::nth_element(dev.begin(), dev.begin() + m/2, dev.end());
        double mad = dev[m/2];
        if (m % 2 == 0) {
            std::nth_element(dev.begin(), dev.begin() + m/2 - 1, dev.begin() + m/2);
            mad = (mad + dev[m/2 - 1]) / 2.0;
        }
        if (mad == 0) return mask;
        for (int i = 0; i < n; ++i) {
            if (R_IsNA(x[i])) continue;
            double z = 0.6745 * (x[i] - med) / mad;
            mask[i] = (std::fabs(z) > coef);
        }
    } else if (method == "percentile") {
        double top_q = quantile_nth_scratch(vals, top);
        double bottom_q = quantile_nth_scratch(vals, bottom);
        for (int i = 0; i < n; ++i) {
            if (R_IsNA(x[i])) continue;
            mask[i] = (x[i] > top_q || x[i] < bottom_q);
        }
    }
    return mask;
}

// [[Rcpp::export]]
LogicalMatrix detect_outliers_matrix_cpp(NumericMatrix x, std::string method,
                                          double top, double bottom, double coef) {
    int n = x.nrow(), p = x.ncol();
    LogicalMatrix mask(n, p);

    #pragma omp parallel for schedule(dynamic) if(n * p > 100000)
    for (int j = 0; j < p; ++j) {
        std::vector<double> vals;
        vals.reserve(n);
        for (int i = 0; i < n; ++i) {
            double v = x(i, j);
            if (!R_IsNA(v)) vals.push_back(v);
        }
        if (vals.empty()) continue;

        if (method == "iqr") {
            double q1 = quantile_nth_scratch(vals, 0.25);
            double q3 = quantile_nth_scratch(vals, 0.75);
            double iqr = q3 - q1;
            double lower = q1 - coef * iqr;
            double upper = q3 + coef * iqr;
            for (int i = 0; i < n; ++i) {
                double v = x(i, j);
                if (R_IsNA(v)) continue;
                mask(i, j) = (v < lower || v > upper);
            }
        } else if (method == "mad") {
            int m = vals.size();
            std::nth_element(vals.begin(), vals.begin() + m/2, vals.end());
            double med = vals[m/2];
            if (m % 2 == 0) {
                std::nth_element(vals.begin(), vals.begin() + m/2 - 1, vals.begin() + m/2);
                med = (med + vals[m/2 - 1]) / 2.0;
            }
            std::vector<double> dev;
            dev.reserve(m);
            for (double v : vals) dev.push_back(std::fabs(v - med));
            std::nth_element(dev.begin(), dev.begin() + m/2, dev.end());
            double mad = dev[m/2];
            if (m % 2 == 0) {
                std::nth_element(dev.begin(), dev.begin() + m/2 - 1, dev.begin() + m/2);
                mad = (mad + dev[m/2 - 1]) / 2.0;
            }
            if (mad == 0) continue;
            for (int i = 0; i < n; ++i) {
                double v = x(i, j);
                if (R_IsNA(v)) continue;
                double z = 0.6745 * (v - med) / mad;
                mask(i, j) = (std::fabs(z) > coef);
            }
        } else if (method == "percentile") {
            double top_q = quantile_nth_scratch(vals, top);
            double bottom_q = quantile_nth_scratch(vals, bottom);
            for (int i = 0; i < n; ++i) {
                double v = x(i, j);
                if (R_IsNA(v)) continue;
                mask(i, j) = (v > top_q || v < bottom_q);
            }
        }
    }
    return mask;
}
