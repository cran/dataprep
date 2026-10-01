// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// FIX: pairwise-complete Pearson correlation.
// The previous version used mean()/sd() which skip NA, but the
// sum_xy loop did not skip NA. Any column pair containing at least
// one NA was therefore silently mis-evaluated. This version computes
// each correlation on the subset of rows where both columns are
// non-missing and non-NaN.
// [[Rcpp::export]]
LogicalVector filter_high_cor_cpp(NumericMatrix x, double cutoff,
                                  bool keep_first = true) {
    const int n = x.nrow();
    const int p = x.ncol();
    LogicalVector keep(p, true);
    if (p < 2) return keep;

    std::vector<std::vector<double>> cor(p, std::vector<double>(p, 0.0));
    std::vector<char> sd_zero(p, 0);

    #pragma omp parallel for schedule(dynamic) if(p > 50)
    for (int i = 0; i < p; ++i) {
        const double* xi = x.begin() + (R_xlen_t)i * n;

        int ci = 0;
        for (int k = 0; k < n; ++k) {
            double v = xi[k];
            if (!R_IsNA(v) && !R_IsNaN(v)) { ++ci; }
        }
        if (ci < 2) { sd_zero[i] = 1; continue; }

        for (int j = i + 1; j < p; ++j) {
            const double* xj = x.begin() + (R_xlen_t)j * n;
            int cnt = 0;
            double sxi = 0.0, sxj = 0.0;
            double sxixi = 0.0, sxjxj = 0.0, sxixj = 0.0;
            for (int k = 0; k < n; ++k) {
                double a = xi[k];
                double b = xj[k];
                if (R_IsNA(a) || R_IsNA(b) || R_IsNaN(a) || R_IsNaN(b)) continue;
                sxi += a; sxj += b;
                sxixi += a * a;
                sxjxj += b * b;
                sxixj += a * b;
                ++cnt;
            }
            if (cnt < 2) continue;
            double mi = sxi / cnt;
            double mj = sxj / cnt;
            double vi = (sxixi - cnt * mi * mi) / (cnt - 1);
            double vj = (sxjxj - cnt * mj * mj) / (cnt - 1);
            if (vi < 0) vi = 0;
            if (vj < 0) vj = 0;
            double sdi = std::sqrt(vi);
            double sdj = std::sqrt(vj);
            if (sdi == 0 || sdj == 0) continue;
            double cov = (sxixj - cnt * mi * mj) / (cnt - 1);
            cor[i][j] = cov / (sdi * sdj);
        }
    }

    for (int i = 0; i < p; ++i) if (sd_zero[i]) keep[i] = false;

    auto var_of = [&](const double* v) -> double {
        double s = 0.0, s2 = 0.0;
        int c = 0;
        for (int k = 0; k < n; ++k) {
            double a = v[k];
            if (R_IsNA(a) || R_IsNaN(a)) continue;
            s += a; s2 += a * a; ++c;
        }
        if (c < 2) return 0.0;
        double m = s / c;
        double vv = (s2 - c * m * m) / (c - 1);
        return vv < 0 ? 0.0 : vv;
    };

    for (int i = 0; i < p; ++i) {
        if (!keep[i]) continue;
        for (int j = i + 1; j < p; ++j) {
            if (!keep[j]) continue;
            if (std::fabs(cor[i][j]) > cutoff) {
                if (keep_first) {
                    keep[j] = false;
                } else {
                    const double* xi = x.begin() + (R_xlen_t)i * n;
                    const double* xj = x.begin() + (R_xlen_t)j * n;
                    if (var_of(xi) >= var_of(xj)) keep[j] = false;
                    else                          keep[i] = false;
                }
            }
        }
    }
    return keep;
}
