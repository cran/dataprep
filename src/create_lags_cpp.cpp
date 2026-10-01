// src/create_lags_cpp.cpp
#include <Rcpp.h>
#include <vector>
#include <unordered_map>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix create_lags_cpp(NumericMatrix x, IntegerVector group,
                              IntegerVector lags, bool fill_na = true) {
    int n = x.nrow();
    int p = x.ncol();
    int nl = lags.size();
    NumericMatrix out(n, p * nl);
    double na_val = NA_REAL;

    // If group is all zero, treat as a single group (id = 1)
    bool has_group = false;
    for (int i = 0; i < n; ++i) {
        if (group[i] != 0) { has_group = true; break; }
    }

    std::unordered_map<int, std::vector<int>> group_rows;
    if (!has_group) {
        std::vector<int> all_rows(n);
        for (int i = 0; i < n; ++i) all_rows[i] = i;
        group_rows[1] = all_rows;
    } else {
        for (int i = 0; i < n; ++i) {
            group_rows[group[i]].push_back(i);
        }
    }

    // For each group
    for (auto &kv : group_rows) {
        auto &rows = kv.second;
        int m = rows.size();
        if (m == 0) continue;

        // For each lag
        for (int kl = 0; kl < nl; ++kl) {
            int lag = lags[kl];
            // For each column
            for (int j = 0; j < p; ++j) {
                int out_col = j * nl + kl;
                if (lag > 0) {
                    // Lag: current row gets value from lag rows earlier
                    for (int i = 0; i < m; ++i) {
                        int src_idx = i - lag;
                        if (src_idx >= 0) {
                            out(rows[i], out_col) = x(rows[src_idx], j);
                        } else {
                            out(rows[i], out_col) = na_val;
                        }
                    }
                } else if (lag < 0) {
                    // Lead: current row gets value from future rows
                    int lead = -lag;
                    for (int i = 0; i < m; ++i) {
                        int src_idx = i + lead;
                        if (src_idx < m) {
                            out(rows[i], out_col) = x(rows[src_idx], j);
                        } else {
                            out(rows[i], out_col) = na_val;
                        }
                    }
                } else {
                    // lag = 0: copy same row
                    for (int i = 0; i < m; ++i) {
                        out(rows[i], out_col) = x(rows[i], j);
                    }
                }
            }
        }
    }
    return out;
}
