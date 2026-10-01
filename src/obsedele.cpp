// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>
#include <unordered_map>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// Thread-local scratch buffers reused across subsets and across calls.
static thread_local std::vector<int> tl_anchor_idx;
static thread_local std::vector<int> tl_sorted_sub;

// [[Rcpp::export]]
LogicalVector obsedele_cpp(NumericVector time_sec,
                           IntegerVector group_int,
                           NumericMatrix x,
                           double step_sec,
                           double half,
                           double threshold_sec,
                           int n_threads = 0) {
#ifdef _OPENMP
    if (n_threads > 0) omp_set_num_threads(n_threads);
#endif

    const int n = x.nrow();
    const int p = x.ncol();

    if (n == 0) return LogicalVector(0);
    if (p == 0) return LogicalVector(n, true);
    if (!std::isfinite(step_sec) || step_sec <= 0.0) {
        Rf_error("obsedele_cpp: step_sec must be positive and finite");
    }
    if (!std::isfinite(half) || half < 0.0) {
        Rf_error("obsedele_cpp: half must be non-negative and finite");
    }
    if (!std::isfinite(threshold_sec) || threshold_sec <= 0.0) {
        Rf_error("obsedele_cpp: threshold_sec must be positive and finite");
    }
    if (time_sec.size() != n || group_int.size() != n) {
        Rf_error("obsedele_cpp: time_sec and group_int must have length nrow(x)");
    }
    for (int i = 0; i < n; ++i) {
        if (ISNAN(time_sec[i])) {
            Rf_error("obsedele_cpp: time_sec contains NA/NaN at index %d", i + 1);
        }
        if (group_int[i] < 0) {
            Rf_error("obsedele_cpp: group_int must be non-negative at index %d",
                     i + 1);
        }
    }

    // Start from "delete all"; each subset explicitly keeps its own rows.
    std::vector<int> keep(n, 0);

    // ---- group mapping ----
    int G = 0;
    std::unordered_map<int, int> gid2idx;
    gid2idx.reserve((size_t)std::min(n, 1024));
    std::vector<int> remap(n, -1);
    for (int i = 0; i < n; ++i) {
        int g = group_int[i];
        if (g == 0) {
            remap[i] = -1;
        } else {
            auto it = gid2idx.find(g);
            if (it == gid2idx.end()) {
                gid2idx.emplace(g, G);
                remap[i] = G;
                ++G;
            } else {
                remap[i] = it->second;
            }
        }
    }

    std::vector<std::vector<int>> group_rows(G);
    if (G > 0) {
        std::vector<int> counts(G, 0);
        for (int i = 0; i < n; ++i)
            if (remap[i] >= 0) ++counts[remap[i]];
        for (int g = 0; g < G; ++g) group_rows[g].reserve(counts[g]);
        for (int i = 0; i < n; ++i)
            if (remap[i] >= 0) group_rows[remap[i]].push_back(i);
    }

    // 'half' is in minutes. Convert to seconds once.
    const double half_seconds = half * 60.0;

    // Anchor-based scan. For every NA observation and every column we
    // compute the time distance to the nearest non-NA anchor on each
    // side, and mark the observation as invalid only when BOTH distances
    // exceed half_seconds. If a side has no anchor at all (the run
    // touches the series boundary) that distance is +Inf, which is the
    // fix for the boundary under-deletion bug in 0.1.5.
    //
    // This replaces the previous grid-expansion algorithm. It avoids
    // allocating an O(L) grid per subset, where L is the number of
    // grid points rather than observations. For 10-minute sampling with
    // a 1-minute grid, L is ~10x the observation count.
    auto process_subset = [&](const std::vector<int>& sub_idx) -> void {
        int m = (int)sub_idx.size();
        if (m == 0) return;

        // Guarantee chronological order. Most inputs are already sorted,
        // so only allocate a copy when necessary.
        const std::vector<int>* sp = &sub_idx;
        bool already_sorted = true;
        for (int k = 1; k < m; ++k) {
            if (time_sec[sub_idx[k]] < time_sec[sub_idx[k - 1]]) {
                already_sorted = false;
                break;
            }
        }
        if (!already_sorted) {
            tl_sorted_sub.assign(sub_idx.begin(), sub_idx.end());
            std::sort(tl_sorted_sub.begin(), tl_sorted_sub.end(),
                      [&](int a, int b) {
                          return time_sec[a] < time_sec[b];
                      });
            sp = &tl_sorted_sub;
        }
        const std::vector<int>& sidx = *sp;

        // Default: keep every observation in this subset.
        for (int k = 0; k < m; ++k) keep[sidx[k]] = 1;

        std::vector<int>& anchor_idx = tl_anchor_idx;
        for (int j = 0; j < p; ++j) {
            const double* x_col = x.begin() + (R_xlen_t)j * n;

            // Positions (inside sidx) of non-NA values in this column.
            anchor_idx.clear();
            anchor_idx.reserve(m);
            for (int k = 0; k < m; ++k) {
                if (!R_IsNA(x_col[sidx[k]])) {
                    anchor_idx.push_back(k);
                }
            }
            if (anchor_idx.empty()) continue;   // no rule applies

            size_t a = 0;  // number of anchors encountered so far
            for (int k = 0; k < m; ++k) {
                int orig = sidx[k];
                if (!R_IsNA(x_col[orig])) { ++a; continue; }
                if (keep[orig] == 0) continue;  // already invalid

                double t_obs = time_sec[orig];
                double dl = (a == 0)
                    ? std::numeric_limits<double>::infinity()
                    : (t_obs - time_sec[sidx[anchor_idx[a - 1]]]);
                double dr = (a == anchor_idx.size())
                    ? std::numeric_limits<double>::infinity()
                    : (time_sec[sidx[anchor_idx[a]]] - t_obs);

                if (dl > half_seconds && dr > half_seconds) {
                    keep[orig] = 0;
                }
            }
        }
    };

    if (G == 0) {
        // No groups: split into periods by time gaps larger than
        // threshold_sec. Repeated timestamps stay in the same period.
        std::vector<int> period(n);
        period[0] = 0;
        for (int i = 1; i < n; ++i) {
            double diff = time_sec[i] - time_sec[i - 1];
            if (diff > threshold_sec)
                period[i] = period[i - 1] + 1;
            else
                period[i] = period[i - 1];
        }
        int num_periods = period[n - 1] + 1;
        std::vector<std::vector<int>> period_map(num_periods);
        for (int i = 0; i < n; ++i) period_map[period[i]].push_back(i);

        #pragma omp parallel for schedule(dynamic) if(num_periods > 1)
        for (int k = 0; k < num_periods; ++k)
            process_subset(period_map[k]);
    } else {
        #pragma omp parallel for schedule(dynamic) if(G > 1)
        for (int g = 0; g < G; ++g)
            process_subset(group_rows[g]);
    }

    LogicalVector result(n);
    for (int i = 0; i < n; ++i) result[i] = keep[i] != 0;
    return result;
}
