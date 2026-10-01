// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// Defined in mark_outliers.cpp (non-inline, external linkage).
void mark_outliers_inplace(double* y, int n,
                           double top, double toperr, double topmag,
                           double bottom, double boterr, double botmag,
                           bool use_threshold_error);

// Defined in obsedele.cpp.
LogicalVector obsedele_cpp(NumericVector time_sec,
                           IntegerVector group_int,
                           NumericMatrix x,
                           double step_sec,
                           double half,
                           double threshold_sec,
                           int n_threads);

// ============================================================================
// Mark outliers in `mat` `interval` times, using a single OpenMP region.
//
// The previous version launched one OpenMP parallel region per (round, rep)
// pair, i.e. `times * interval` launches (100 by default). Each launch
// costs 5-50 microseconds on a many-core host, and the total launch
// overhead is measurable on small matrices. This version folds the
// `interval` inner loop into one parallel region, so the number of
// parallel-region launches drops to `times`.
//
// Two task layouts are used:
//   * No grouping column: tasks are (column j), split over threads.
//   * With grouping:      tasks are (group, column) pairs, flattened into a
//                         single task list so every thread participates.
// ============================================================================
static void mark_matrix_inplace_n(NumericMatrix& mat,
                                  const IntegerVector& grp,
                                  double top, double toperr, double topmag,
                                  double bottom, double boterr, double botmag,
                                  int interval,
                                  int n_threads) {
    const int n = mat.nrow();
    const int p = mat.ncol();
    if (n == 0 || p == 0 || interval <= 0) return;

    bool has_group = false;
    for (int i = 0; i < n; ++i) {
        if (grp[i] != 0) { has_group = true; break; }
    }

#ifdef _OPENMP
    const int parallel_threshold = 200000;
    int use_threads = (n_threads > 0) ? n_threads : omp_get_max_threads();
    if (use_threads < 1) use_threads = 1;
#else
    const int parallel_threshold = 0;
    (void)parallel_threshold;
    (void)n_threads;
#endif

    // ---- no grouping: parallelise over columns only ------------------------
    if (!has_group) {
#ifdef _OPENMP
#pragma omp parallel num_threads(use_threads) \
    if(use_threads > 1 && n * p > parallel_threshold)
        {
            for (int rep = 0; rep < interval; ++rep) {
#pragma omp for schedule(dynamic) nowait
                for (int j = 0; j < p; ++j) {
                    double* col = mat.begin() + (R_xlen_t)j * n;
                    mark_outliers_inplace(col, n, top, toperr, topmag,
                                          bottom, boterr, botmag, true);
                }
            }
        }
#else
        for (int rep = 0; rep < interval; ++rep) {
            for (int j = 0; j < p; ++j) {
                double* col = mat.begin() + (R_xlen_t)j * n;
                mark_outliers_inplace(col, n, top, toperr, topmag,
                                      bottom, boterr, botmag, true);
            }
        }
#endif
        return;
    }

    // ---- grouped: flatten (group, column) pairs into a task list -----------
    std::unordered_map<int, std::vector<int>> group_rows;
    group_rows.reserve(64);
    for (int i = 0; i < n; ++i) group_rows[grp[i]].push_back(i);

    struct Task { const std::vector<int>* rows; int col; };
    std::vector<Task> tasks;
    tasks.reserve(group_rows.size() * (size_t)p);
    for (auto& kv : group_rows) {
        if (kv.second.empty()) continue;
        for (int j = 0; j < p; ++j) {
            tasks.push_back({&kv.second, j});
        }
    }
    const size_t n_tasks = tasks.size();
    if (n_tasks == 0) return;

#ifdef _OPENMP
#pragma omp parallel num_threads(use_threads) \
    if(use_threads > 1 && (R_xlen_t)n * p > parallel_threshold)
    {
        // One reusable buffer per thread.
        std::vector<double> col_vals;
        for (int rep = 0; rep < interval; ++rep) {
#pragma omp for schedule(dynamic) nowait
            for (size_t t = 0; t < n_tasks; ++t) {
                const Task& task = tasks[t];
                const std::vector<int>& rows = *task.rows;
                const int m = (int)rows.size();
                if (m == 0) continue;
                col_vals.resize((size_t)m);
                for (int r = 0; r < m; ++r)
                    col_vals[(size_t)r] = mat(rows[(size_t)r], task.col);
                mark_outliers_inplace(col_vals.data(), m, top, toperr, topmag,
                                      bottom, boterr, botmag, true);
                for (int r = 0; r < m; ++r)
                    mat(rows[(size_t)r], task.col) = col_vals[(size_t)r];
            }
        }
    }
#else
    {
        std::vector<double> col_vals;
        for (int rep = 0; rep < interval; ++rep) {
            for (size_t t = 0; t < n_tasks; ++t) {
                const Task& task = tasks[t];
                const std::vector<int>& rows = *task.rows;
                const int m = (int)rows.size();
                if (m == 0) continue;
                col_vals.resize((size_t)m);
                for (int r = 0; r < m; ++r)
                    col_vals[(size_t)r] = mat(rows[(size_t)r], task.col);
                mark_outliers_inplace(col_vals.data(), m, top, toperr, topmag,
                                      bottom, boterr, botmag, true);
                for (int r = 0; r < m; ++r)
                    mat(rows[(size_t)r], task.col) = col_vals[(size_t)r];
            }
        }
    }
#endif
}

static void shrink_inplace(NumericMatrix& mat,
                           NumericVector& time_sec,
                           IntegerVector& group_int,
                           std::vector<int>& orig_idx,
                           const LogicalVector& keep) {
    const int n = mat.nrow();
    const int p = mat.ncol();

    int new_n = 0;
    for (int i = 0; i < n; ++i) if (keep[i]) ++new_n;
    if (new_n == n) return;
    if (new_n == 0) {
        mat = NumericMatrix(0, p);
        time_sec = NumericVector(0);
        group_int = IntegerVector(0);
        orig_idx.clear();
        return;
    }

    NumericMatrix  new_mat(new_n, p);
    NumericVector  new_time(new_n);
    IntegerVector  new_group(new_n);
    std::vector<int> new_orig(new_n);

    for (int j = 0; j < p; ++j) {
        double* dst = new_mat.begin() + (R_xlen_t)j * new_n;
        const double* src = mat.begin() + (R_xlen_t)j * n;
        int pos = 0;
        for (int i = 0; i < n; ++i) {
            if (keep[i]) dst[pos++] = src[i];
        }
    }
    int pos = 0;
    for (int i = 0; i < n; ++i) {
        if (keep[i]) {
            new_time[pos]  = time_sec[i];
            new_group[pos] = group_int[i];
            new_orig[pos]  = orig_idx[i];
            ++pos;
        }
    }

    mat       = new_mat;
    time_sec  = new_time;
    group_int = new_group;
    orig_idx  = new_orig;
}

// [[Rcpp::export]]
List condextr_cpp(NumericVector time_sec,
                  IntegerVector group_int,
                  NumericMatrix x,
                  double step_sec,
                  double half,
                  double threshold_sec,
                  double top,    double toperr,   double topmag,
                  double bottom, double boterr,   double botmag,
                  int interval,  int times,
                  int n_threads = 0) {
#ifdef _OPENMP
    if (n_threads > 0) omp_set_num_threads(n_threads);
#endif

    const int n0 = x.nrow();
    if (n0 == 0) {
        return List::create(
            Named("keep")      = LogicalVector(0),
            Named("mat")       = NumericMatrix(0, x.ncol()),
            Named("time_sec")  = NumericVector(0),
            Named("group_int") = IntegerVector(0)
        );
    }

    NumericMatrix  cur_mat  = clone(x);
    NumericVector  cur_time = clone(time_sec);
    IntegerVector  cur_group = clone(group_int);
    std::vector<int> orig_idx(n0);
    for (int i = 0; i < n0; ++i) orig_idx[i] = i;

    for (int round = 0; round < times; ++round) {
        // Single OpenMP region for all `interval` marking passes.
        mark_matrix_inplace_n(cur_mat, cur_group,
                              top, toperr, topmag,
                              bottom, boterr, botmag,
                              interval, n_threads);

        LogicalVector keep = obsedele_cpp(cur_time, cur_group, cur_mat,
                                          step_sec, half, threshold_sec,
                                          n_threads);
        shrink_inplace(cur_mat, cur_time, cur_group, orig_idx, keep);
        if (cur_mat.nrow() == 0) break;
    }

    if (cur_mat.nrow() > 0) {
        LogicalVector keep = obsedele_cpp(cur_time, cur_group, cur_mat,
                                          step_sec, half, threshold_sec,
                                          n_threads);
        shrink_inplace(cur_mat, cur_time, cur_group, orig_idx, keep);
    }

    LogicalVector final_keep(n0, false);
    for (int k = 0; k < (int)orig_idx.size(); ++k) {
        final_keep[orig_idx[k]] = true;
    }

    return List::create(
        Named("keep")      = final_keep,
        Named("mat")       = cur_mat,
        Named("time_sec")  = cur_time,
        Named("group_int") = cur_group
    );
}
