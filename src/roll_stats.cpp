// ==================== roll_stats.cpp ====================
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <queue>
#include <map>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

class MedianFinder {
private:
    std::priority_queue<double> max_heap;
    std::priority_queue<double, std::vector<double>, std::greater<double>> min_heap;
    std::map<double, int> delayed;

    void clean_heaps() {
        while (!max_heap.empty()) {
            double top_val = max_heap.top();
            auto it = delayed.find(top_val);
            if (it == delayed.end() || it->second <= 0) break;
            it->second--;
            max_heap.pop();
        }
        while (!min_heap.empty()) {
            double top_val = min_heap.top();
            auto it = delayed.find(top_val);
            if (it == delayed.end() || it->second <= 0) break;
            it->second--;
            min_heap.pop();
        }
    }

    void balance() {
        clean_heaps();
        if (max_heap.size() > min_heap.size() + 1) {
            min_heap.push(max_heap.top());
            max_heap.pop();
        } else if (min_heap.size() > max_heap.size()) {
            max_heap.push(min_heap.top());
            min_heap.pop();
        }
        clean_heaps();
    }

public:
    void add(double x) {
        if (max_heap.empty() || x <= max_heap.top()) {
            max_heap.push(x);
        } else {
            min_heap.push(x);
        }
        balance();
    }

    void remove(double x) {
        delayed[x]++;
        balance();
    }

    double get_median() {
        clean_heaps();
        balance();
        size_t total = max_heap.size() + min_heap.size();
        if (total == 0) return NA_REAL;
        if (max_heap.size() == min_heap.size()) {
            return (max_heap.top() + min_heap.top()) / 2.0;
        } else {
            return max_heap.top();
        }
    }
};

// [[Rcpp::export]]
NumericVector roll_stats_cpp(NumericVector x, int window, std::string method) {
    const int n = x.size();
    NumericVector res(n, NA_REAL);
    if (window <= 0 || window > n) return res;

    if (method == "mean" || method == "sum" || method == "var" || method == "sd") {
        std::vector<double> sum_pre(n + 1, 0.0);
        std::vector<double> sum_sq_pre(n + 1, 0.0);
        std::vector<int>    cnt_pre(n + 1, 0);
        for (int i = 0; i < n; ++i) {
            double v = x[i];
            if (ISNAN(v)) {
                sum_pre[i + 1]    = sum_pre[i];
                sum_sq_pre[i + 1] = sum_sq_pre[i];
                cnt_pre[i + 1]    = cnt_pre[i];
            } else {
                sum_pre[i + 1]    = sum_pre[i] + v;
                sum_sq_pre[i + 1] = sum_sq_pre[i] + v * v;
                cnt_pre[i + 1]    = cnt_pre[i] + 1;
            }
        }
        for (int i = 0; i < n; ++i) {
            int start = std::max(0, i - window + 1);
            int win_cnt = cnt_pre[i + 1] - cnt_pre[start];
            if (win_cnt == 0) continue;
            double win_sum = sum_pre[i + 1] - sum_pre[start];
            if (method == "sum") {
                res[i] = win_sum;
            } else if (method == "mean") {
                res[i] = win_sum / win_cnt;
            } else {
                double win_sum_sq = sum_sq_pre[i + 1] - sum_sq_pre[start];
                double mean = win_sum / win_cnt;
                double var = (win_sum_sq - win_cnt * mean * mean) / (win_cnt - 1);
                if (method == "var") res[i] = var;
                else                 res[i] = std::sqrt(var);
            }
        }
    } else if (method == "min" || method == "max") {
        std::deque<int> dq;
        bool is_min = (method == "min");
        for (int i = 0; i < n; ++i) {
            while (!dq.empty() && dq.front() < i - window + 1) dq.pop_front();
            if (!ISNAN(x[i])) {
                if (is_min) {
                    while (!dq.empty() && x[i] <= x[dq.back()]) dq.pop_back();
                } else {
                    while (!dq.empty() && x[i] >= x[dq.back()]) dq.pop_back();
                }
                dq.push_back(i);
            }
            if (i >= window - 1) {
                if (!dq.empty()) res[i] = x[dq.front()];
            }
        }
        for (int i = 0; i < window - 1; ++i) {
            std::vector<double> win;
            win.reserve(i + 1);
            for (int j = 0; j <= i; ++j) {
                if (!ISNAN(x[j])) win.push_back(x[j]);
            }
            if (win.empty()) continue;
            if (is_min) res[i] = *std::min_element(win.begin(), win.end());
            else        res[i] = *std::max_element(win.begin(), win.end());
        }
    } else if (method == "median") {
        MedianFinder mf;
        std::deque<double> values;
        for (int i = 0; i < n; ++i) {
            double v = x[i];
            if (!ISNAN(v)) {
                mf.add(v);
                values.push_back(v);
            } else {
                values.push_back(NA_REAL);
            }
            if (i >= window) {
                double old = values.front();
                values.pop_front();
                if (!ISNAN(old)) {
                    mf.remove(old);
                }
            }
            if (i >= window - 1) {
                res[i] = mf.get_median();
            }
        }
    } else {
        stop("Unsupported rolling method");
    }
    return res;
}
