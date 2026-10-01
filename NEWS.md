# dataprep 0.1.8

## Upgrading from 0.1.5? Read this first

Three behaviour changes affect row and column counts. All three are
bug fixes, but each one changes the output on real data. If your
downstream analysis depends on exact row counts, read the
quantified comparison below before upgrading.

1. **`obsedele()` now scans each column independently.** The 0.1.5
   implementation collapsed all selected columns into one long
   vector before computing missing runs; this changed NA run
   boundaries and could both over-delete boundary rows and retain
   rows that should have been deleted. The 0.1.8 implementation
   scans each column independently: a row is deleted when
   *any* selected column has a missing run longer than `half`
   minutes on both sides.

2. **`half` is now always in minutes, and the boundary is
   inclusive.** In 0.1.5, `half` counted grid rows in units of
   `by`: with `by = "5 min", half = 30` the effective window was
   150 minutes. In 0.1.8, `half` is always in minutes, independent
   of `by`. Rows whose nearest anchor is exactly `half` minutes
   away are retained (`within half minutes` is a `<=` condition).

3. **`optisolu()` no longer crashes with `cores > 16`.** The 0.1.5
   `parallel::makeCluster()` path exhausted memory when the worker
   processes each received a full copy of the input. The 0.1.8
   implementation loads the package on each worker, exports the
   input data only once per worker, and runs each `(interval,
   times)` case in a separate task, so `cores = 64` and
   `cores = NULL` (automatic) are both safe.

### Quantified effect on a full-year dataset

On SMEAR I Varrio 2025 (49,422 rows × 61 numeric channels,
10-minute sampling), running the same pipeline with the same
parameters:

| Stage | 0.1.5 | 0.1.8 | Δ |
|---|---:|---:|---:|
| `varidele` | 25 columns deleted | 25 columns deleted | 0 |
| `obsedele` | 1,494 rows deleted | 1,496 rows deleted | +2 |
| `condextr` | 1,868 rows deleted | 1,863 rows deleted | −5 |
| `shorvalu` | 50,376 NAs filled | 50,387 NAs filled | +11 |
| `dataprep` final | 46,060 rows | 46,063 rows | **+3** |

Net change: 0.006% of the input. The six rows that differ
between versions all sit at run boundaries where the anchor
distance is within one sampling interval of `half` minutes.

See `vignette("dataprep-migration")` for the full upgrade guide
and minimal reproductions of both changes.

## Test environments

Two reference hosts were used. Their relative ranking of the
engines is identical; the absolute multipliers scale with the
hardware.

### Reference host A — Ubuntu 25.10

| Component | Value |
|---|---|
| OS | Ubuntu 25.10 (Questing Quokka), kernel 6.17.0-41-generic |
| CPU | 2× AMD EPYC 9965 192-Core Processor (Turin, Zen 5c) |
| Physical cores | 384 (2 × 192) |
| Logical cores | 768 (SMT-2) |
| L1d / L1i | 18 MiB / 12 MiB |
| L2 | 384 MiB |
| L3 | 768 MiB |
| NUMA nodes | 2 |
| RAM | 1.0 TiB (16 × 64 GiB Micron, DDR5-5600, Multi-bit ECC) |
| Max frequency | 3.70 GHz |
| AVX-512 | Full (f, dq, ifma, cd, bw, vl, vbmi, vbmi2, vnni, bitalg, vpopcntdq, bf16) |
| R | 4.5.1 (2025-06-13) |
| Compiler | g++ 15.2.0 |
| reticulate | 1.47.0 |
| data.table | 1.18.6.1 |
| reshape2 | 1.4.5 |
| tidyr | 1.3.2 |
| Python | 3.13.7 |
| pandas | 3.0.6 |
| polars | 1.44.2 (runtime rt64) |
| dask | 2026.8.0 |
| duckdb | 1.5.5 |

### Reference host B — Windows 11 Pro for Workstations

| Component | Value |
|---|---|
| OS | Windows 11 Pro for Workstations, 10.0.26100, Build 26100 |
| CPU | 2× AMD EPYC 7B12 64-Core Processor |
| Physical cores | 128 (2 × 64) |
| Logical cores | 128 (no SMT) |
| L1d / L1i | 4 MiB / 4 MiB |
| L2 | 64 MiB |
| L3 | 512 MiB |
| NUMA nodes | 2 |
| RAM | about 224 GiB (7 × 32 GiB, 2933 MT/s, Micron / Samsung, non-ECC) |
| Max frequency | 2.25 GHz |
| AVX | AVX, AVX2 (no AVX-512) |
| R | 4.6.1 (2026-06-24 ucrt) |
| Compiler | GCC 14.3.0 |
| reticulate | 1.47.0 |
| data.table | 1.18.6.1 |
| reshape2 | 1.4.5 |
| tidyr | 1.3.2 |
| Python | 3.13.15 |
| pandas | 3.0.6 |
| polars | 1.44.2 (runtime rt64) |
| dask | 2026.8.0 |
| duckdb | 1.5.5 |

## Performance summary

### Cleaning pipeline (dataprep 0.1.5 → 0.1.8)

Speedup relative to 0.1.5 on the same input, same parameters.
Values below 1.0× mean the new implementation is marginally
slower on that cell.

| Function | 500 rows | 7,640 rows | 49,422 rows (Ubuntu) |
|---|---:|---:|---:|
| `varidele` | 1.2× | 1.1× | 11.6× |
| `obsedele` | 203× | 424× | 232× |
| `condextr` | 196× | 217× | 1146× |
| `optisolu` | 188× | 77× | 109× |
| `dataprep` | 185× | 228× | 247× |

On Windows 11 Pro for Workstations, the same full-year pipeline
gives `obsedele` ≈ 648×, `condextr` ≈ 839×, `shorvalu` ≈ 81×,
`optisolu` ≈ 25× (at `cores = 32`), and the integrated `dataprep`
call ≈ 173×. `varidele` is around 1.17× on this cell; this is
expected, since `varidele` is a single `colMeans(is.na(.))` in
both versions and the new code path has little room for improvement.

> **Note on `optisolu` cores.** The 0.1.5 implementation could
> crash when `cores > 16`. The benchmark above used
> `cores = 16` for both versions to keep the comparison fair.
> 0.1.8 loads the package on each worker, exports the input data
> once per worker, and runs each `(interval, times)` case as a
> separate task, so `cores = 64` is safe. The practical speed-up
> on a many-core host is **larger** than the table above.

### `melt()` — speed-up vs every one of the 7 major alternatives

Speed-ups relative to each competitor span **0.6×–1628.9×**
across both hosts. The sub-1.0× cells are concentrated at 1e7 rows
with 10 id columns (Ubuntu) and 1e5 rows with 10 id columns
(Windows), where `polars` is faster than `dataprep`; every other
cell has `dataprep` ahead of or on par with the fastest competitor.

Means in milliseconds (Ubuntu 25.10):

| rows | val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---|---|---|---|---|---|---|---|---|---|
| 1e3 | 9 | 0.173 | 0.378 (2.2×) | 0.257 (1.5×) | 2.788 (16.1×) | 2.128 (12.3×) | 0.645 (3.7×) | 15.786 (91.3×) | 4.489 (26.0×) |
| 1e6 | 9 | 3.474 | 18.797 (5.4×) | 9.646 (2.8×) | 80.014 (23.0×) | 61.686 (17.8×) | 14.671 (4.2×) | 47.584 (13.7×) | 648.895 (186.8×) |
| 1e7 | 9 | 33.412 | 364.564 (10.9×) | 365.065 (10.9×) | 1083.987 (32.4×) | 710.806 (21.3×) | 139.959 (4.2×) | 482.693 (14.4×) | 6389.564 (191.2×) |
| 1e8 | 9 | 276.295 | 3579.061 (13.0×) | 3571.833 (12.9×) | 12126.897 (43.9×) | 7463.089 (27.0×) | 3121.344 (11.3×) | 4756.547 (17.2×) | 71947.305 (260.4×) |
| 1e3 | 10000 | 2.295 | 93.092 (40.6×) | 12.415 (5.4×) | 104.924 (45.7×) | 495.659 (216.0×) | 19.408 (8.5×) | 3737.866 (1628.9×) | 1919.247 (836.4×) |
Means in milliseconds (Windows 11 Pro for Workstations):

| rows | val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---|---|---|---|---|---|---|---|---|---|
| 1e3 | 9 | 0.286 | 0.647 (2.3×) | 0.468 (1.6×) | 4.048 (14.2×) | 3.365 (11.8×) | 0.528 (1.8×) | 28.149 (98.4×) | 8.201 (28.7×) |
| 1e6 | 9 | 10.515 | 26.197 (2.5×) | 24.209 (2.3×) | 160.785 (15.3×) | 214.683 (20.4×) | 21.721 (2.1×) | 181.218 (17.2×) | 1537.791 (146.2×) |
| 1e7 | 9 | 77.008 | 245.861 (3.2×) | 247.910 (3.2×) | 1561.960 (20.3×) | 1925.931 (25.0×) | 275.120 (3.6×) | 1499.583 (19.5×) | 14517.337 (188.5×) |
| 1e8 | 9 | 935.148 | 2636.186 (2.8×) | 2534.491 (2.7×) | 16599.644 (17.8×) | 19578.283 (20.9×) | 4263.390 (4.6×) | 14714.823 (15.7×) | 148009.529 (158.3×) |
| 1e3 | 10000 | 11.569 | 161.647 (14.0×) | 34.383 (3.0×) | 203.580 (17.6×) | 1410.476 (121.9×) | 36.412 (3.1×) | 10333.080 (893.2×) | 5310.694 (459.0×) |
The **median** speed-up across all melt cells and all competitors
is 11.3× on Ubuntu and 5.6× on Windows. The **mean** is 67.8× and
46.6× respectively. The median is pulled down by the 1e5-row small
tables; at larger scales the speed-up is much higher.

### `dcast()` — speed-up vs every one of the 7 major alternatives

Speed-ups relative to each competitor span **1.9×–799.8×**
across both hosts. Every cell has `dataprep` ahead of every other
engine.

Means in milliseconds (Ubuntu 25.10):

| n_long | levels | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---|---|---|---|---|---|---|---|---|---|
| 1e6 | 10 | 1.654 | 151.826 (91.8×) | 328.980 (198.9×) | 44.728 (27.0×) | 56.741 (34.3×) | 105.018 (63.5×) | 78.217 (47.3×) | 155.880 (94.3×) |
| 1e6 | 100 | 1.415 | 101.438 (71.7×) | 329.202 (232.6×) | 42.047 (29.7×) | 53.686 (37.9×) | 173.478 (122.6×) | 74.540 (52.7×) | 178.421 (126.0×) |
| 1e7 | 100 | 5.073 | 2016.036 (397.4×) | 575.604 (113.5×) | 660.455 (130.2×) | 816.883 (161.0×) | 506.557 (99.9×) | 1002.368 (197.6×) | 1669.419 (329.1×) |
| 1e8 | 100 | 40.680 | 16963.494 (417.0×) | 18866.887 (463.8×) | 8100.475 (199.1×) | 9529.877 (234.3×) | 2460.625 (60.5×) | 12764.776 (313.8×) | 17616.392 (433.1×) |
Means in milliseconds (Windows 11 Pro for Workstations):

| n_long | levels | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---|---|---|---|---|---|---|---|---|---|
| 1e6 | 10 | 3.426 | 281.533 (82.2×) | 148.638 (43.4×) | 93.290 (27.2×) | 251.508 (73.4×) | 44.604 (13.0×) | 346.422 (101.1×) | 391.237 (114.2×) |
| 1e6 | 100 | 4.049 | 173.399 (42.8×) | 169.140 (41.8×) | 91.050 (22.5×) | 229.426 (56.7×) | 56.026 (13.8×) | 332.823 (82.2×) | 800.504 (197.7×) |
| 1e7 | 100 | 38.527 | 3197.642 (83.0×) | 1100.505 (28.6×) | 1359.999 (35.3×) | 2337.317 (60.7×) | 814.529 (21.1×) | 3014.871 (78.3×) | 7495.310 (194.5×) |
| 1e8 | 100 | 107.287 | 23690.584 (220.8×) | 14715.772 (137.2×) | 14245.599 (132.8×) | 25843.625 (240.9×) | 6222.505 (58.0×) | 33050.665 (308.1×) | 85804.834 (799.8×) |
The **median** speed-up across all dcast cells and all competitors
is 46.5× on Ubuntu and 41.4× on Windows. The **mean** is 90.2× and
76.6× respectively. On the 1e8-row cells (8 GB of input),
`dataprep` completes in **40–214 ms** while several competitors
exceed 12 s on Ubuntu and 12 s on Windows.

### Cross-engine consistency

`melt()` and `dcast()` produce output **numerically identical** to
`reshape2`, `data.table`, `tidyr`, `pandas`, `polars`, `dask`,
and `duckdb` on every tested shape, within `tol = 1e-12`.

| Operation | Cells tested | Engines | Pairwise |
|---|---:|---:|---|
| `melt` | 4 shapes | 8 | all consistent |
| `dcast` | 4 shapes | 8 | all consistent |

Full tables — including `mean`, `median`, and the full
per-competitor gradient — are in
`vignette("dataprep-performance")`. The reproducible runner is
shipped under `inst/`.

## Behaviour changes

### `obsedele()` semantics

The 0.1.8 C++ backend (`obsedele_cpp`) implements the retention
criterion with an **anchor-based scan**: for each missing value and
each selected column, the time distance to the nearest non-missing
anchor on the left and on the right is computed directly. A row is
deleted when **any** selected column has **both** distances exceeding `half` minutes.

This is mathematically identical to the running-mean criterion used
in 0.1.0 and the `rleid`-based criterion used in 0.1.5, but:

* **Scans each column independently.** The 0.1.5 implementation
  merged columns before computing runs, which changed run
  boundaries and could both over-delete and under-delete boundary
  rows.
* **Runs in O(n) time with O(1) extra allocation per column.**
  No grid materialisation, no run-length state.
* **Parallelises over columns with OpenMP** without any shared
  mutable state.

### `optisolu()` multi-core safety

The 0.1.5 `parallel::makeCluster()` path gave each worker a full
copy of the input. With `cores > 16` and large data this exhausted
memory and aborted the R session. The 0.1.8 implementation shares
read-only data across workers and accepts up to 64 cores safely.

### `melt()` new `major` and `as.factor` arguments

The `major` argument is now honoured strictly.  `NULL` (default) is
equivalent to `"col"`: column-major, identical to `reshape2::melt`.
`major = "row"` produces tidyr-compatible row ordering.  The earlier
implementation could switch automatically based on input shape, and
the tiny fast path silently ignored `major`; both are fixed.  There
is no longer any automatic switching.

A new `as.factor` argument controls the type of the `variable`
column.  `NULL` (default) uses `TRUE` for `major = "col"` and
`FALSE` for `major = "row"`.  Explicit `TRUE` / `FALSE` overrides
that default.  The return value is always a plain `data.frame`; no
`tibble` attributes are attached.

### `prep_fit()` / `prep_transform()` degenerate columns

A constant training column has `sd = 0`, `IQR = 0`, or
`max - min = 0`. `prep_fit()` now stores `1` as the scale value
for such columns, so the transform becomes `x - center`.
`prep_transform()` additionally guards against `scale_val == 0`
in case a plan is edited by hand.

## Bug fixes

* **`dcast()`: `na.rm = TRUE` combined with an explicit non-`NA`
  `fill` no longer loses the `fill` value.** On the block-path
  (canonical melt output), the tile transpose wrote the input
  values unconditionally after the fill pass, so cells whose
  input was `NA`/`NaN` ended up as `NA` instead of the requested
  `fill`. The transpose kernels now receive `na_rm` and the
  resolved `fill_val` and substitute them while the tile is
  built. The general path was not affected; both paths now
  produce identical output. Repro:
  `dcast(data.frame(id=c(1,1,2,2), variable=c("x","y","x","y"),
  value=c(1,NA,3,4)), id="id", variable="variable",
  value="value", na.rm=TRUE, fill=-1)` now returns
  `(1,"y") = -1`.

* **`dcast()`: duplicate `(id, variable)` pairs now resolve
  consistently with the documented "last occurrence wins"
  rule.** The block path previously kept the *first*
  occurrence of a duplicated block; the general path kept the
  *last*. The block path now keeps the last occurrence, matching
  `dcast()`'s documentation and `reshape2::dcast()`. This only
  affects non-canonical inputs (canonical `melt()` output has
  no duplicates); canonical round-trips are unchanged.

## New functions

### Cleaning

* [`balance_panel()`](https://chunshengliang.github.io/dataprep/reference/balance_panel.html) — balance an
  unbalanced panel by filling or completing.
* [`bin_data()`](https://chunshengliang.github.io/dataprep/reference/bin_data.html) — discretize continuous
  variables.
* [`clean_strings()`](https://chunshengliang.github.io/dataprep/reference/clean_strings.html) — trim /
  case / regex cleaning of character columns.
* [`deduplicate()`](https://chunshengliang.github.io/dataprep/reference/deduplicate.html) — exact and fuzzy
  duplicate removal.
* [`encode_categorical()`](https://chunshengliang.github.io/dataprep/reference/encode_categorical.html) —
  label / frequency / one-hot encoding.
* [`filter_high_cor()`](https://chunshengliang.github.io/dataprep/reference/filter_high_cor.html) — drop
  highly correlated variables.
* [`filter_low_var()`](https://chunshengliang.github.io/dataprep/reference/filter_low_var.html) — drop
  near-constant variables.
* [`phys_filter()`](https://chunshengliang.github.io/dataprep/reference/phys_filter.html) — physical range
  filtering.
* [`validate_data()`](https://chunshengliang.github.io/dataprep/reference/validate_data.html) — rule-based
  data validation.
* [`winsorize()`](https://chunshengliang.github.io/dataprep/reference/winsorize.html) — cap extreme values.
* [`zerona()`](https://chunshengliang.github.io/dataprep/reference/zerona.html) — replace zeros with NA.

### Imputation and transformation

* [`impute_missing()`](https://chunshengliang.github.io/dataprep/reference/impute_missing.html) — linear /
  LOCF / NOCB / mean / median.
* [`log_returns()`](https://chunshengliang.github.io/dataprep/reference/log_returns.html) — log returns.
* [`transform_data()`](https://chunshengliang.github.io/dataprep/reference/transform_data.html) — log /
  sqrt / Box-Cox / Yeo-Johnson and z-score / min-max / robust
  scaling.

### Diagnostics and reporting

* [`na_diagnose()`](https://chunshengliang.github.io/dataprep/reference/na_diagnose.html) — missing-value
  run statistics.
* [`data_report()`](https://chunshengliang.github.io/dataprep/reference/data_report.html) — compact data
  quality report.
* [`dry_run()`](https://chunshengliang.github.io/dataprep/reference/dry_run.html) — simulate preprocessing
  without changing data.

### Time series

* [`create_lags()`](https://chunshengliang.github.io/dataprep/reference/create_lags.html) — grouped lag /
  lead columns.
* [`day_night_flag()`](https://chunshengliang.github.io/dataprep/reference/day_night_flag.html) — day /
  night indicator.
* [`season_flag()`](https://chunshengliang.github.io/dataprep/reference/season_flag.html) — season / month
  / quarter indicator.
* [`decompose_ts()`](https://chunshengliang.github.io/dataprep/reference/decompose_ts.html) — additive /
  multiplicative decomposition.
* [`detrend_ts()`](https://chunshengliang.github.io/dataprep/reference/detrend_ts.html) — linear detrending.
* [`remove_diurnal_cycle()`](https://chunshengliang.github.io/dataprep/reference/remove_diurnal_cycle.html) —
  subtract mean diurnal cycle.
* [`resample_time()`](https://chunshengliang.github.io/dataprep/reference/resample_time.html) — resample to
  coarser period.
* [`roll_apply()`](https://chunshengliang.github.io/dataprep/reference/roll_apply.html) — rolling statistics
  with alignment.
* [`drift_detect()`](https://chunshengliang.github.io/dataprep/reference/drift_detect.html) — rolling drift
  detection.

### Sampling and workflow

* [`sample_data()`](https://chunshengliang.github.io/dataprep/reference/sample_data.html) — simple and
  stratified sampling.
* [`prep_fit()`](https://chunshengliang.github.io/dataprep/reference/prep_fit.html) /
  [`prep_transform()`](https://chunshengliang.github.io/dataprep/reference/prep_transform.html) — fit /
  transform style preprocessing plan that prevents data leakage.

### Reshaping

* [`dcast()`](https://chunshengliang.github.io/dataprep/reference/dcast.html) — long-to-wide reshaping,
  paired with [`melt()`](https://chunshengliang.github.io/dataprep/reference/melt.html).

## API changes

* Argument `cols` now consistently accepts names, integer
  indices, or logical masks across the package.
* All exported functions gain a `verbose = FALSE` argument
  that controls progress and timing messages.
* Functions that operate on a time column accept `date_col =
  NULL`; when `NULL`, the first column matching `date`, `Date`,
  or `DATE` is used.
* [`melt()`](https://chunshengliang.github.io/dataprep/reference/melt.html) gains `id.vars`,
  `measure.vars`, `variable.name`, `value.name`, `na.rm`,
  `cores`, `major`, `as.factor`, `verbose`, `parallel_threshold`.
  `id.vars` is an alias of `id` for reshape2 / data.table
  compatibility.
* [`dcast()`](https://chunshengliang.github.io/dataprep/reference/dcast.html) ships a `formula`
  interface (`id1 + id2 ~ variable`), `value.var` as an alias
  of `value`, and `fun.aggregate` for reducing duplicate
  `(id, variable)` pairs, plus `fill`, `na.rm`, `cores`, and
  `verbose`.
* [`dataprep()`](https://chunshengliang.github.io/dataprep/reference/dataprep.html),
  [`shorvalu()`](https://chunshengliang.github.io/dataprep/reference/shorvalu.html),
  [`descdata()`](https://chunshengliang.github.io/dataprep/reference/descdata.html),
  [`melt()`](https://chunshengliang.github.io/dataprep/reference/melt.html),
  [`dcast()`](https://chunshengliang.github.io/dataprep/reference/dcast.html),
  [`prep_fit()`](https://chunshengliang.github.io/dataprep/reference/prep_fit.html),
  and [`prep_transform()`](https://chunshengliang.github.io/dataprep/reference/prep_transform.html)
  gain a `cores` argument for OpenMP control (the cleaning functions
  [`obsedele()`](https://chunshengliang.github.io/dataprep/reference/obsedele.html),
  [`condextr()`](https://chunshengliang.github.io/dataprep/reference/condextr.html),
  [`percoutl()`](https://chunshengliang.github.io/dataprep/reference/percoutl.html),
  and [`optisolu()`](https://chunshengliang.github.io/dataprep/reference/optisolu.html)
  already accepted `cores` since 0.1.5; their backends now route it
  to OpenMP as well). The global option
  `options(dataprep.cores = ...)` is respected by
  [`melt()`](https://chunshengliang.github.io/dataprep/reference/melt.html)
  and [`dcast()`](https://chunshengliang.github.io/dataprep/reference/dcast.html).
* [`descdata()`](https://chunshengliang.github.io/dataprep/reference/descdata.html) now accepts `stats`
  as either numeric indices or character names.
* [`percplot()`](https://chunshengliang.github.io/dataprep/reference/percplot.html) now prints both the
  sample size (`n`) and the number of missing values (`na`) in
  each facet when a grouping column is supplied.
* [`descplot()`](https://chunshengliang.github.io/dataprep/reference/descplot.html) and
  [`percplot()`](https://chunshengliang.github.io/dataprep/reference/percplot.html) gain a `num_xaxis`
  argument that overrides the automatic choice between log and
  linear x-axis scales when column names are numeric.

## Documentation

Seven vignettes ship with the package:

* `vignette("dataprep-philosophy")` — design philosophy and
  preprocessing methodology.
* `vignette("dataprep-cleaning")` — step-by-step walkthrough of
  the four cleaning steps.
* `vignette("dataprep-performance")` — full benchmark tables and
  8-engine consistency checks.
* `vignette("dataprep-migration")` — 0.1.5 → 0.1.8 upgrade guide.
* `vignette("dataprep-workflow")` — leakage-free preprocessing
  with `prep_fit()` / `prep_transform()`.
* `vignette("dataprep-melt-dcast")` — fast reshaping usage and
  implementation notes.
* `vignette("dataprep-plots")` — descriptive statistics and
  diagnostic plots.

## Performance notes

Three benchmark cells sit close to, or marginally behind, the
fastest competitor — `dcast()` at 100 id columns, `melt()` at
1e7 rows × 10 id columns, and `varidele()` on full-year data — and
all three share the same root cause: the affected internal buffers
(the 96-bit fingerprint table, the per-id-column id blocks, and the
`is.na` scratch matrix) are allocated with `Rf_allocVector`, which
routes through R's default `malloc`-based allocator and therefore
cannot be directed to hugepages, a per-process free pool, or a
NUMA-aware arena, so first-touch page faults dominate those cells.
Two possible fixes were prototyped on the reshaping backends and
both were measured to close the gap: routing those buffers through
`Rf_allocVector3` with a hypothetical custom `R_allocator_t` adds a
further few-fold speed-up on top of the existing hundred-fold to
thousand-fold margins, and returning **ALTREP** virtual objects from
`melt()` / `dcast()` — a lazy `variable` column and a deferred id
block, so that large parts of the output are never materialised —
adds another order of magnitude (tens of times) on the wide-table
shapes. Neither is used in this release. `Rf_allocVector3` is not
recommended by CRAN, and an ALTREP return value, while fast to
produce, is slow for downstream complex statistics because every
element access re-enters the virtual-object layer, which shifts the
cost from `dataprep` to the caller's analysis code. 

The shipped 0.1.8 backends therefore keep the standard allocation path, and the
reported speed-ups stand as measured: `melt()` spans 0.6×–1628.9×
and `dcast()` spans 1.9×–799.8× across the two reference hosts, with
the sub-1.0× `melt()` cells confined to the 1e7-row (Ubuntu) and
1e5-row (Windows) shapes with 10 id columns, where `polars` is faster
than `dataprep`. On the cleaning pipeline the
0.1.5 → 0.1.8 speed-ups of 1.1×–1146× stand as reported;
`Rf_allocVector3` and ALTREP were not evaluated there.

## Environment variables (optional)

* `DATAPREP_RUN_BENCHMARK` = `1` enables the shipped benchmark
  scripts. They are disabled by default so that `R CMD check`
  does not execute them.

## See also

* [Design philosophy](https://chunshengliang.github.io/dataprep/articles/dataprep-philosophy.html) — why the
  pipeline has the shape it does.
* [Cleaning pipeline](https://chunshengliang.github.io/dataprep/articles/dataprep-cleaning.html) — full
  walkthrough of `varidele` / `obsedele` / `condextr` /
  `shorvalu`.
* [Performance](https://chunshengliang.github.io/dataprep/articles/dataprep-performance.html) — benchmark
  tables and cross-engine consistency.
* [Migration](https://chunshengliang.github.io/dataprep/articles/dataprep-migration.html) — upgrade guide.
* [Leakage-free workflow](https://chunshengliang.github.io/dataprep/articles/dataprep-workflow.html) —
  `prep_fit` / `prep_transform`.
* [Reshaping](https://chunshengliang.github.io/dataprep/articles/dataprep-melt-dcast.html) — melt / dcast.
* [Descriptive statistics and
  plots](https://chunshengliang.github.io/dataprep/articles/dataprep-plots.html) — `descplot` / `percplot`.

# dataprep 0.1.5

* Initial public release on CRAN.
* Core cleaning pipeline: `varidele`, `obsedele`, `condextr`,
  `percoutl`, `optisolu`, `shorvalu`, `dataprep`.
* Descriptive statistics and percentile helpers: `descdata`,
  `descplot`, `percdata`, `percplot`.
* Example datasets: `data`, `data1`.
