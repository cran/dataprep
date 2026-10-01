# dataprep <img src="man/figures/logo.png" align="right" height="180" alt="" />

<div align="right"><sub>logo by Chun-Sheng Liang</sub></div>

> Fast, efficient, and versatile data preprocessing and reshaping tools for R,
> with C++ / OpenMP / SIMD backends.

[![R-CMD-check](https://github.com/chunshengliang/dataprep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chunshengliang/dataprep/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/dataprep)](https://cran.r-project.org/package=dataprep)
[![CRAN checks](https://badges.cranchecks.info/worst/dataprep.svg)](https://cran.r-project.org/web/checks/check_results_dataprep.html)
[![Downloads per month](https://cranlogs.r-pkg.org/badges/dataprep?color=brightgreen)](https://cran.r-project.org/package=dataprep)
[![Downloads total](https://cranlogs.r-pkg.org/badges/grand-total/dataprep)](https://cran.r-project.org/package=dataprep)
[![StackOverflow](https://img.shields.io/stackexchange/stackoverflow/t/dataprep?logo=stackoverflow&label=Questions)](https://stackoverflow.com/questions/tagged/dataprep)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/chunshengliang/dataprep)


## In one paragraph

`dataprep` provides an opinionated, high-performance pipeline for
cleaning tabular and time-series data. The 0.1.8 release rewrites
the cleaning routines in C++ and delivers a speedup over 0.1.5 that
ranges from about **1.0×** (for `varidele` on some full-year data)
to about **1146×** (for `condextr` on full-year Ubuntu data). The
`melt()` and `dcast()` reshaping functions are benchmarked against
every one of the seven major alternatives in the R and Python
ecosystems, at every tested scale (from 1,000 to 100,000,000 rows),
on two reference hosts; the resulting speed-up ranges are
**0.6–1628.9×** for `melt()` and **1.9–799.8×** for `dcast()`. On both
hosts the output is **identical** to `reshape2`, `data.table`,
`tidyr`, `pandas`, `polars`, `dask`, and `duckdb`, within
`tol = 1e-12`.

## Why dataprep

`dataprep` provides a coherent, opinionated pipeline for
preprocessing tabular and time-series data:

* **Variable deletion** by missing-value fraction (`varidele`).
* **Observation deletion** by consecutive missing runs (`obsedele`).
* **Outlier removal** by point-by-point weighted conditional
  extremum (`condextr`) or by percentile (`percoutl`).
* **Missing-value imputation** within short periods (`shorvalu`)
  or by linear / LOCF / NOCB / mean / median (`impute_missing`).
* **Fast reshaping** between wide and long formats (`melt`,
  `dcast`) with SIMD + OpenMP.
* **Descriptive statistics, diagnostics, transformation,
  standardization, encoding, validation, and reporting.**
* **Time-series tools**: detrending, diurnal-cycle removal,
  rolling statistics, lags, resampling, decomposition, drift
  detection, day/night and season flags.
* **Fit / transform interfaces** (`prep_fit`, `prep_transform`)
  that prevent data leakage during preprocessing.

Most heavy routines are written in C++ with Rcpp. Since 0.1.8,
many operations are parallelized with OpenMP and vectorized with
AVX2 / AVX-512 when the hardware supports it.

## Design philosophy

The cleaning pipeline is organised around four sequential steps,
each addressing a distinct failure mode of high-resolution
environmental data:

<p align="center">
  <img src="man/figures/fig1_pipeline.png"
       alt="Four-step preprocessing pipeline"
       width="50%" />
</p>

1. **Variable deletion.** Drop size bins whose missing fraction
   exceeds a threshold, so downstream interpolation never has to
   extrapolate from far-away anchors.
2. **Observation deletion.** Drop rows whose selected columns
   contain a consecutive missing run longer than `half` minutes
   on **both** sides. Every remaining point then has a trustworthy
   anchor within `half` minutes.
3. **Conditional extremum outlier removal.** A single value can
   be a global maximum and still be legitimate, or vice versa.
   `condextr()` judges each candidate in context.

   <div align="center">
     <img src="man/figures/Outlier_Comparison.png"
          alt="Conditional extremum vs. traditional percentile deletion"
          width="50%" />
   </div>

4. **Short-period grouping interpolation.** After steps 1–3,
   remaining `NA`s sit inside short gaps with a valid anchor
   within `half` minutes. `shorvalu()` interpolates within each
   short segment only.

   <div align="center">
     <img src="man/figures/Time_Series_Interpolation_Final.png"
          alt="Short-period grouping interpolation"
          width="50%" />
   </div>

   Interpolating across a long gap silently mixes two physically
   distinct regimes and can create new outliers at the segment
   boundary. Grouping by short segments keeps the interpolation
   local.

Steps 1–4 are wrapped by `dataprep()` for one-call use. The design
reasoning is documented in full in
`vignette("dataprep-philosophy")`. `data1` in this package is the
**already-aggregated** seven-column version of the same dataset;
it is not a useful input for the cleaning pipeline.

## Installation

**Recommended** (also builds the vignettes locally; needs `pandoc`
and the R packages `knitr` and `rmarkdown`):

```r
# install.packages("remotes")
remotes::install_github("chunshengliang/dataprep", build_vignettes = TRUE)
```

**Fallback** (no extra dependencies):

```r
remotes::install_github("chunshengliang/dataprep")
```

The package requires a C++17 compiler (Rtools on Windows,
Xcode / clang on macOS, gcc on Linux). The `build_vignettes = TRUE`
variant additionally needs `pandoc` and the R packages `knitr` and
`rmarkdown`; if any of those is missing, `remotes` will fail. Vignettes
are also available on the package website:
<https://chunshengliang.github.io/dataprep/articles/>.

**Note for Windows users**

When installing from GitHub with `remotes::install_github()`, Windows
users may see:

> Warning: file 'dataprep/configure' did not have execute permissions: corrected
>
> Warning: file 'dataprep/cleanup' did not have execute permissions: corrected

This is expected and harmless. Windows NTFS does not preserve Unix
execute bits, so `R CMD build` corrects them automatically. The
`configure.win` and `cleanup.win` scripts still run, and the package
installs and works normally — **the warning does not affect any
functionality in any way**. Linux, macOS, and CRAN checks do not emit
this warning, and Windows users installing the CRAN binary package
with `install.packages("dataprep")` are not affected either.

## Quick start

```r
library(dataprep)

# The size-bin columns are the ones whose names are numeric
# (1.00, 1.12, ..., 1000). The four non-size columns
# (`date`, `tconc`, `TPNC`, `monthyear`) are excluded by this
# pattern.
size_bins <- grep("^[-+]?[0-9]*\\.?[0-9]+$", names(data))

cleaned <- dataprep(
  data,
  cols       = size_bins,
  group      = 4,        # monthyear
  interval   = 10,
  times      = 10,
  intervals  = 30
)
dim(cleaned)
```

## Performance

`melt()` and `dcast()` are benchmarked against all 7 major
alternatives across 10 shapes and 6 scales (1,000 to
100,000,000 rows). Every cell is measured with a C++ steady-clock
timer and an adaptive `times` rule (20 / 15 / 10 / 5 / 1 iterations
based on warmup time). Two reference hosts were used.


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

The two hosts differ in core count, cache size and memory
bandwidth. The relative ranking of the engines is identical on
both; the absolute multipliers scale with the hardware. On a
typical 8–16-core workstation the same comparisons remain within
10–100×.

All numbers below are means in milliseconds. Each cell is
written as `time (speedup×)`, where `time` is the mean for that
engine and `speedup×` is `time / dataprep_time`. The `dataprep`
column itself is the baseline, so it has no multiplier.

### `melt()` — Ubuntu 25.10

**Vary rows, 1 id + 9 value columns**

| rows | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.173 | 0.378 (2.2×) | 0.257 (1.5×) | 2.788 (16.1×) | 2.128 (12.3×) | 0.645 (3.7×) | 15.786 (91.3×) | 4.489 (26.0×) |
| 1e4 | 0.241 | 0.462 (1.9×) | 0.334 (1.4×) | 3.261 (13.5×) | 2.499 (10.4×) | 0.829 (3.4×) | 15.818 (65.6×) | 10.850 (45.0×) |
| 1e5 | 0.679 | 1.204 (1.8×) | 1.034 (1.5×) | 8.167 (12.0×) | 6.659 (9.8×) | 2.029 (3.0×) | 17.638 (26.0×) | 68.473 (100.8×) |
| 1e6 | 3.474 | 18.797 (5.4×) | 9.646 (2.8×) | 80.014 (23.0×) | 61.686 (17.8×) | 14.671 (4.2×) | 47.584 (13.7×) | 648.895 (186.8×) |
| 1e7 | 33.412 | 364.564 (10.9×) | 365.065 (10.9×) | 1083.987 (32.4×) | 710.806 (21.3×) | 139.959 (4.2×) | 482.693 (14.4×) | 6389.564 (191.2×) |
| 1e8 | 276.295 | 3579.061 (13.0×) | 3571.833 (12.9×) | 12126.897 (43.9×) | 7463.089 (27.0×) | 3121.344 (11.3×) | 4756.547 (17.2×) | 71947.305 (260.4×) |

**Vary rows, 10 id (5 int + 5 chr)**

| rows | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.242 | 0.574 (2.4×) | 0.408 (1.7×) | 3.013 (12.5×) | 5.565 (23.0×) | 0.908 (3.8×) | 51.309 (212.2×) | 10.359 (42.8×) |
| 1e4 | 0.472 | 2.017 (4.3×) | 1.766 (3.7×) | 4.811 (10.2×) | 6.123 (13.0×) | 1.953 (4.1×) | 51.327 (108.7×) | 49.807 (105.4×) |
| 1e5 | 3.680 | 16.603 (4.5×) | 14.949 (4.1×) | 22.929 (6.2×) | 13.307 (3.6×) | 4.299 (1.2×) | 56.135 (15.3×) | 472.664 (128.4×) |
| 1e6 | 20.354 | 208.901 (10.3×) | 157.119 (7.7×) | 221.574 (10.9×) | 90.483 (4.4×) | 39.946 (2.0×) | 111.196 (5.5×) | 4701.936 (231.0×) |
| 1e7 | 827.346 | 3125.168 (3.8×) | 2651.650 (3.2×) | 3724.859 (4.5×) | 1340.982 (1.6×) | 518.916 (0.6×) | 963.953 (1.2×) | 47964.021 (58.0×) |

**Vary value columns, 1e3 rows, 1 id**

| n_val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 0.174 | 0.381 (2.2×) | 0.256 (1.5×) | 2.803 (16.1×) | 2.123 (12.2×) | 0.587 (3.4×) | 15.358 (88.2×) | 4.829 (27.7×) |
| 100 | 0.261 | 1.110 (4.3×) | 0.371 (1.4×) | 3.699 (14.2×) | 6.431 (24.7×) | 0.750 (2.9×) | 44.906 (172.1×) | 21.673 (83.1×) |
| 1000 | 0.916 | 8.127 (8.9×) | 1.291 (1.4×) | 12.233 (13.4×) | 48.227 (52.6×) | 2.704 (3.0×) | 313.498 (342.2×) | 178.161 (194.5×) |
| 10000 | 2.295 | 93.092 (40.6×) | 12.415 (5.4×) | 104.924 (45.7×) | 495.659 (216.0×) | 19.408 (8.5×) | 3737.866 (1628.9×) | 1919.247 (836.4×) |

**Vary value columns, 1e3 rows, 10 id**

| n_val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 0.247 | 0.576 (2.3×) | 0.426 (1.7×) | 3.022 (12.2×) | 5.662 (22.9×) | 1.010 (4.1×) | 48.905 (198.0×) | 13.088 (53.0×) |
| 100 | 0.533 | 2.972 (5.6×) | 1.985 (3.7×) | 5.397 (10.1×) | 20.806 (39.0×) | 2.094 (3.9×) | 188.372 (353.4×) | 60.608 (113.7×) |
| 1000 | 4.177 | 25.214 (6.0×) | 16.681 (4.0×) | 28.636 (6.9×) | 166.521 (39.9×) | 6.611 (1.6×) | 1784.542 (427.2×) | 570.076 (136.5×) |
| 10000 | 25.009 | 308.602 (12.3×) | 182.019 (7.3×) | 282.574 (11.3×) | 1783.493 (71.3×) | 71.605 (2.9×) | 23777.051 (950.7×) | 5815.567 (232.5×) |

### `melt()` — Windows 11 Pro for Workstations

**Vary rows, 1 id + 9 value columns**

| rows | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.286 | 0.647 (2.3×) | 0.468 (1.6×) | 4.048 (14.2×) | 3.365 (11.8×) | 0.528 (1.8×) | 28.149 (98.4×) | 8.201 (28.7×) |
| 1e4 | 0.529 | 0.963 (1.8×) | 0.738 (1.4×) | 5.106 (9.7×) | 5.550 (10.5×) | 0.849 (1.6×) | 28.967 (54.8×) | 23.331 (44.1×) |
| 1e5 | 2.690 | 3.366 (1.3×) | 3.368 (1.3×) | 16.860 (6.3×) | 22.516 (8.4×) | 3.344 (1.2×) | 44.894 (16.7×) | 180.028 (66.9×) |
| 1e6 | 10.515 | 26.197 (2.5×) | 24.209 (2.3×) | 160.785 (15.3×) | 214.683 (20.4×) | 21.721 (2.1×) | 181.218 (17.2×) | 1537.791 (146.2×) |
| 1e7 | 77.008 | 245.861 (3.2×) | 247.910 (3.2×) | 1561.960 (20.3×) | 1925.931 (25.0×) | 275.120 (3.6×) | 1499.583 (19.5×) | 14517.337 (188.5×) |
| 1e8 | 935.148 | 2636.186 (2.8×) | 2534.491 (2.7×) | 16599.644 (17.8×) | 19578.283 (20.9×) | 4263.390 (4.6×) | 14714.823 (15.7×) | 148009.529 (158.3×) |

**Vary rows, 10 id (5 int + 5 chr)**

| rows | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.570 | 1.206 (2.1×) | 0.947 (1.7×) | 4.695 (8.2×) | 11.374 (20.0×) | 1.266 (2.2×) | 98.325 (172.6×) | 21.374 (37.5×) |
| 1e4 | 1.706 | 4.985 (2.9×) | 3.630 (2.1×) | 8.509 (5.0×) | 14.912 (8.7×) | 2.074 (1.2×) | 103.953 (60.9×) | 114.451 (67.1×) |
| 1e5 | 13.969 | 38.706 (2.8×) | 27.974 (2.0×) | 45.778 (3.3×) | 44.324 (3.2×) | 9.372 (0.7×) | 134.847 (9.7×) | 1007.726 (72.1×) |
| 1e6 | 92.041 | 392.050 (4.3×) | 272.765 (3.0×) | 444.403 (4.8×) | 323.335 (3.5×) | 92.341 (1.0×) | 371.440 (4.0×) | 9545.364 (103.7×) |
| 1e7 | 857.942 | 3974.411 (4.6×) | 2867.507 (3.3×) | 4703.791 (5.5×) | 3213.337 (3.7×) | 965.515 (1.1×) | 2710.996 (3.2×) | 96232.400 (112.2×) |

**Vary value columns, 1e3 rows, 1 id**

| n_val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 0.440 | 0.773 (1.8×) | 0.608 (1.4×) | 4.256 (9.7×) | 3.618 (8.2×) | 0.642 (1.5×) | 28.358 (64.5×) | 31.298 (71.2×) |
| 100 | 0.939 | 2.505 (2.7×) | 1.241 (1.3×) | 6.455 (6.9×) | 16.555 (17.6×) | 228.139 (242.9×) | 112.824 (120.1×) | 47.901 (51.0×) |
| 1000 | 3.843 | 16.235 (4.2×) | 3.787 (1.0×) | 23.592 (6.1×) | 142.100 (37.0×) | 4.550 (1.2×) | 964.746 (251.0×) | 452.106 (117.6×) |
| 10000 | 11.569 | 161.647 (14.0×) | 34.383 (3.0×) | 203.580 (17.6×) | 1410.476 (121.9×) | 36.412 (3.1×) | 10333.080 (893.2×) | 5310.694 (459.0×) |

**Vary value columns, 1e3 rows, 10 id**

| n_val | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 0.551 | 1.255 (2.3×) | 0.887 (1.6×) | 4.577 (8.3×) | 12.074 (21.9×) | 1.108 (2.0×) | 102.415 (185.7×) | 24.440 (44.3×) |
| 100 | 1.701 | 6.266 (3.7×) | 3.721 (2.2×) | 9.453 (5.6×) | 58.290 (34.3×) | 2.607 (1.5×) | 501.132 (294.7×) | 141.639 (83.3×) |
| 1000 | 16.754 | 54.631 (3.3×) | 31.430 (1.9×) | 55.585 (3.3×) | 567.945 (33.9×) | 12.350 (0.7×) | 4799.771 (286.5×) | 1373.995 (82.0×) |
| 10000 | 101.066 | 554.243 (5.5×) | 346.842 (3.4×) | 512.059 (5.1×) | 5672.202 (56.1×) | 115.524 (1.1×) | 54507.720 (539.3×) | 12874.917 (127.4×) |

### `dcast()` — Ubuntu 25.10

**Vary n_long, 1 id, 10 levels**

| n_long | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.888 | 1.664 (1.9×) | 1.810 (2.0×) | 4.134 (4.7×) | 1.905 (2.1×) | 33.322 (37.5×) | 8.107 (9.1×) | 7.270 (8.2×) |
| 1e4 | 0.940 | 2.611 (2.8×) | 2.559 (2.7×) | 4.474 (4.8×) | 2.329 (2.5×) | 48.157 (51.3×) | 8.868 (9.4×) | 10.398 (11.1×) |
| 1e5 | 1.090 | 19.351 (17.8×) | 14.634 (13.4×) | 7.749 (7.1×) | 7.078 (6.5×) | 49.857 (45.7×) | 14.741 (13.5×) | 34.205 (31.4×) |
| 1e6 | 1.654 | 151.826 (91.8×) | 328.980 (198.9×) | 44.728 (27.0×) | 56.741 (34.3×) | 105.018 (63.5×) | 78.217 (47.3×) | 155.880 (94.3×) |
| 1e7 | 9.202 | 1693.368 (184.0×) | 560.932 (61.0×) | 716.377 (77.9×) | 741.921 (80.6×) | 310.878 (33.8×) | 957.022 (104.0×) | 1706.000 (185.4×) |
| 1e8 | 91.926 | 21766.497 (236.8×) | 16869.402 (183.5×) | 10507.819 (114.3×) | 10397.700 (113.1×) | 2434.035 (26.5×) | 13416.534 (145.9×) | 17118.026 (186.2×) |

**Vary levels, 1 id, 1e6 rows**

| levels | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 1.654 | 151.826 (91.8×) | 328.980 (198.9×) | 44.728 (27.0×) | 56.741 (34.3×) | 105.018 (63.5×) | 78.217 (47.3×) | 155.880 (94.3×) |
| 100 | 1.415 | 101.438 (71.7×) | 329.202 (232.6×) | 42.047 (29.7×) | 53.686 (37.9×) | 173.478 (122.6×) | 74.540 (52.7×) | 178.421 (126.0×) |
| 1000 | 2.107 | 100.949 (47.9×) | 251.767 (119.5×) | 45.413 (21.6×) | 56.445 (26.8×) | 186.575 (88.6×) | 76.185 (36.2×) | 195.787 (92.9×) |
| 10000 | 12.285 | 140.127 (11.4×) | 405.321 (33.0×) | 57.827 (4.7×) | 59.199 (4.8×) | 308.858 (25.1×) | 80.920 (6.6×) | 493.181 (40.1×) |

**Vary n_long, 1 id, 100 levels**

| n_long | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e4 | 1.010 | 2.860 (2.8×) | 2.884 (2.9×) | 4.640 (4.6×) | 2.483 (2.5×) | 45.241 (44.8×) | 9.460 (9.4×) | 14.845 (14.7×) |
| 1e5 | 1.184 | 18.509 (15.6×) | 8.577 (7.2×) | 7.826 (6.6×) | 6.861 (5.8×) | 60.782 (51.3×) | 14.937 (12.6×) | 43.299 (36.6×) |
| 1e6 | 1.415 | 101.438 (71.7×) | 329.202 (232.6×) | 42.047 (29.7×) | 53.686 (37.9×) | 173.478 (122.6×) | 74.540 (52.7×) | 178.421 (126.0×) |
| 1e7 | 5.073 | 2016.036 (397.4×) | 575.604 (113.5×) | 660.455 (130.2×) | 816.883 (161.0×) | 506.557 (99.9×) | 1002.368 (197.6×) | 1669.419 (329.1×) |
| 1e8 | 40.680 | 16963.494 (417.0×) | 18866.887 (463.8×) | 8100.475 (199.1×) | 9529.877 (234.3×) | 2460.625 (60.5×) | 12764.776 (313.8×) | 17616.392 (433.1×) |

**Vary n_id, 1e6 rows, 10 levels**

| n_id | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.654 | 151.826 (91.8×) | 328.980 (198.9×) | 44.728 (27.0×) | 56.741 (34.3×) | 105.018 (63.5×) | 78.217 (47.3×) | 155.880 (94.3×) |
| 2 | 1.915 | 218.272 (114.0×) | 304.452 (159.0×) | 54.255 (28.3×) | 80.896 (42.2×) | 122.649 (64.0×) | 108.293 (56.5×) | 285.377 (149.0×) |
| 10 | 3.565 | 1247.607 (350.0×) | 405.322 (113.7×) | 93.897 (26.3×) | 192.356 (54.0×) | 119.942 (33.6×) | 248.201 (69.6×) | 922.863 (258.9×) |
| 100 | 22.579 | 10394.072 (460.3×) | 549.582 (24.3×) | 431.569 (19.1×) | 1415.001 (62.7×) | 150.336 (6.7×) | 1608.617 (71.2×) | 8314.759 (368.3×) |

### `dcast()` — Windows 11 Pro for Workstations

**Vary n_long, 1 id, 10 levels**

| n_long | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e3 | 0.384 | 2.412 (6.3×) | 3.885 (10.1×) | 6.465 (16.9×) | 2.832 (7.4×) | 5.114 (13.3×) | 15.597 (40.7×) | 19.382 (50.5×) |
| 1e4 | 0.478 | 4.178 (8.7×) | 7.391 (15.5×) | 8.016 (16.8×) | 5.078 (10.6×) | 5.806 (12.1×) | 18.732 (39.2×) | 24.628 (51.5×) |
| 1e5 | 1.029 | 31.267 (30.4×) | 39.986 (38.9×) | 13.887 (13.5×) | 20.444 (19.9×) | 11.069 (10.8×) | 42.061 (40.9×) | 73.489 (71.5×) |
| 1e6 | 3.426 | 281.533 (82.2×) | 148.638 (43.4×) | 93.290 (27.2×) | 251.508 (73.4×) | 44.604 (13.0×) | 346.422 (101.1×) | 391.237 (114.2×) |
| 1e7 | 23.194 | 2820.301 (121.6×) | 989.520 (42.7×) | 1481.134 (63.9×) | 2698.928 (116.4×) | 459.512 (19.8×) | 3299.792 (142.3×) | 3619.348 (156.0×) |
| 1e8 | 214.305 | 29441.634 (137.4×) | 13164.922 (61.4×) | 16383.457 (76.4×) | 31753.144 (148.2×) | 4722.561 (22.0×) | 38192.158 (178.2×) | 33441.939 (156.0×) |

**Vary levels, 1 id, 1e6 rows**

| levels | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 3.426 | 281.533 (82.2×) | 148.638 (43.4×) | 93.290 (27.2×) | 251.508 (73.4×) | 44.604 (13.0×) | 346.422 (101.1×) | 391.237 (114.2×) |
| 100 | 4.049 | 173.399 (42.8×) | 169.140 (41.8×) | 91.050 (22.5×) | 229.426 (56.7×) | 56.026 (13.8×) | 332.823 (82.2×) | 800.504 (197.7×) |
| 1000 | 5.064 | 189.364 (37.4×) | 153.576 (30.3×) | 81.644 (16.1×) | 253.038 (50.0×) | 178.688 (35.3×) | 316.612 (62.5×) | 1136.216 (224.4×) |
| 10000 | 29.411 | 252.442 (8.6×) | 191.452 (6.5×) | 115.340 (3.9×) | 275.314 (9.4×) | 1350.575 (45.9×) | 367.614 (12.5×) | 13596.212 (462.3×) |

**Vary n_long, 1 id, 100 levels**

| n_long | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1e4 | 0.514 | 4.960 (9.7×) | 8.608 (16.8×) | 8.088 (15.7×) | 5.519 (10.7×) | 9.883 (19.2×) | 18.710 (36.4×) | 94.938 (184.8×) |
| 1e5 | 0.838 | 34.405 (41.1×) | 41.520 (49.6×) | 17.537 (20.9×) | 37.751 (45.1×) | 13.291 (15.9×) | 63.692 (76.0×) | 303.785 (362.7×) |
| 1e6 | 4.049 | 173.399 (42.8×) | 169.140 (41.8×) | 91.050 (22.5×) | 229.426 (56.7×) | 56.026 (13.8×) | 332.823 (82.2×) | 800.504 (197.7×) |
| 1e7 | 38.527 | 3197.642 (83.0×) | 1100.505 (28.6×) | 1359.999 (35.3×) | 2337.317 (60.7×) | 814.529 (21.1×) | 3014.871 (78.3×) | 7495.310 (194.5×) |
| 1e8 | 107.287 | 23690.584 (220.8×) | 14715.772 (137.2×) | 14245.599 (132.8×) | 25843.625 (240.9×) | 6222.505 (58.0×) | 33050.665 (308.1×) | 85804.834 (799.8×) |

**Vary n_id, 1e6 rows, 10 levels**

| n_id | dataprep | reshape2 | data.table | tidyr | pandas | polars | dask | duckdb |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 3.426 | 281.533 (82.2×) | 148.638 (43.4×) | 93.290 (27.2×) | 251.508 (73.4×) | 44.604 (13.0×) | 346.422 (101.1×) | 391.237 (114.2×) |
| 2 | 10.712 | 401.842 (37.5×) | 195.915 (18.3×) | 116.768 (10.9×) | 367.464 (34.3×) | 57.728 (5.4×) | 487.128 (45.5×) | 697.317 (65.1×) |
| 10 | 16.392 | 2687.391 (163.9×) | 326.747 (19.9×) | 187.891 (11.5×) | 875.166 (53.4×) | 66.015 (4.0×) | 1109.804 (67.7×) | 2012.130 (122.8×) |
| 100 | 76.279 | 23604.925 (309.5×) | 1002.287 (13.1×) | 740.787 (9.7×) | 6652.292 (87.2×) | 198.455 (2.6×) | 8254.600 (108.2×) | 15421.548 (202.2×) |

### Summary of speedups

Speedup is defined as `competitor mean / dataprep mean`.
Each table summarises every benchmark cell on that host, across all
seven competitors (`reshape2`, `data.table`, `tidyr`, `pandas`,
`polars`, `dask`, `duckdb`). Cell labels are written as
`n_rows × n_cols × n_id × n_val` for `melt()` and
`n_long × n_id × n_levels` for `dcast()`.

**Ubuntu 25.10**

| Operation | Min | Median | Mean | Max |
|---|---:|---:|---:|---:|
| `melt()`  | 0.6× (polars @ 1e7 × 19 × 10 × 9) | 11.3× | 67.8× | 1628.9× (dask @ 1e3 × 10001 × 1 × 10000) |
| `dcast()` | 1.9× (reshape2 @ 1e3 × 1 × 10)    | 46.5× | 90.2× | 463.8× (data.table @ 1e8 × 1 × 100)      |

**Windows 11 Pro for Workstations**

| Operation | Min | Median | Mean | Max |
|---|---:|---:|---:|---:|
| `melt()`  | 0.7× (polars @ 1e5 × 19 × 10 × 9) | 5.6× | 46.6× | 893.2× (dask @ 1e3 × 10001 × 1 × 10000) |
| `dcast()` | 2.6× (polars @ 1e6 × 100 × 10)   | 41.4× | 76.6× | 799.8× (duckdb @ 1e8 × 1 × 100)         |

Combined across both hosts, the smallest speedups remain at
0.6× (`melt()` at 1e7 rows on Ubuntu), while the largest reach
1628.9× for `melt()` and 799.8× for `dcast()`. The mean speedup is above
46× for `melt()` and above 76× for `dcast()` on both hosts. For
`melt()` on the largest cells (1e8 rows, 1 id + 9 val, 8 GB of
input), `dataprep` is the only engine that completes within 2.5 s,
specifically < 0.3 s on Ubuntu and < 1.0 s on Windows.

Complete tables — including mean, median, and the full
per-competitor gradient — are in
`vignette("dataprep-performance")`.

## Cross-engine consistency

`melt()` and `dcast()` produce output identical to `reshape2`,
`data.table`, `tidyr`, `pandas`, `polars`, `dask`, and `duckdb` on
every tested shape, within `tol = 1e-12`:

| Operation | Cells tested | Engines | Pairwise |
|---|---:|---:|---|
| `melt` | 4 shapes | 8 | all consistent |
| `dcast` | 4 shapes | 8 | all consistent |

Reproducible scripts ship under `inst/` and are disabled by
default so that `R CMD check` does not run them. A single script,
`benchmark_melt_dcast.R`, runs both the per-tool benchmarks and the
8-engine consistency checks:

```r
Sys.setenv(DATAPREP_RUN_BENCHMARK = "1")
source(system.file("benchmark_melt_dcast.R", package = "dataprep"))
```

## When not to preprocess

The pipeline above assumes that the input is high-resolution
instrument data with intermittent gaps and occasional outliers.
Three cases where you should not run the full pipeline:

1. **Already-aggregated data.** `data1` in this package is the
   result of aggregating the 61 size bins of `data` into three
   modes. It has no long gaps and no obvious outliers, so
   `varidele`, `obsedele`, `condextr`, and `shorvalu` have nothing
   to do.
2. **Models that tolerate missing values.** Gradient boosting,
   random forests, and XGBoost handle `NA` natively.
3. **Gaps shorter than the physical mixing time.** When the
   aerosol is well-mixed, a few missing points can be interpolated
   with negligible error.

See `vignette("dataprep-philosophy")` for the full reasoning.

## Function overview

### Cleaning

* `varidele()` — remove variables by missing fraction
* `obsedele()` — remove observations by consecutive missing runs
* `condextr()` — point-by-point weighted conditional extremum
* `percoutl()` — traditional percentile removal
* `detect_outliers()` — IQR / MAD / percentile masks
* `winsorize()` — cap extreme values
* `phys_filter()` — physical range filter
* `filter_high_cor()` / `filter_low_var()` — variable selection
* `deduplicate()` — exact / fuzzy duplicate removal
* `validate_data()` — rule-based validation
* `balance_panel()` — panel balancing

### Missing values and imputation

* `na_diagnose()` — NA run statistics
* `impute_missing()` — linear / LOCF / NOCB / mean / median
* `shorvalu()` — short-period linear interpolation

### Transformation

* `transform_data()` — log / sqrt / Box-Cox / Yeo-Johnson,
  z-score / min-max / robust
* `log_returns()` — log returns
* `bin_data()` — equal-width / equal-frequency / custom binning
* `encode_categorical()` — label / frequency / one-hot

### Time series

* `create_lags()` — grouped lag / lead columns
* `roll_apply()` — rolling statistics
* `resample_time()` — resample to hour / day / month
* `detrend_ts()` — linear detrending
* `remove_diurnal_cycle()` — subtract mean diurnal cycle
* `decompose_ts()` — additive / multiplicative decomposition
* `drift_detect()` — rolling drift detection
* `day_night_flag()` / `season_flag()` — time flags

### Reshaping

* `melt()` — wide to long, SIMD + OpenMP backend
* `dcast()` — long to wide, block-path strided copy

### Workflow and reporting

* `descdata()` / `descplot()` — descriptive statistics
* `percdata()` / `percplot()` — percentile summaries
* `data_report()` — compact data quality report
* `dry_run()` — simulate preprocessing
* `prep_fit()` / `prep_transform()` — fit / transform pipeline
* `sample_data()` — stratified sampling

## Documentation

* **Design philosophy and preprocessing methodology** — why the
  pipeline has the shape it does.
* **Cleaning pipeline** — step-by-step walkthrough of `varidele` /
  `obsedele` / `condextr` / `shorvalu`.
* **Performance and cross-engine consistency** — full
  benchmark tables and consistency checks.
* **Upgrading from 0.1.5 to 0.1.8** — behaviour changes
  and migration checklist.
* **Leakage-free workflow** — `prep_fit()` / `prep_transform()`.
* **Fast reshaping with `melt()` and `dcast()`**.
* **Descriptive statistics and plots**.
* **Function reference**.
* **Changelog**.

## Funding

This work was supported by the National Natural Science Foundation
of China (No. 12301674).

## Citation

If you use dataprep in published work, please cite:

> Liang, C.-S., Wu, H., Li, H.-Y., Zhang, Q., Li, Z. & He, K.-B.
> (2020). Efficient data preprocessing, episode classification, and
> source apportionment of particle number concentrations.
> *Science of the Total Environment*, 741, 140923.
> https://doi.org/10.1016/j.scitotenv.2020.140923

## License

GPL (>= 2)
