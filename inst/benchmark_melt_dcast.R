# ============================================================================
# Melt / dcast benchmark and 8-engine consistency harness for dataprep.
#
# Structure:
#   Section 1  Build / install dataprep
#   Section 2  Sanity checks: dataprep vs reshape2 vs tidyr
#   Section 3  8-engine consistency check for melt / unpivot
#   Section 4  8-engine consistency check for dcast / pivot
#   Section 5  Adaptive per-tool benchmark for melt
#   Section 6  Adaptive per-tool benchmark for dcast
#
# All inputs use mixed id types (integer + character) for n_id >= 2,
# matching real composite keys. Set DATAPREP_MIXED_TYPES=FALSE to fall
# back to the all-integer baseline. Labels are generated with id_desc()
# so the header is printed exactly once per cell, in the form
#   [<label>] mixed=<TRUE|FALSE>
# ============================================================================

library(Rcpp)
library(dplyr)


# Timestamped output filenames. One pair per script invocation, format
# YYYYMMDDHHMM. Re-running the sweep in the same minute overwrites the
# previous files; use "%Y%m%d%H%M%S" if you need second-level resolution.
.BENCH_TS  <- format(Sys.time(), "%Y%m%d%H%M")
.MELT_CSV  <- sprintf("melt_benchmark_%s.csv",  .BENCH_TS)
.DCAST_CSV <- sprintf("dcast_benchmark_%s.csv", .BENCH_TS)


source(system.file("benchmark_helpers.R", package = "dataprep"))


# ============================================================================
# Section 1 -- build / install
# ============================================================================
# cd /media/zhike/D/repackages
# R CMD INSTALL --no-multiarch --no-test-load dataprep
library(dataprep)


# ============================================================================
# Section 2 -- sanity checks against reshape2 and tidyr
# ============================================================================
data("data", package = "dataprep")

stopifnot(identical(dataprep::melt(data, 1:4),
                    reshape2::melt(data, 1:4)))
stopifnot(identical(dataprep::melt(data, 1:4, major = "col"),
                    reshape2::melt(data, 1:4)))

all_identical <- function(...) {
  xs <- list(...)
  for (i in seq_along(xs)[-1])
    if (!identical(as.data.frame(xs[[1]]), as.data.frame(xs[[i]])))
      return(FALSE)
  TRUE
}

stopifnot(all_identical(
  dataprep::melt(data, 1:4, major = "col"),
  reshape2::melt(data, 1:4),
  tidyr::pivot_longer(data, !1:4, cols_vary = "slowest",
                      names_to = "variable") %>%
    mutate(variable = factor(variable, unique(variable))) %>%
    as.data.frame()
))

stopifnot(all_identical(
  dataprep::melt(data, 1:4, major = "row"),
  tidyr::pivot_longer(data, !1:4, names_to = "variable") %>%
    as.data.frame()
))


# ============================================================================
# Section 3 -- 8-engine consistency check for melt
# ============================================================================
suppressPackageStartupMessages({
  library(reticulate); library(data.table)
  library(reshape2);   library(tidyr)
  library(dataprep)
})

sapply(c("reticulate", "data.table", "reshape2", "tidyr"),
       function(x) as.character(packageVersion(x)))

pandas         <- import("pandas")
polars         <- import("polars")
dask           <- import("dask")
dask_dataframe <- import("dask.dataframe")
duckdb         <- import("duckdb")

cat("pandas:", as.character(pandas$`__version__`),
    " polars:", as.character(polars$`__version__`),
    " dask:",   as.character(dask$`__version__`),
    " duckdb:", as.character(duckdb$`__version__`), "\n")

py_run_string("import warnings; warnings.filterwarnings('ignore')")

py_pull <- function(tmp) {
  py$tmp_df <- tmp
  py_run_string("out = tmp_df.to_dict(orient='list')")
  df <- as.data.frame(py$out, stringsAsFactors = FALSE)
  py$tmp_df <- NULL
  py$out    <- NULL
  df
}

canonicalize_long <- function(x, id_cols,
                              var_col   = "variable",
                              value_col = "value") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  need <- c(id_cols, var_col, value_col)
  missing <- setdiff(need, names(x))
  if (length(missing) > 0)
    stop(sprintf("Missing columns in result: %s",
                 paste(missing, collapse = ", ")))

  x <- x[, need, drop = FALSE]
  x[[var_col]] <- as.character(x[[var_col]])

  # Normalise id column types: character stays character, numeric
  # becomes double. This lets integer / double / factor / object
  # outputs from different engines be compared on equal footing.
  for (nm in id_cols) {
    if (is.character(x[[nm]]) || is.factor(x[[nm]])) {
      x[[nm]] <- as.character(x[[nm]])
    } else {
      x[[nm]] <- as.numeric(x[[nm]])
    }
  }

  ord <- do.call(order, x[c(id_cols, var_col)])
  x   <- x[ord, , drop = FALSE]
  rownames(x) <- NULL
  x
}

compare_long <- function(x, y, id_cols,
                         var_col   = "variable",
                         value_col = "value",
                         tol       = 1e-12) {
  if (nrow(x) != nrow(y)) return(FALSE)
  if (!identical(sort(names(x)), sort(names(y)))) return(FALSE)

  for (nm in id_cols) {
    xc <- x[[nm]]; yc <- y[[nm]]
    if (is.character(xc) || is.factor(xc) ||
        is.character(yc) || is.factor(yc)) {
      a <- as.character(xc); b <- as.character(yc)
      if (any(xor(is.na(a), is.na(b)))) return(FALSE)
      if (!identical(a, b)) return(FALSE)
    } else {
      a <- as.numeric(xc); b <- as.numeric(yc)
      if (any(xor(is.na(a), is.na(b)))) return(FALSE)
      if (any(abs(a - b) > tol, na.rm = TRUE)) return(FALSE)
    }
  }

  if (!identical(as.character(x[[var_col]]),
                 as.character(y[[var_col]]))) return(FALSE)

  vx <- as.numeric(x[[value_col]])
  vy <- as.numeric(y[[value_col]])
  if (any(xor(is.na(vx), is.na(vy)))) return(FALSE)
  ok <- is.finite(vx) & is.finite(vy)
  if (any(ok)) {
    d <- max(abs(vx[ok] - vy[ok]))
    if (!is.finite(d) || d > tol) return(FALSE)
  }
  TRUE
}

melt_all_engines <- function(n_rows, n_id, n_val, tol = 1e-12) {

  id_cols    <- paste0("id", seq_len(n_id))
  value_cols <- paste0("v",  seq_len(n_val))

  df <- make_wide_input(n_rows, n_id, n_val)

  df_dt <- as.data.table(df)
  pd_df <- r_to_py(df)
  pl_df <- polars$DataFrame(df)
  ddf   <- dask_dataframe$from_pandas(df, npartitions = 4L)

  con <- duckdb$connect()
  try(con$unregister("df_cons", fail_if_missing = TRUE), silent = TRUE)
  con$register("df_cons", df)
  sql_unpivot <- sprintf(
    "SELECT %s, variable, value FROM df_cons UNPIVOT (value FOR variable IN (%s));",
    paste(id_cols, collapse = ", "),
    paste(sprintf("'%s'", value_cols), collapse = ", ")
  )

  res <- list()

  res$reshape2 <- canonicalize_long(
    reshape2::melt(df, id.vars = id_cols), id_cols = id_cols)

  res$dataprep <- canonicalize_long(
    dataprep::melt(df, id.vars = id_cols), id_cols = id_cols)

  res$data.table <- canonicalize_long(
    as.data.frame(data.table::melt(df_dt, id.vars = id_cols,
                                   variable.name = "variable",
                                   value.name    = "value")),
    id_cols = id_cols)

  res$tidyr <- canonicalize_long(
    as.data.frame(tidyr::pivot_longer(df, cols = -seq_along(id_cols),
                                      names_to   = "variable",
                                      values_to  = "value",
                                      cols_vary  = "slowest")),
    id_cols = id_cols)

  res$pandas <- canonicalize_long(
    py_pull(pandas$melt(pd_df, id_vars = id_cols,
                        var_name = "variable", value_name = "value")),
    id_cols = id_cols)

  res$polars <- canonicalize_long(
    py_pull(pl_df$unpivot(index         = id_cols,
                          variable_name = "variable",
                          value_name    = "value")$to_pandas()),
    id_cols = id_cols)

  res$dask <- canonicalize_long(
    py_pull(ddf$melt(id_vars = id_cols,
                     var_name = "variable", value_name = "value")$compute()),
    id_cols = id_cols)

  res$duckdb <- canonicalize_long(
    py_pull(con$sql(sql_unpivot)$df()),
    id_cols = id_cols)

  con$close()

  ref <- res$reshape2

  cat(sprintf("\n[melt consistency] rows=%s %s val=%d mixed=%s\n",
              format(n_rows, big.mark = ","), id_desc(n_id), n_val,
              as.character(mixed_types_enabled() && n_id >= 2L)))

  for (nm in names(res)) {
    ok <- compare_long(res[[nm]], ref, id_cols = id_cols, tol = tol)
    cat(sprintf("  %-12s : %s\n", nm, ifelse(ok, "PASS", "FAIL")))
  }

  nms      <- names(res)
  all_pass <- TRUE
  for (i in seq_along(nms)) {
    for (j in seq_along(nms)) {
      if (i >= j) next
      ok <- compare_long(res[[nms[i]]], res[[nms[j]]],
                         id_cols = id_cols, tol = tol)
      if (!ok) {
        cat(sprintf("  FAIL: %s vs %s\n", nms[i], nms[j]))
        all_pass <- FALSE
      }
    }
  }
  if (all_pass) cat("  pairwise: all consistent\n")

  invisible(list(results = res, all_pass = all_pass))
}

if (.run_bench) {
  set.seed(123)
  invisible(gc()); melt_all_engines(1000L,  n_id = 1L,  n_val = 9L)
  invisible(gc()); melt_all_engines(1000L,  n_id = 2L,  n_val = 5L)
  invisible(gc()); melt_all_engines(10000L, n_id = 10L, n_val = 10L)
  invisible(gc()); melt_all_engines(100000L, n_id = 1L, n_val = 9L)
}


# ============================================================================
# Section 4 -- 8-engine consistency check for dcast
# ============================================================================
suppressPackageStartupMessages({
  library(reticulate); library(data.table)
  library(reshape2);   library(tidyr)
  library(dplyr);      library(dataprep)
})

pandas         <- import("pandas")
polars         <- import("polars")
dask_dataframe <- import("dask.dataframe")
duckdb         <- import("duckdb")
py_run_string("import warnings; warnings.filterwarnings('ignore')")

canonicalize_wide <- function(x, id_cols) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  missing <- setdiff(id_cols, names(x))
  if (length(missing) > 0)
    stop(sprintf("Missing id columns: %s", paste(missing, collapse = ", ")))

  for (nm in id_cols) {
    if (is.character(x[[nm]]) || is.factor(x[[nm]])) {
      x[[nm]] <- as.character(x[[nm]])
    } else {
      x[[nm]] <- as.numeric(x[[nm]])
    }
  }

  ord <- do.call(order, x[id_cols])
  x   <- x[ord, , drop = FALSE]
  rownames(x) <- NULL

  other <- setdiff(names(x), id_cols)
  x     <- x[, c(id_cols, sort(other)), drop = FALSE]
  x
}

compare_wide <- function(x, y, id_cols, tol = 1e-12) {
  if (!identical(dim(x), dim(y)))     return(FALSE)
  if (!identical(names(x), names(y))) return(FALSE)

  for (nm in id_cols) {
    xc <- x[[nm]]; yc <- y[[nm]]
    if (is.character(xc) || is.factor(xc) ||
        is.character(yc) || is.factor(yc)) {
      a <- as.character(xc); b <- as.character(yc)
      if (any(xor(is.na(a), is.na(b)))) return(FALSE)
      if (!identical(a, b)) return(FALSE)
    } else {
      a <- as.numeric(xc); b <- as.numeric(yc)
      if (any(xor(is.na(a), is.na(b))))        return(FALSE)
      if (any(abs(a - b) > tol, na.rm = TRUE)) return(FALSE)
    }
  }

  val_cols <- setdiff(names(x), id_cols)
  for (nm in val_cols) {
    vx <- as.numeric(x[[nm]]); vy <- as.numeric(y[[nm]])
    if (any(xor(is.na(vx), is.na(vy)))) return(FALSE)
    ok <- is.finite(vx) & is.finite(vy)
    if (any(ok)) {
      d <- max(abs(vx[ok] - vy[ok]))
      if (!is.finite(d) || d > tol) return(FALSE)
    }
  }
  TRUE
}

dcast_all_engines <- function(n_rows, n_id, n_val, tol = 1e-12) {

  id_cols    <- paste0("id", seq_len(n_id))
  value_cols <- paste0("v",  seq_len(n_val))

  wide <- make_wide_input(n_rows, n_id, n_val)
  wide[[1L]] <- seq_len(n_rows)
  long <- reshape2::melt(wide,
                         id.vars          = id_cols,
                         variable.name    = "variable",
                         value.name       = "value",
                         factorsAsStrings = FALSE)
  long$variable <- as.character(long$variable)

  long_dt <- as.data.table(long)
  long_pd <- r_to_py(long)
  long_pl <- polars$DataFrame(long)

  con <- duckdb$connect()
  try(con$unregister("long_cons", fail_if_missing = TRUE), silent = TRUE)
  con$register("long_cons", long)
  sql_pivot <- sprintf(
    "PIVOT long_cons ON variable USING FIRST(value) GROUP BY %s;",
    paste(id_cols, collapse = ", ")
  )

  pl_pivot <- function() {
    tryCatch(
      long_pl$pivot(index = id_cols, on = "variable", values = "value"),
      error = function(e)
        long_pl$pivot(index = id_cols, columns = "variable", values = "value")
    )
  }

  fml <- as.formula(paste(paste(id_cols, collapse = "+"), "~ variable"))

  res <- list()

  res$reshape2 <- canonicalize_wide(
    reshape2::dcast(long, fml, value.var = "value"), id_cols)

  res$dataprep <- canonicalize_wide(
    dataprep::dcast(long, id = id_cols,
                       variable = "variable", value = "value"),
    id_cols)

  res$data.table <- canonicalize_wide(
    as.data.frame(data.table::dcast(long_dt, fml, value.var = "value")),
    id_cols)

  res$tidyr <- canonicalize_wide(
    as.data.frame(tidyr::pivot_wider(long,
                                     id_cols     = all_of(id_cols),
                                     names_from  = variable,
                                     values_from = value)),
    id_cols)

  pd_res <- long_pd$pivot(index   = id_cols,
                          columns = "variable",
                          values  = "value")$reset_index()
  res$pandas <- canonicalize_wide(py_pull(pd_res), id_cols)

  res$polars <- canonicalize_wide(
    py_pull(pl_pivot()$to_pandas()), id_cols)

  id_cols_lit <- paste0(
    "[",
    paste(sprintf("'%s'", id_cols), collapse = ", "),
    "]"
  )

  py$tmp_long <- r_to_py(long)
  py_run_string(sprintf("
import dask.dataframe as dd
import pandas as pd

ddf = dd.from_pandas(tmp_long, npartitions=4)
pdf = ddf.compute()

wide_pdf = pdf.pivot_table(
    index    = %s,
    columns  = 'variable',
    values   = 'value',
    aggfunc  = 'first'
).reset_index()

wide_pdf.columns.name = None
out = wide_pdf.to_dict(orient='list')
", id_cols_lit))

  res$dask <- canonicalize_wide(
    as.data.frame(py$out, stringsAsFactors = FALSE), id_cols)
  py$tmp_long <- NULL
  py$out      <- NULL

  res$duckdb <- canonicalize_wide(
    py_pull(con$sql(sql_pivot)$df()), id_cols)

  con$close()

  ref <- res$reshape2

  cat(sprintf("\n[dcast consistency] n_long=%s %s levels=%d mixed=%s\n",
              format(n_rows * n_val, big.mark = ","),
              id_desc(n_id), n_val,
              as.character(mixed_types_enabled() && n_id >= 2L)))

  for (nm in names(res)) {
    ok <- compare_wide(res[[nm]], ref, id_cols = id_cols, tol = tol)
    cat(sprintf("  %-12s : %s\n", nm, ifelse(ok, "PASS", "FAIL")))
  }

  nms      <- names(res)
  all_pass <- TRUE
  for (i in seq_along(nms)) {
    for (j in seq_along(nms)) {
      if (i >= j) next
      ok <- compare_wide(res[[nms[i]]], res[[nms[j]]],
                         id_cols = id_cols, tol = tol)
      if (!ok) {
        cat(sprintf("  FAIL: %s vs %s\n", nms[i], nms[j]))
        all_pass <- FALSE
      }
    }
  }
  if (all_pass) cat("  pairwise: all consistent\n")

  invisible(list(results = res, all_pass = all_pass))
}

if (.run_bench) {
  set.seed(123)
  invisible(gc()); dcast_all_engines(1000L,   n_id = 2L,  n_val = 5L)
  invisible(gc()); dcast_all_engines(1000L,   n_id = 1L,  n_val = 50L)
  invisible(gc()); dcast_all_engines(5000L,   n_id = 10L, n_val = 10L)
  invisible(gc()); dcast_all_engines(100000L, n_id = 1L,  n_val = 10L)
}


# ============================================================================
# Post-processing helper for bench tables
# ============================================================================
add_relative_cols <- function(sm) {
  pos_mean   <- sm$mean[is.finite(sm$mean)     & sm$mean   > 0]
  pos_median <- sm$median[is.finite(sm$median) & sm$median > 0]
  base_mean   <- if (length(pos_mean)   > 0) min(pos_mean)   else NA_real_
  base_median <- if (length(pos_median) > 0) min(pos_median) else NA_real_

  safe_ratio <- function(x, base) {
    if (!is.finite(base) || base <= 0) return(rep(NA_real_, length(x)))
    out <- x / base
    out[!is.finite(out)] <- NA_real_
    out
  }
  sm$relative_mean   <- ifelse(sm$skipped, NA_real_,
                               safe_ratio(sm$mean,   base_mean))
  sm$relative_median <- ifelse(sm$skipped, NA_real_,
                               safe_ratio(sm$median, base_median))
  sm
}


# ============================================================================
# Section 5 -- adaptive per-tool benchmark for melt
# ============================================================================
source(system.file("benchmark_helpers.R", package = "dataprep"))

reticulate::py_run_string("
def bench_melt_pandas():
    return bench_pd_df.melt(id_vars=bench_id_cols,
                            var_name='variable', value_name='value')
def bench_melt_polars():
    return bench_pl_df.unpivot(index=bench_id_cols,
                               variable_name='variable', value_name='value')
def bench_melt_dask():
    return bench_ddf.melt(id_vars=bench_id_cols,
                          var_name='variable',
                          value_name='value').compute()
def bench_melt_duckdb():
    return bench_con.sql(bench_sql).df()
")

melt_py_call <- function(code) {
  reticulate::py_eval(code, convert = FALSE)
}

prepare_melt_inputs <- function(n_rows, n_id, n_val) {
  id_cols    <- paste0("id", seq_len(n_id))
  value_cols <- paste0("v",  seq_len(n_val))
  n_cols     <- n_id + n_val

  df <- make_wide_input(n_rows, n_id, n_val)

  df_dt <- as.data.table(df)
  pd_df <- reticulate::r_to_py(df)
  pl_df <- polars$DataFrame(df)
  ddf   <- dask_dataframe$from_pandas(df, npartitions = 4L)

  con <- duckdb$connect()
  try(con$unregister("df_bench", fail_if_missing = TRUE), silent = TRUE)
  con$register("df_bench", df)
  sql_unpivot <- sprintf(
    "SELECT %s, variable, value FROM df_bench UNPIVOT (value FOR variable IN (%s));",
    paste(id_cols, collapse = ", "),
    paste(sprintf("'%s'", value_cols), collapse = ", ")
  )

  list(
    df             = df,
    df_dt          = df_dt,
    pd_df          = pd_df,
    pl_df          = pl_df,
    ddf            = ddf,
    con            = con,
    id_cols        = id_cols,
    value_cols     = value_cols,
    id_cols_py     = reticulate::r_to_py(id_cols),
    sql_unpivot    = sql_unpivot,
    sql_unpivot_py = reticulate::r_to_py(sql_unpivot),
    n_rows         = n_rows,
    n_id           = n_id,
    n_val          = n_val,
    n_cols         = n_cols
  )
}

run_melt_bench <- function(inputs, label = "",
                           csv_path = .MELT_CSV) {

  df             <- inputs$df
  df_dt          <- inputs$df_dt
  pd_df          <- inputs$pd_df
  pl_df          <- inputs$pl_df
  ddf            <- inputs$ddf
  con            <- inputs$con
  id_cols        <- inputs$id_cols
  id_cols_py     <- inputs$id_cols_py
  sql_unpivot_py <- inputs$sql_unpivot_py
  n_rows         <- inputs$n_rows
  n_id           <- inputs$n_id
  n_val          <- inputs$n_val
  n_cols         <- inputs$n_cols

  if (!nzchar(label))
    label <- sprintf("Melt: rows=%s, %s + %d val",
                     format(n_rows, big.mark = ","), id_desc(n_id), n_val)

  cat(sprintf("\n%s\n[%s] mixed=%s\n%s\n",
              strrep("=", 100), label,
              as.character(mixed_types_enabled() && n_id >= 2L),
              strrep("=", 100)))

  expected_output_bytes <-
    as.numeric(n_rows) * as.numeric(n_val) *
    (12 + 4 * as.numeric(n_id))
  sentinel <- sentinel_alloc(sentinel_mult() * expected_output_bytes)
  gc(verbose = FALSE, full = TRUE)
  py_gc_collect()

  main <- reticulate::import_main()
  main$bench_pd_df   <- pd_df
  main$bench_pl_df   <- pl_df
  main$bench_ddf     <- ddf
  main$bench_con     <- con
  main$bench_id_cols <- id_cols_py
  main$bench_sql     <- sql_unpivot_py

  tools <- list(
    reshape2   = function() reshape2::melt(df, id.vars = id_cols),
    data.table = function() data.table::melt(df_dt, id.vars = id_cols),
    tidyr      = function() tidyr::pivot_longer(df, cols = -seq_along(id_cols)),
    dataprep   = function() dataprep::melt(df, id.vars = id_cols),
    pandas     = function() melt_py_call("bench_melt_pandas()"),
    polars     = function() melt_py_call("bench_melt_polars()"),
    dask       = function() melt_py_call("bench_melt_dask()"),
    duckdb     = function() melt_py_call("bench_melt_duckdb()")
  )

  cat("\n  Per-tool warmup + timed runs:\n")
  sm_list <- lapply(names(tools), function(nm)
    bench_one(tools[[nm]], nm, family = "melt"))

  main$bench_pd_df   <- NULL
  main$bench_pl_df   <- NULL
  main$bench_ddf     <- NULL
  main$bench_con     <- NULL
  main$bench_id_cols <- NULL
  main$bench_sql     <- NULL

  sm <- do.call(rbind, sm_list)
  sm$n_rows <- n_rows
  sm$n_cols <- n_cols
  sm$n_id   <- n_id
  sm$n_val  <- n_val

  sm <- add_relative_cols(sm)
  sm$label <- label

  sm <- sm[order(sm$skipped, sm$median, na.last = TRUE), ]
  sm <- sm[, c("label", "tool", "n_rows", "n_cols", "n_id", "n_val",
               "times", "first_run_sec", "skipped",
               "min", "lq", "mean", "median", "uq", "max", "neval",
               "gc_sec",
               "relative_mean", "relative_median")]

  cat("\n  Results (sorted by skipped, then median, ms):\n")
  print(sm, digits = 4, row.names = FALSE)

  write_result(sm, csv_path)

  invisible(NULL)
}

run_melt_cell <- function(n_rows, n_id, n_val, label = "",
                          csv_path = .MELT_CSV) {
  inputs <- prepare_melt_inputs(n_rows, n_id, n_val)
  on.exit({
    try(inputs$con$close(), silent = TRUE)
    rm(inputs); invisible(gc())
  }, add = TRUE)
  run_melt_bench(inputs, label = label, csv_path = csv_path)
}


# ============================================================================
# Section 6 -- adaptive per-tool benchmark for dcast
# ============================================================================
source(system.file("benchmark_helpers.R", package = "dataprep"))

reticulate::py_run_string("
def pivot_pandas(long_pd, id_cols):
    return long_pd.pivot(index=id_cols, columns='variable',
                         values='value').reset_index()
def pivot_polars(long_pl, id_cols):
    try:
        return long_pl.pivot(index=id_cols, on='variable',
                             values='value').to_pandas()
    except TypeError:
        return long_pl.pivot(index=id_cols, columns='variable',
                             values='value').to_pandas()
def pivot_dask(ddf, id_cols):
    pdf = ddf.compute()
    return pdf.pivot_table(index=id_cols, columns='variable',
                           values='value', aggfunc='first').reset_index()
def pivot_duckdb(con, sql):
    return con.sql(sql).df()
")

dcast_py_call <- function(fn, ...) reticulate::py_call(fn, ...)

prepare_dcast_inputs <- function(n_long, n_id, n_levels) {
  id_cols <- paste0("id", seq_len(n_id))

  long   <- make_long(n_long, n_id, n_levels)
  n_long <- nrow(long)

  long_dt <- as.data.table(long)
  long_pd <- reticulate::r_to_py(long)
  long_pl <- polars$DataFrame(long)
  ddf     <- dask_dataframe$from_pandas(long, npartitions = 4L)

  con <- duckdb$connect()
  try(con$unregister("long_bench", fail_if_missing = TRUE), silent = TRUE)
  con$register("long_bench", long)
  sql_pivot <- sprintf(
    "PIVOT long_bench ON variable USING FIRST(value) GROUP BY %s;",
    paste(id_cols, collapse = ", ")
  )

  list(
    long           = long,
    long_dt        = long_dt,
    long_pd        = long_pd,
    long_pl        = long_pl,
    ddf            = ddf,
    con            = con,
    id_cols        = id_cols,
    sql_pivot      = sql_pivot,
    sql_pivot_py   = reticulate::r_to_py(sql_pivot),
    id_cols_py     = reticulate::r_to_py(id_cols),
    n_long         = n_long,
    n_id           = n_id,
    n_levels       = n_levels
  )
}

run_dcast_bench <- function(inputs, label = "",
                            csv_path = .DCAST_CSV) {

  long           <- inputs$long
  long_dt        <- inputs$long_dt
  long_pd        <- inputs$long_pd
  long_pl        <- inputs$long_pl
  ddf            <- inputs$ddf
  con            <- inputs$con
  id_cols        <- inputs$id_cols
  sql_pivot_py   <- inputs$sql_pivot_py
  id_cols_py     <- inputs$id_cols_py
  n_long         <- inputs$n_long
  n_id           <- inputs$n_id
  n_levels       <- inputs$n_levels

  if (!nzchar(label))
    label <- sprintf("Dcast: n_long=%s, %s + %d lvl",
                     format(n_long, big.mark = ","), id_desc(n_id), n_levels)

  cat(sprintf("\n%s\n[%s] mixed=%s\n%s\n",
              strrep("=", 100), label,
              as.character(mixed_types_enabled() && n_id >= 2L),
              strrep("=", 100)))

  expected_output_bytes <-
    as.numeric(n_long) * 8 +
    as.numeric(n_long %/% max(1L, n_levels)) * as.numeric(n_id) * 8
  sentinel <- sentinel_alloc(sentinel_mult() * expected_output_bytes)
  gc(verbose = FALSE, full = TRUE)
  py_gc_collect()

  py_pivot_pandas <- reticulate::py$pivot_pandas
  py_pivot_polars <- reticulate::py$pivot_polars
  py_pivot_dask   <- reticulate::py$pivot_dask
  py_pivot_duckdb <- reticulate::py$pivot_duckdb

  fml <- as.formula(paste(paste(id_cols, collapse = "+"), "~ variable"))

  tools <- list(
    reshape2   = function()
      reshape2::dcast(long, fml, value.var = "value"),
    data.table = function()
      data.table::dcast(long_dt, fml, value.var = "value"),
    tidyr      = function()
      tidyr::pivot_wider(long,
                         id_cols     = dplyr::all_of(id_cols),
                         names_from  = variable,
                         values_from = value),
    dataprep   = function()
      dataprep::dcast(long, id = id_cols,
                         variable = "variable", value = "value"),
    pandas     = function()
      dcast_py_call(py_pivot_pandas, long_pd, id_cols_py),
    polars     = function()
      dcast_py_call(py_pivot_polars, long_pl, id_cols_py),
    dask       = function()
      dcast_py_call(py_pivot_dask, ddf, id_cols_py),
    duckdb     = function()
      dcast_py_call(py_pivot_duckdb, con, sql_pivot_py)
  )

  cat("\n  Per-tool warmup + timed runs:\n")
  sm_list <- lapply(names(tools), function(nm)
    bench_one(tools[[nm]], nm, family = "dcast"))

  sm <- do.call(rbind, sm_list)
  sm$n_long   <- n_long
  sm$n_id     <- n_id
  sm$n_levels <- n_levels

  sm <- add_relative_cols(sm)
  sm$label <- label

  sm <- sm[order(sm$skipped, sm$median, na.last = TRUE), ]
  sm <- sm[, c("label", "tool", "n_long", "n_id", "n_levels",
               "times", "first_run_sec", "skipped",
               "min", "lq", "mean", "median", "uq", "max", "neval",
               "gc_sec",
               "relative_mean", "relative_median")]

  cat("\n  Results (sorted by skipped, then median, ms):\n")
  print(sm, digits = 4, row.names = FALSE)

  write_result(sm, csv_path)

  invisible(NULL)
}

run_dcast_cell <- function(n_long, n_id, n_levels, label = "",
                           csv_path = .DCAST_CSV) {
  inputs <- prepare_dcast_inputs(n_long, n_id, n_levels)
  on.exit({
    try(inputs$con$close(), silent = TRUE)
    rm(inputs); invisible(gc())
  }, add = TRUE)
  run_dcast_bench(inputs, label = label, csv_path = csv_path)
}


# ============================================================================
# Driver -- run the full sweep when DATAPREP_RUN_BENCHMARK=1
# ============================================================================
if (.run_bench) {

  # Fresh CSV for this invocation: every cell appends to it.
  unlink(c(.MELT_CSV, .DCAST_CSV))

  # ---- melt sweep ---------------------------------------------------------
  set.seed(123)

  bench_reset_disabled("melt")
  for (nr in 10^(3:8)) {
    run_melt_cell(nr, n_id = 1, n_val = 9,
                  label = sprintf("Melt: rows=%s, %s + 9 val",
                                  format(nr, big.mark = ","), id_desc(1L)))
  }

  bench_reset_disabled("melt")
  for (nr in 10^(3:7)) {
    run_melt_cell(nr, n_id = 10, n_val = 9,
                  label = sprintf("Melt: rows=%s, %s + 9 val",
                                  format(nr, big.mark = ","), id_desc(10L)))
  }

  bench_reset_disabled("melt")
  for (nc in 10^(1:4)) {
    run_melt_cell(1e3, n_id = 1, n_val = nc,
                  label = sprintf("Melt: 1e3 rows, %s + %d val",
                                  id_desc(1L), nc))
  }

  bench_reset_disabled("melt")
  for (nc in 10^(1:4)) {
    run_melt_cell(1e3, n_id = 10, n_val = nc,
                  label = sprintf("Melt: 1e3 rows, %s + %d val",
                                  id_desc(10L), nc))
  }

  # ---- dcast sweep --------------------------------------------------------
  bench_reset_disabled("dcast")
  for (nl in 10^(3:8)) {
    run_dcast_cell(nl, n_id = 1L, n_levels = 10L,
                   label = sprintf("Dcast: n_long=%s, %s + 10 lvl",
                                   format(nl, big.mark = ","), id_desc(1L)))
  }

  bench_reset_disabled("dcast")
  for (lv in c(100L, 1000L, 10000L)) {
    run_dcast_cell(1e6, n_id = 1L, n_levels = lv,
                   label = sprintf("Dcast: n_long=%s, %s + %d lvl",
                                   format(1e6, big.mark = ","),
                                   id_desc(1L), lv))
  }

  bench_reset_disabled("dcast")
  for (ni in c(2L, 10L, 100L)) {
    run_dcast_cell(1e6, n_id = ni, n_levels = 10L,
                   label = sprintf("Dcast: n_long=%s, %s + 10 lvl",
                                   format(1e6, big.mark = ","),
                                   id_desc(ni)))
  }

  bench_reset_disabled("dcast")
  for (nl in setdiff(10^(4:8), 1e6)) {
    run_dcast_cell(nl, n_id = 1L, n_levels = 100L,
                   label = sprintf("Dcast: n_long=%s, %s + 100 lvl",
                                   format(nl, big.mark = ","), id_desc(1L)))
  }
}
