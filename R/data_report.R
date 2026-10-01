#' Generate a data quality report
#' @param data A data frame.
#' @param cols Columns to include.
#' @param date_col Time column.
#' @param verbose Logical.
#' @return Invisibly, a list with dimensions, types, missing diagnosis, and descriptive statistics.
#' @export
data_report <- function(data, cols = NULL, date_col = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  if (verbose) {
    cat("========== Data Quality Report ==========\n")
    cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")
  }
  types <- sapply(data, function(x) class(x)[1])
  if (verbose) {
    cat("Variable type distribution:\n")
    print(table(types))
    cat("\n")
  }
  if (is.null(cols)) {
    numeric_cols <- which(sapply(data, is.numeric))
  } else {
    numeric_cols <- resolve_cols(data, cols)
  }

  na_res <- NULL
  de_res <- NULL
  if (length(numeric_cols) > 0) {
    na_res <- na_diagnose(data, cols = numeric_cols,
                          date_col = date_col, verbose = FALSE)
    de_res <- descdata(data, cols = numeric_cols, verbose = FALSE)
  }

  if (verbose && length(numeric_cols) > 0) {
    cat("Missing value diagnosis (numeric columns):\n")
    print(na_res)
    cat("\n")
    cat("Descriptive statistics (numeric columns):\n")
    print(de_res)
    cat("\n")
    cat("Time used by data_report:",
        format(Sys.time() - t0, digits = 3), "\n")
  }

  invisible(list(
    dim = c(nrow(data), ncol(data)),
    types = types,
    na_diagnose = na_res,
    desc_stats = de_res
  ))
}
