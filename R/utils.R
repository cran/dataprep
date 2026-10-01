resolve_cols <- function(data, cols) {
  if (is.null(cols)) {
    if (is.data.frame(data) || is.matrix(data)) return(seq_len(ncol(data)))
    else return(NULL)
  }
  if (is.character(cols)) {
    if (is.data.frame(data)) idx <- match(cols, names(data))
    else if (is.matrix(data)) idx <- match(cols, colnames(data))
    else stop("Character column names are only supported for data frames or matrices")
    if (any(is.na(idx))) stop("Some column names do not exist")
    return(idx)
  }
  if (is.numeric(cols)) {
    if (is.data.frame(data) || is.matrix(data)) {
      if (any(cols < 1 | cols > ncol(data))) stop("Column indices out of range")
      return(as.integer(cols))
    } else stop("Numeric column indices are only supported for data frames or matrices")
  }
  stop("cols must be NULL, a character vector, or a numeric vector")
}

# Resolve columns that default to "all numeric columns" when NULL.
# This is the correct default for functions that operate on numeric data
# (varidele, obsedele, condextr, shorvalu, winsorize, transform_data,
# filter_*, roll_apply, drift_detect, detrend_ts, remove_diurnal_cycle,
# decompose_ts, create_lags, na_diagnose, log_returns, impute_missing,
# detect_outliers, optisolu, dataprep).
resolve_numeric_cols <- function(data, cols) {
  if (is.null(cols)) {
    if (is.data.frame(data) || is.matrix(data)) {
      is_num <- vapply(
        seq_len(ncol(data)),
        function(j) is.numeric(data[[j]]) ||
                    (is.logical(data[[j]]) && all(is.na(data[[j]]))),
        logical(1)
      )
      return(which(is_num))
    }
    return(NULL)
  }
  resolve_cols(data, cols)
}

resolve_date_col <- function(data, date_col = NULL) {
  if (!is.data.frame(data) && !is.matrix(data))
    stop("Time column is only supported for data frames or matrices")
  col_names <- if (is.data.frame(data)) names(data) else colnames(data)
  if (is.null(date_col)) {
    exact <- which(col_names %in% c("date", "Date", "DATE"))
    if (length(exact) > 0) {
      idx <- exact[1]
    } else {
      approx <- which(grepl("date|Date|DATE", col_names))
      if (length(approx) == 0)
        stop("No time column found; please specify via date_col")
      idx <- approx[1]
    }
  } else {
    if (is.character(date_col)) {
      idx <- match(date_col, col_names)
      if (is.na(idx)) stop("Specified time column name does not exist")
    } else if (is.numeric(date_col)) {
      if (date_col < 1 || date_col > length(col_names))
        stop("Time column index out of range")
      idx <- as.integer(date_col)
    } else stop("date_col must be character or numeric")
  }
  list(idx = idx, name = col_names[idx])
}

check_numeric_cols <- function(data, cols) {
  bad <- character()
  for (j in cols) {
    x <- data[[j]]
    if (is.numeric(x)) next
    if (is.logical(x) && all(is.na(x))) next
    bad <- c(bad, names(data)[j])
  }
  if (length(bad) > 0) {
    stop("The following columns are not numeric: ",
         paste(bad, collapse = ", "))
  }
  invisible(TRUE)
}

to_numeric_matrix <- function(data, cols) {
  check_numeric_cols(data, cols)
  mat <- as.matrix(data[, cols, drop = FALSE])
  storage.mode(mat) <- "double"
  mat
}

# Parse a "by" string such as "min", "5 min", "hour", "2 hours".
# Returns a list with `step_sec` (seconds per step) and `unit_sec`.
parse_time_unit <- function(by) {
  num <- ifelse(grepl("^[A-Za-z]+$", by), 1,
                as.numeric(gsub(".*?([0-9]+).*", "\\1", by)))
  unit_char <- gsub(".*?([a-z]+).*", "\\1", by)
  unit_sec <- switch(unit_char,
                     "secs" = 1, "sec" = 1,
                     "mins" = 60, "min" = 60,
                     "hours" = 3600, "hour" = 3600,
                     "days" = 86400, "day" = 86400,
                     "weeks" = 604800, "week" = 604800,
                     stop("Unsupported time unit: ", unit_char))
  list(num = num, unit_sec = unit_sec, step_sec = num * unit_sec)
}