#' One-call data preprocessing pipeline
#' @param data A data frame.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param group Grouping column.
#' @param optimal Logical; use \code{optisolu} to search for optimal parameters.
#' @param interval,times Parameters for \code{condextr}.
#' @param fraction Missing fraction threshold for \code{varidele}.
#' @param top,top.error,top.magnitude,bottom,bottom.error,bottom.magnitude
#'   Outlier thresholds.
#' @param by,half Time parameters.
#' @param intervals Time gap for interpolation.
#' @param date_col Time column.
#' @param cores Number of CPU cores passed to \code{obsedele()},
#'   \code{condextr()}, and \code{optisolu()}. \code{NULL}
#'   (default) lets each backend choose based on data size.
#' @param verbose Logical.
#' @return A preprocessed data frame.
#' @export
dataprep <- function(data, cols = NULL, group = NULL,
                     optimal = FALSE, interval = 10, times = 10,
                     fraction = .25,
                     top = .995, top.error = .1, top.magnitude = .2,
                     bottom = .0025, bottom.error = .2, bottom.magnitude = .4,
                     by = 'min', half = 30, intervals = 30,
                     date_col = NULL, cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  date_info <- resolve_date_col(data, date_col)

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  original_names <- names(data)

  a <- varidele(data, cols = idx, fraction = fraction, verbose = verbose)
  if (is.null(a)) {
    warning("varidele returned NULL: all selected columns were removed ",
            "or all rows are entirely NA.")
    return(NULL)
  }

  retained_names <- intersect(original_names[idx], names(a))
  if (length(retained_names) < 1) {
    if (verbose) cat("No selected variables remain after varidele\n")
    return(NULL)
  }

  if (!is.null(group)) {
    group_name <- if (is.numeric(group)) original_names[group] else group
    if (!(group_name %in% names(a)))
      stop("Group column was removed during variable deletion")
  } else {
    group_name <- NULL
  }

  b <- obsedele(a, cols = retained_names, group = group_name,
                by = by, half = half, date_col = date_col,
                cores = cores, verbose = verbose)

  if (optimal) {
    s <- optisolu(b, cols = retained_names, group = group_name,
                  interval = interval, times = times,
                  top = top, top.error = top.error, top.magnitude = top.magnitude,
                  bottom = bottom, bottom.error = bottom.error,
                  bottom.magnitude = bottom.magnitude,
                  by = by, half = half, date_col = date_col,
                  cores = cores, verbose = verbose)
    opt_rows <- which(s$optimal)
    if (length(opt_rows) > 0) {
      interval <- s$interval[opt_rows[1]]
      times    <- s$times[opt_rows[1]]
      if (verbose)
        cat("Optimal solution found: interval =", interval,
            "times =", times, "\n")
    } else {
      warning("No combination strictly outperformed percoutl; ",
              "falling back to the highest relaindex. ",
              "Consider increasing interval/times for a wider search.")
      best <- which.max(s$relaindex)
      interval <- s$interval[best]
      times    <- s$times[best]
      if (verbose)
        cat("Fallback solution: interval =", interval,
            "times =", times, "\n")
    }
  }

  c <- condextr(b, cols = retained_names, group = group_name,
                interval = interval, times = times,
                top = top, top.error = top.error, top.magnitude = top.magnitude,
                bottom = bottom, bottom.error = bottom.error,
                bottom.magnitude = bottom.magnitude,
                by = by, half = half, date_col = date_col,
                cores = cores, verbose = verbose)

  d <- shorvalu(c, cols = retained_names, intervals = intervals,
                date_col = date_col, verbose = verbose)

  if (verbose) {
    if (optimal)
      cat("Optimal solution found with interval =", interval,
          "and times =", times, "\n")
    cat("Time used by dataprep:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  d
}