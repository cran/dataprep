#' Resample time series
#' @param data A data frame.
#' @param cols Columns to aggregate.
#' @param date_col Time column.
#' @param period \code{"hour"}, \code{"day"}, or \code{"month"}.
#' @param fun Aggregation function.
#' @param na.rm Remove NA before aggregating.
#' @param verbose Logical.
#' @return A data frame with aggregated values.
#' @export
resample_time <- function(data, cols = NULL, date_col = NULL,
                          period = "hour", fun = "mean", na.rm = TRUE,
                          verbose = FALSE) {
  t0 <- Sys.time()
  fun <- match.arg(fun, c("mean", "sum", "min", "max", "median", "sd"))
  if (!is.data.frame(data)) stop("resample_time requires a data frame")
  if (is.vector(data) && !is.list(data)) {
    stop("resample_time requires a data frame with a time column")
  }

  idx <- resolve_numeric_cols(data, cols)
  check_numeric_cols(data, idx)

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name
  time_vec <- data[[date_name]]
  if (!inherits(time_vec, c("POSIXct", "Date"))) {
    stop("Time column must be POSIXct or Date")
  }

  if (period == "hour") {
    grp <- as.POSIXct(format(time_vec, "%Y-%m-%d %H:00:00"), tz = attr(time_vec, "tzone"))
  } else if (period == "day") {
    grp <- as.Date(format(time_vec, "%Y-%m-%d"))
  } else if (period == "month") {
    grp <- as.Date(format(time_vec, "%Y-%m-01"))
  } else {
    stop("period must be 'hour', 'day', or 'month'")
  }

  agg_list <- list()
  for (j in idx) {
    col_name <- names(data)[j]
    agg <- switch(fun,
                  mean   = aggregate(data[[j]], list(grp), FUN = mean, na.rm = na.rm),
                  sum    = aggregate(data[[j]], list(grp), FUN = sum, na.rm = na.rm),
                  min    = aggregate(data[[j]], list(grp), FUN = min, na.rm = na.rm),
                  max    = aggregate(data[[j]], list(grp), FUN = max, na.rm = na.rm),
                  median = aggregate(data[[j]], list(grp), FUN = median, na.rm = na.rm),
                  sd     = aggregate(data[[j]], list(grp), FUN = sd, na.rm = na.rm)
    )
    names(agg) <- c("time", col_name)
    agg_list[[col_name]] <- agg
  }

  result <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), agg_list)
  result <- result[order(result$time), ]
  rownames(result) <- NULL

  if (verbose) cat("Time resampling completed.\n")
  result
}
