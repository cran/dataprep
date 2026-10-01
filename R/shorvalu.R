#' Short-period interpolation
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to interpolate. If \code{NULL}, all numeric columns are used.
#' @param intervals Time gap (in \code{units}) that defines a short period.
#' @param units Time unit for \code{intervals}.
#' @param date_col Time column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#' @return A data frame with missing values filled.
#' @export
shorvalu <- function(data, cols = NULL, intervals = 30, units = "mins",
                     date_col = NULL, cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))

  # Vector input: fall back to plain linear interpolation.
  if (is.vector(data) && !is.list(data)) {
    data <- lin_interp_cpp(data)
    if (verbose) cat("Vector interpolation completed\n")
    return(data)
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name
  tv <- data[[date_name]]
  if (!inherits(tv, c("POSIXct", "Date")))
    stop("Time column must be POSIXct or Date")

  orig_na <- sum(is.na(data[, idx, drop = FALSE]))

  # Convert `intervals` (in `units`) into seconds once.
  units_sec <- switch(units,
                      "secs"  = 1,
                      "mins"  = 60,
                      "hours" = 3600,
                      "days"  = 86400,
                      "weeks" = 604800)
  intervals_sec <- intervals * units_sec

  # POSIXct stores seconds since epoch, Date stores days. Normalise both to
  # seconds so the C++ segmentation sees a single scale.
  time_sec <- if (inherits(tv, "Date")) as.numeric(tv) * 86400
              else                       as.numeric(tv)

  # Segment the series at every gap > intervals_sec. This replaces the
  # previous R-level diff/which/diff chain, which was O(n) in R and
  # dominated runtime on large POSIXct vectors.
  seg    <- shorvalu_segment_cpp(time_sec, intervals_sec)
  starts <- seg$starts         # 1-based, matches which()
  lens   <- seg$lens

  mat <- to_numeric_matrix(data, idx)
  n_threads <- if (is.null(cores)) 0L else as.integer(cores)

  # shorvalu_fill_cpp expects 0-based `starts`.
  shorvalu_fill_cpp(mat, as.integer(starts - 1L), as.integer(lens),
                    n_threads = n_threads)
  data[, idx] <- as.data.frame(mat)

  after_na <- sum(is.na(data[, idx, drop = FALSE]))

  if (verbose) {
    cat("Missing values left in selected variables:", after_na, "\n")
    cat(orig_na - after_na,
        "missing values are replaced by shorvalu interpolation\n")
    cat("Time used by shorvalu:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  data
}