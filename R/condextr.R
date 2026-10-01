#' Conditional extremum outlier removal
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param group Grouping column.
#' @param top,top.error,top.magnitude Top threshold parameters.
#' @param bottom,bottom.error,bottom.magnitude Bottom threshold parameters.
#' @param interval Number of marking steps between observation deletions.
#' @param by,half Time unit and half-window for observation deletion.
#' @param times Number of observation deletion rounds.
#' @param date_col Time column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#' @return Data with outliers removed.
#' @export
condextr <- function(data, cols = NULL, group = NULL, top = .995,
                     top.error = .1, top.magnitude = .2,
                     bottom = .0025, bottom.error = .2,
                     bottom.magnitude = .4,
                     interval = 10, by = "min", half = 30, times = 10,
                     date_col = NULL, cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  # Vector input: no time axis, so only outlier marking is possible.
  if (is.vector(data) && !is.list(data)) {
    for (j in seq_len(times)) {
      for (i in seq_len(interval)) {
        data <- mark_outliers_cpp(data, top, top.error, top.magnitude,
                                  bottom, bottom.error, bottom.magnitude,
                                  TRUE)
      }
    }
    return(data)
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name

  group_idx <- NULL
  if (is.null(group)) {
    warning("Without a group column, the deleted values may congregate ",
            "in a minority of periods. Consider supplying `group`.",
            call. = FALSE)
  }
  if (!is.null(group)) {
    group_idx <- if (is.character(group)) which(names(data) == group)
                 else as.integer(group)
    if (length(group_idx) != 1 || is.na(group_idx) ||
        group_idx < 1 || group_idx > ncol(data))
      stop("Invalid group column")
    if (group_idx %in% idx)
      stop("group column should not be within cols")
  }

  time_vec <- data[[date_name]]
  if (inherits(time_vec, "POSIXct")) {
    time_sec <- as.numeric(time_vec)
  } else if (inherits(time_vec, "Date")) {
    time_sec <- as.numeric(time_vec) * 86400
  } else {
    stop("Time column must be POSIXct or Date")
  }

  tu <- parse_time_unit(by)
  step_sec      <- tu$step_sec
  threshold_sec <- half * 60

  if (!is.null(group_idx)) {
    group_vec <- data[[group_idx]]
    group_int <- as.integer(factor(group_vec))
    group_int[is.na(group_int)] <- 0L
  } else {
    group_int <- rep(0L, nrow(data))
  }

  mat <- to_numeric_matrix(data, idx)
  n_threads <- if (is.null(cores)) 0L else as.integer(cores)

  # The full condextr loop (mark + obsedele, repeated `times` times,
  # with `interval` marking rounds between deletions) is executed
  # entirely in C++ via condextr_cpp. This avoids any intermediate
  # matrices crossing the R/C++ boundary.
  res <- condextr_cpp(
    time_sec      = time_sec,
    group_int     = group_int,
    x             = mat,
    step_sec      = step_sec,
    half          = half,
    threshold_sec = threshold_sec,
    top           = top,
    toperr        = top.error,
    topmag        = top.magnitude,
    bottom        = bottom,
    boterr        = bottom.error,
    botmag        = bottom.magnitude,
    interval      = as.integer(interval),
    times         = as.integer(times),
    n_threads     = n_threads
  )

  keep      <- res$keep
  final_mat <- res$mat

  result <- data[keep, , drop = FALSE]
  result[, idx] <- as.data.frame(final_mat)
  rownames(result) <- NULL

  if (inherits(data, "grouped_df") && requireNamespace("dplyr", quietly = TRUE)) {
    group_vars <- dplyr::group_vars(data)
    if (length(group_vars) > 0) {
      result <- dplyr::group_by(result,
                                dplyr::across(dplyr::all_of(group_vars)))
    } else {
      result <- dplyr::ungroup(result)
    }
  }

  if (verbose) {
    kept_idx     <- which(keep)
    orig_subset  <- data[kept_idx, idx, drop = FALSE]
    final_subset <- result[, idx, drop = FALSE]
    outliers_marked <- sum(is.na(final_subset) & !is.na(orig_subset))
    total_deleted   <- nrow(data) - nrow(result)
    cat(outliers_marked,
        "values are regarded as outliers and deleted excluding those in deleted observations\n")
    cat(total_deleted,
        "observations are deleted in total by condextr (after",
        interval * times, "cycles)\n")
    cat("Time used by condextr:",
        format(Sys.time() - t0, digits = 3), "\n")
  }

  result
}