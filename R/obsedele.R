#' Delete observations with excessive consecutive missing values
#'
#' @details
#' For every missing value in each selected column, the C++ backend
#' computes the time distance to the nearest non-missing anchor on the
#' left and on the right. A row is deleted when \strong{any} selected column has \strong{both}
#' distances exceed \code{half} minutes. When a run touches the series
#' boundary, the missing side is treated as \code{+Inf}, so boundary
#' rows are only deleted when the surviving side is also too far away.
#'
#' This is a change from dataprep 0.1.5, which collapsed all selected
#' columns into one long vector before computing missing runs. The old
#' approach merged NA runs across columns and over-deleted boundary
#' rows. See \code{vignette("dataprep-migration")} for the upgrade
#' guide.
#'
#' @section Boundary behaviour:
#' The comparison is inclusive: if an anchor is exactly \code{half}
#' minutes away, the row is retained. On the SMEAR I Varrio 2025
#' full-year dataset this rule retains three rows that 0.1.5 removed.
#'
#' @param data A data frame.
#' @param cols Columns to check. If \code{NULL}, all numeric columns
#'   are used.
#' @param group Optional grouping column.
#' @param by Time unit used only to validate the internal \code{step_sec}. The 0.1.8 anchor-based scan does not use a regular grid, so this argument does not affect the result.
#' @param half Half window size in minutes.
#' @param date_col Time column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#'
#' @return A data frame with rows removed.
#'
#' @seealso \code{\link{varidele}}, \code{\link{condextr}},
#'   \code{\link{na_diagnose}}, \code{\link{dataprep}}.
#'
#' @examples
#' df <- data.frame(
#'   date  = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 600,
#'   group = rep(1L, 10),
#'   x     = c(1, NA, NA, NA, 5, NA, NA, 2, NA, 3)
#' )
#' obsedele(df, cols = "x", group = "group", half = 30)
#' @references
#' 1. Example data is from \url{https://smear.avaa.csc.fi/download}.
#'    It includes particle number concentrations in SMEAR I Varrio forest.
#'
#' @author
#' Chun-Sheng Liang <chun-shengliang@qq.com>
#' @export
obsedele <- function(data, cols = NULL, group = NULL, by = "min", half = 30,
                     date_col = NULL, cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name

  group_idx <- NULL
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

  mat_selected <- to_numeric_matrix(data, idx)
  n_threads <- if (is.null(cores)) 0L else as.integer(cores)

  keep <- obsedele_cpp(
    time_sec      = time_sec,
    group_int     = group_int,
    x             = mat_selected,
    step_sec      = step_sec,
    half          = half,
    threshold_sec = threshold_sec,
    n_threads     = n_threads
  )

  result <- data[keep, , drop = FALSE]
  rownames(result) <- NULL
  if (verbose) {
    cat(nrow(data) - nrow(result), "observations are deleted\n")
    cat("Time used by obsedele:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  result
}
