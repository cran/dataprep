#' Traditional percentile-based outlier removal
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process.
#' @param group Grouping column.
#' @param top,bottom Percentile thresholds.
#' @param by,half Time parameters.
#' @param date_col Time column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#' @return A data frame with outliers removed.
#' @export
percoutl <- function(data, cols = NULL, group = NULL, top = .995,
                     bottom = .0025, by = "min", half = 30,
                     date_col = NULL, cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name

  if (is.vector(data) && !is.list(data)) {
    # Vector: only outlier marking, no observation deletion
    # because there is no time axis.
    return(mark_outliers_cpp(data, top, 0, 0, bottom, 0, 0, FALSE))
  }

  a <- data
  idx <- resolve_cols(a, cols)
  if (!is.null(group)) {
    group_idx <- if (is.character(group)) which(names(a) == group) else group
    if (group_idx %in% idx) idx <- setdiff(idx, group_idx)
  }

  if (is.null(group)) {
    a[, idx] <- lapply(a[, idx, drop = FALSE], function(x)
      mark_outliers_cpp(x, top, 0, 0, bottom, 0, 0, FALSE))
    a <- obsedele(a, cols = idx, by = by, half = half,
                  date_col = date_col, cores = cores)
  } else {
    group_col <- if (is.character(group)) group else names(a)[group]
    for (k in unique(a[[group_col]])) {
      rows <- which(a[[group_col]] == k)
      a[rows, idx] <- lapply(a[rows, idx, drop = FALSE], function(x)
        mark_outliers_cpp(x, top, 0, 0, bottom, 0, 0, FALSE))
    }
    a <- obsedele(a, cols = idx, group = group_col, by = by, half = half,
                  date_col = date_col, cores = cores)
  }

  # FIX: 0.1.5 called obsedele() a second time unconditionally
  # after the branch above. The second call used the same arguments
  # as the first, and obsedele() is idempotent, so it did not change
  # the result --- it only doubled the runtime of percoutl().

  if (verbose) {
    cat(sum(is.na(a[idx])) -
          sum(is.na(data[data[[date_name]] %in% a[[date_name]], idx])),
        "values are regarded as outliers and deleted excluding those in deleted observations\n")
    cat(nrow(data) - nrow(a),
        "observations are deleted in total by percoutl\n")
    cat("Time used by percoutl:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  a
}