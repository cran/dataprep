#' Simulate preprocessing and report changes
#' @param data A data frame.
#' @param steps Steps to simulate.
#' @param cols Columns to include.
#' @param group Grouping column.
#' @param date_col Time column.
#' @param fraction Missing fraction threshold.
#' @param top,bottom Percentile thresholds.
#' @param by,half Time parameters.
#' @param method_outlier Outlier method.
#' @param coef Coefficient.
#' @param verbose Logical.
#' @return A list with per-step before/after counts. The function
#'   actually runs \code{varidele}, \code{obsedele}, and
#'   \code{detect_outliers} on a copy of the input and never
#'   modifies the caller's data frame.
#' @export
dry_run <- function(data, steps = c("varidele", "obsedele", "outlier"),
                    cols = NULL, group = NULL, date_col = NULL,
                    fraction = 0.25, top = 0.995, bottom = 0.0025,
                    by = "min", half = 30, method_outlier = "iqr", coef = 1.5,
                    verbose = FALSE) {
  t0 <- Sys.time()
  if (!is.data.frame(data)) stop("data must be a data frame")

  if (is.null(cols)) {
    cols <- which(sapply(data, is.numeric))
  } else {
    cols <- resolve_cols(data, cols)
  }

  report <- list()
  original_n    <- nrow(data)
  original_cols <- ncol(data)

  for (step in steps) {
    switch(
      step,
      "varidele" = {
        # Operate on column NAMES, not integer indices. This guarantees
        # that every column not in `cols` (including the time column and
        # any grouping/character columns) is preserved automatically.
        col_names   <- names(data)[cols]
        mat         <- as.matrix(data[, cols, drop = FALSE])
        frac        <- colMeans(is.na(mat))
        keep_mask   <- frac < fraction
        kept_names  <- col_names[keep_mask]
        drop_names  <- col_names[!keep_mask]

        report$varidele <- list(
          removed_columns = drop_names,
          removed_count   = length(drop_names)
        )

        # Drop by name; keep everything else.
        keep_all <- setdiff(names(data), drop_names)
        data     <- data[, keep_all, drop = FALSE]

        # Remap `cols` to the new positions.
        cols <- match(kept_names, names(data))
        cols <- cols[!is.na(cols)]
      },
      "obsedele" = {
        if (length(cols) == 0) {
          report$obsedele <- list(
            rows_before = nrow(data),
            rows_after  = nrow(data),
            removed     = 0L,
            note        = "Skipped: no numeric columns remain."
          )
        } else {
          a <- obsedele(data, cols = cols, group = group,
                        by = by, half = half, date_col = date_col,
                        verbose = FALSE)
          report$obsedele <- list(
            rows_before = nrow(data),
            rows_after  = nrow(a),
            removed     = nrow(data) - nrow(a)
          )
          data <- a
        }
      },
      "outlier" = {
        if (length(cols) == 0) {
          report$outlier <- list(
            na_before = 0L,
            na_after  = 0L,
            added     = 0L,
            note      = "Skipped: no numeric columns remain."
          )
        } else {
          a <- detect_outliers(data, cols = cols, method = method_outlier,
                               top = top, bottom = bottom, coef = coef,
                               group = group, mask_only = FALSE,
                               verbose = FALSE)
          na_before <- sum(is.na(data[, cols, drop = FALSE]))
          na_after  <- sum(is.na(a   [, cols, drop = FALSE]))
          report$outlier <- list(
            na_before = na_before,
            na_after  = na_after,
            added     = na_after - na_before
          )
          data <- a
        }
      },
      stop("Unknown step: ", step)
    )
  }

  report$original_n    <- original_n
  report$original_ncol <- original_cols
  report$final_n       <- nrow(data)
  report$final_ncol    <- ncol(data)

  if (verbose) cat("Dry run completed.\n")
  report
}
