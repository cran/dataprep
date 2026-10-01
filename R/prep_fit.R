#' Build a preprocessing plan on training data
#' @param data A data frame.
#' @param steps Steps to include.
#' @param cols Columns to process.
#' @param group Grouping column.
#' @param date_col Time column.
#' @param fraction Missing fraction threshold.
#' @param top,bottom Percentile thresholds.
#' @param by,half Time parameters.
#' @param method_outlier Outlier method.
#' @param coef Coefficient.
#' @param method_impute Imputation method.
#' @param scale_method Scaling method for the \code{"scale"} step:
#'   \code{"zscore"}, \code{"minmax"}, or \code{"robust"}.
#'   Other values fall back to identity scaling.
#' @param verbose Logical.
#' @return A \code{prep_plan} list.
#' @export
prep_fit <- function(data, steps = c("varidele", "obsedele", "outlier", "impute", "scale"),
                     cols = NULL, group = NULL, date_col = NULL,
                     fraction = 0.25, top = 0.995, bottom = 0.0025,
                     by = "min", half = 30, method_outlier = "iqr",
                     coef = 1.5, method_impute = "linear",
                     scale_method = "zscore", verbose = FALSE) {
  t0 <- Sys.time()
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (is.null(cols)) cols <- which(sapply(data, is.numeric))
  else cols <- resolve_cols(data, cols)
  check_numeric_cols(data, cols)

  plan <- list(
    steps = steps,
    params = list(),
    data_info = list(
      original_names = names(data),
      col_idx = cols,
      date_col = date_col,
      group = group
    )
  )

  for (step in steps) {
    switch(step,
           "varidele" = {
             mat <- as.matrix(data[, cols, drop = FALSE])
             frac <- colMeans(is.na(mat))
             keep_flag <- frac < fraction
             plan$params$varidele_keep <- keep_flag
             keep_names <- names(data)[cols][keep_flag]
             if (length(keep_names) == 0)
               stop("All selected variables were removed by varidele")
             plan$params$varidele_keep_names <- keep_names
             drop_names <- names(data)[cols][!keep_flag]
             if (length(drop_names) > 0) {
               data <- data[, setdiff(names(data), drop_names), drop = FALSE]
             }
             cols <- match(keep_names, names(data))
             plan$params$varidele_cols_after <- names(data)[cols]
           },
           "obsedele" = {
             plan$params$obsedele_by <- by
             plan$params$obsedele_half <- half
             data <- obsedele(data, cols = cols, group = group,
                              by = by, half = half, date_col = date_col,
                              cores = NULL, verbose = verbose)
           },
           "outlier" = {
             plan$params$outlier_method <- method_outlier
             plan$params$outlier_group <- group
             if (is.null(group)) {
               thresholds <- list()
               for (j in seq_along(cols)) {
                 x <- data[[cols[j]]]
                 if (method_outlier == "percentile") {
                   q_top <- quantile(x, top, na.rm = TRUE)
                   q_bottom <- quantile(x, bottom, na.rm = TRUE)
                   thresholds[[names(data)[cols[j]]]] <- list(top = q_top, bottom = q_bottom)
                 } else if (method_outlier == "iqr") {
                   q1 <- quantile(x, 0.25, na.rm = TRUE)
                   q3 <- quantile(x, 0.75, na.rm = TRUE)
                   iqr <- q3 - q1
                   thresholds[[names(data)[cols[j]]]] <- list(lower = q1 - coef * iqr, upper = q3 + coef * iqr)
                 } else if (method_outlier == "mad") {
                   med <- median(x, na.rm = TRUE)
                   mad_val <- mad(x, na.rm = TRUE)
                   thresholds[[names(data)[cols[j]]]] <- list(median = med, mad = mad_val, coef = coef)
                 }
               }
               plan$params$outlier_thresholds <- thresholds
             } else {
               group_col <- if (is.character(group)) group else names(data)[group]
               ug <- unique(data[[group_col]])
               thresholds <- list()
               for (g in ug) {
                 rows <- which(data[[group_col]] == g)
                 for (j in cols) {
                   x <- data[rows, j]
                   key <- paste0(g, "_", names(data)[j])
                   if (method_outlier == "percentile") {
                     q_top <- quantile(x, top, na.rm = TRUE)
                     q_bottom <- quantile(x, bottom, na.rm = TRUE)
                     thresholds[[key]] <- list(top = q_top, bottom = q_bottom)
                   } else if (method_outlier == "iqr") {
                     q1 <- quantile(x, 0.25, na.rm = TRUE)
                     q3 <- quantile(x, 0.75, na.rm = TRUE)
                     iqr <- q3 - q1
                     thresholds[[key]] <- list(lower = q1 - coef * iqr, upper = q3 + coef * iqr)
                   } else if (method_outlier == "mad") {
                     med <- median(x, na.rm = TRUE)
                     mad_val <- mad(x, na.rm = TRUE)
                     thresholds[[key]] <- list(median = med, mad = mad_val, coef = coef)
                   }
                 }
               }
               plan$params$outlier_thresholds <- thresholds
             }
             data <- detect_outliers(data, cols = cols, method = method_outlier,
                                     top = top, bottom = bottom, coef = coef,
                                     group = group, mask_only = FALSE,
                                     verbose = verbose)
           },
           "impute" = {
             plan$params$impute_method <- method_impute
             plan$params$impute_group <- group
             data <- impute_missing(data, cols = cols, method = method_impute,
                                    group = group, verbose = verbose)
           },
           "scale" = {
             plan$params$scale_method <- scale_method
             plan$params$scale_center <- list()
             plan$params$scale_scale <- list()
             for (j in cols) {
               x <- data[[j]]
               vals <- x[!is.na(x)]
               if (length(vals) == 0) {
                 plan$params$scale_center[[names(data)[j]]] <- NA_real_
                 plan$params$scale_scale[[names(data)[j]]] <- 1
                 next
               }
               if (scale_method == "zscore") {
                 center <- mean(vals)
                 scale_val <- sd(vals)
               } else if (scale_method == "minmax") {
                 center <- min(vals)
                 scale_val <- max(vals) - min(vals)
               } else if (scale_method == "robust") {
                 center <- median(vals)
                 scale_val <- IQR(vals)
               } else {
                 center <- 0
                 scale_val <- 1
               }
               # Guard: a constant training column has sd = 0, IQR = 0,
               # or max - min = 0. Storing 0 as `scale_val` would make
               # prep_transform() divide by zero. Store 1 instead, so
               # the transform becomes `x - center`.
               if (is.na(scale_val) || scale_val == 0) scale_val <- 1
               plan$params$scale_center[[names(data)[j]]] <- center
               plan$params$scale_scale[[names(data)[j]]] <- scale_val
             }
             plan$params$scale_group <- group
             data <- transform_data(data, cols = cols, method = scale_method,
                                    group = group, verbose = verbose)
           },
           stop(paste("Unknown step:", step))
    )
  }

  plan$final_data <- data
  if (verbose) cat("Preprocessing plan fitted.\n")
  plan
}
