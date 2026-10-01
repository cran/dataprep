#' Transform and standardize numeric variables
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to transform. If \code{NULL}, all numeric columns are used.
#' @param method Transformation or scaling method.
#' @param group Optional grouping column.
#' @param lambda Box-Cox / Yeo-Johnson parameter.
#' @param verbose Logical.
#' @return A data frame with transformed variables.
#' @export
transform_data <- function(data, cols = NULL, method = "log", group = NULL,
                           lambda = 1.0, verbose = FALSE) {
  t0 <- Sys.time()
  elem_methods  <- c("log", "log1p", "sqrt", "inverse", "boxcox", "yeojohnson")
  scale_methods <- c("zscore", "center", "scale", "minmax", "robust")
  if (!method %in% c(elem_methods, scale_methods)) {
    stop("Unsupported method: ",
         paste(c(elem_methods, scale_methods), collapse = ", "))
  }

  if (is.vector(data) && !is.list(data)) {
    if (method %in% elem_methods) return(transform_elem_cpp(data, method, lambda))
    else                          return(transform_scale_cpp(data, method))
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  if (is.null(group)) {
    for (j in idx) {
      if (method %in% elem_methods) {
        data[[j]] <- transform_elem_cpp(data[[j]], method, lambda)
      } else {
        data[[j]] <- transform_scale_cpp(data[[j]], method)
      }
    }
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (j in idx) {
        if (method %in% elem_methods) {
          data[rows, j] <- transform_elem_cpp(data[rows, j], method, lambda)
        } else {
          data[rows, j] <- transform_scale_cpp(data[rows, j], method)
        }
      }
    }
  }

  if (verbose) cat("Transformation completed.\n")
  data
}