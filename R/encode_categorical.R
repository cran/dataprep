#' Encode categorical variables
#' @param data A data frame.
#' @param cols Columns to encode.
#' @param method "label", "frequency", or "onehot".
#' @param group Optional grouping column.
#' @param prefix Prefix for one-hot column names.
#' @param verbose Logical.
#' @return A data frame with encoded variables.
#' @export
encode_categorical <- function(data, cols = NULL, method = "label",
                               group = NULL, prefix = "enc_",
                               verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("label", "onehot", "frequency"))

  if (is.vector(data) && !is.list(data)) {
    if (method == "label") {
      return(as.integer(factor(data)))
    } else if (method == "frequency") {
      freq <- table(data)
      return(as.numeric(freq[as.character(data)]))
    } else if (method == "onehot") {
      cats <- unique(data)
      mat <- matrix(0, nrow = length(data), ncol = length(cats))
      colnames(mat) <- paste0(prefix, cats)
      for (i in seq_along(cats)) {
        mat[, i] <- as.integer(data == cats[i])
      }
      return(mat)
    }
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  # Default: select only factor and character columns
  if (is.null(cols)) {
    cols <- which(vapply(data, function(x) is.factor(x) || is.character(x), logical(1)))
  }

  idx <- resolve_cols(data, cols)
  for (j in idx) {
    if (!is.factor(data[[j]]) && !is.character(data[[j]])) {
      stop("Column ", names(data)[j], " is not categorical")
    }
  }

  if (method == "label") {
    for (j in idx) {
      data[[j]] <- as.integer(factor(data[[j]]))
    }
  } else if (method == "frequency") {
    for (j in idx) {
      if (is.null(group)) {
        freq <- table(data[[j]])
        data[[j]] <- as.numeric(freq[as.character(data[[j]])])
      } else {
        group_col <- if (is.character(group)) group
                     else names(data)[group]
        ug <- unique(data[[group_col]])
        for (g in ug) {
          rows <- which(data[[group_col]] == g)
          freq_g <- table(data[rows, j])
          data[rows, j] <- as.numeric(freq_g[as.character(data[rows, j])])
        }
      }
    }
  } else if (method == "onehot") {
    new_cols <- list()
    for (j in idx) {
      cats <- unique(data[[j]])
      for (cat in cats) {
        new_name <- paste0(prefix, names(data)[j], "_", cat)
        new_cols[[new_name]] <- as.integer(data[[j]] == cat)
      }
    }
    # Guard against idx covering all columns; data[, -idx] would
    # produce an empty data frame otherwise.
    if (length(idx) > 0) {
      data <- data[, -idx, drop = FALSE]
    }
    new_df <- as.data.frame(new_cols)
    data <- cbind(data, new_df)
  }

  if (verbose) cat("Categorical encoding completed.\n")
  data
}