#' Balance panel data
#' @param data A data frame containing panel data.
#' @param unit_col Column index or name identifying the individual unit.
#' @param time_col Column index or name identifying the time point.
#' @param fill Value used to fill missing combinations.
#' @param method Either \code{"fill"} or \code{"complete"}.
#'   With \code{"fill"}, numeric non-key columns of the added
#'   rows are filled with \code{fill}; non-numeric columns are
#'   left as \code{NA}.
#' @param verbose Logical; if \code{TRUE}, prints progress message.
#' @return A balanced data frame.
#' @export
balance_panel <- function(data, unit_col = NULL, time_col = NULL,
                          fill = NA_real_, method = c("fill", "complete"),
                          verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("balance_panel requires a data frame")
  if (is.null(unit_col) || is.null(time_col)) {
    if (is.null(unit_col)) unit_col <- names(data)[1]
    if (is.null(time_col)) {
      date_info <- resolve_date_col(data, NULL)
      time_col <- date_info$name
    }
  }
  unit_name <- if (is.character(unit_col)) unit_col else names(data)[unit_col]
  time_name <- if (is.character(time_col)) time_col else names(data)[time_col]

  units <- unique(data[[unit_name]])
  times <- sort(unique(data[[time_name]]))

  if (method == "complete") {
    complete_units <- sapply(units, function(u) {
      sub <- data[[time_name]][data[[unit_name]] == u]
      length(setdiff(times, sub)) == 0
    })
    keep_units <- units[complete_units]
    result <- data[data[[unit_name]] %in% keep_units, , drop = FALSE]
  } else {
    grid <- expand.grid(unit = units, time = times, stringsAsFactors = FALSE)
    names(grid) <- c(unit_name, time_name)
    result <- merge(grid, data, by = c(unit_name, time_name), all.x = TRUE)
    nonkey <- setdiff(names(result), c(unit_name, time_name))
    for (col in nonkey) {
      if (is.numeric(result[[col]])) {
        result[[col]][is.na(result[[col]])] <- fill
      }
    }
  }
  rownames(result) <- NULL
  if (verbose) cat("Panel balanced.\n")
  result
}
