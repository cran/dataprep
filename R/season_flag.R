#' Season, month, or quarter flag
#' @param data A data frame with a time column.
#' @param date_col Time column.
#' @param type \code{"season"}, \code{"month"}, or \code{"quarter"}.
#' @param verbose Logical.
#' @return A vector of flags.
#' @export
season_flag <- function(data, date_col = NULL, type = c("season", "month", "quarter"),
                        verbose = FALSE) {
  t0 <- Sys.time()
  type <- match.arg(type)
  if (is.vector(data) && !is.list(data)) {
    stop("season_flag requires a data frame with a time column")
  }

  if (is.null(date_col)) {
    date_info <- resolve_date_col(data, date_col)
    date_name <- date_info$name
  } else {
    date_name <- if (is.character(date_col)) date_col else names(data)[date_col]
  }

  time_vec <- data[[date_name]]
  if (!inherits(time_vec, c("POSIXct", "Date"))) {
    stop("Time column must be POSIXct or Date")
  }

  lt <- as.POSIXlt(time_vec)
  mon <- lt$mon + 1

  if (type == "season") {
    flag <- ifelse(mon %in% c(12, 1, 2), "winter",
                   ifelse(mon %in% c(3, 4, 5), "spring",
                          ifelse(mon %in% c(6, 7, 8), "summer", "autumn")))
  } else if (type == "month") {
    flag <- factor(month.abb[mon], levels = month.abb)
  } else {
    qtr <- ceiling(mon / 3)
    flag <- factor(paste0("Q", qtr))
  }

  if (verbose) cat("Season flag created.\n")
  return(flag)
}
