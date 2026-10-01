#' Remove diurnal (or other periodic) cycle
#' @param data A data frame or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param date_col Time column.
#' @param by "hour", "month", or "day".
#' @param verbose Logical.
#' @return A data frame or vector with cycle removed.
#' @export
remove_diurnal_cycle <- function(data, cols = NULL, date_col = NULL,
                                 by = "hour", verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    if (is.null(date_col)) {
      warning("Vector input without date_col assumes equally spaced ",
              "data at the requested 'by' interval. Provide date_col ",
              "for irregular series.")
      n <- length(data)
      if (by == "hour") {
        cycle_id <- ((seq_len(n) - 1) %% 24) + 1
      } else if (by == "month") {
        cycle_id <- ((seq_len(n) - 1) %% 12) + 1
      } else if (by == "day") {
        cycle_id <- ((seq_len(n) - 1) %% 365) + 1
      } else {
        stop("by must be 'hour', 'month', or 'day'")
      }
      means <- tapply(data, cycle_id, mean, na.rm = TRUE)
      x_demean <- data - means[as.character(cycle_id)]
      x_demean[is.na(x_demean) & !is.na(data)] <- NA
      if (verbose) cat("Diurnal cycle removed.\n")
      return(x_demean)
    } else {
      if (!inherits(date_col, c("POSIXct", "Date"))) {
        stop("date_col must be POSIXct or Date for vector input")
      }
      time_vec <- date_col
      if (by == "hour") {
        cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%H"))
      } else if (by == "month") {
        cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%m"))
      } else if (by == "day") {
        cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%j"))
      } else {
        stop("by must be 'hour', 'month', or 'day'")
      }
      means <- tapply(data, cycle_id, mean, na.rm = TRUE)
      x_demean <- data - means[as.character(cycle_id)]
      x_demean[is.na(x_demean) & !is.na(data)] <- NA
      if (verbose) cat("Diurnal cycle removed.\n")
      return(x_demean)
    }
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name
  time_vec  <- data[[date_name]]
  if (!inherits(time_vec, c("POSIXct", "Date"))) {
    stop("A valid time column is required for diurnal cycle removal")
  }

  if (by == "hour") {
    cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%H"))
  } else if (by == "month") {
    cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%m"))
  } else if (by == "day") {
    cycle_id <- as.integer(format(as.POSIXlt(time_vec), "%j"))
  } else {
    stop("by must be 'hour', 'month', or 'day'")
  }

  for (j in idx) {
    x <- data[[j]]
    means <- tapply(x, cycle_id, mean, na.rm = TRUE)
    x_demean <- x - means[as.character(cycle_id)]
    x_demean[is.na(x_demean) & !is.na(x)] <- NA
    data[[j]] <- x_demean
  }

  if (verbose) cat("Diurnal cycle removed.\n")
  data
}