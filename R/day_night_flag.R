#' Day/night flag
#' @param data A data frame with a time column.
#' @param date_col Time column.
#' @param lat,lon Latitude and longitude (scalar or vector).
#' @param threshold Hour threshold for local-time classification.
#' @param type One of \code{"binary"}, \code{"sun"}, \code{"shade"}.
#' @param local_tz Time zone.
#' @param verbose Logical.
#' @return A vector of flags.
#' @export
day_night_flag <- function(data, date_col = NULL, lat = NULL, lon = NULL,
                           threshold = 6, type = c("binary", "sun", "shade"),
                           local_tz = "UTC", verbose = FALSE) {
  t0 <- Sys.time()
  type <- match.arg(type)
  if (is.vector(data) && !is.list(data)) {
    stop("day_night_flag requires a data frame with a time column")
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
  if (is.null(lat) || is.null(lon)) {
    hours <- as.numeric(format(as.POSIXlt(time_vec, tz = local_tz), "%H")) +
             as.numeric(format(as.POSIXlt(time_vec, tz = local_tz), "%M")) / 60
    is_day <- hours >= threshold & hours < (24 - threshold)
  } else {
    lats <- if (length(lat) == nrow(data)) lat else rep(lat, nrow(data))
    lons <- if (length(lon) == nrow(data)) lon else rep(lon, nrow(data))
    lt <- as.POSIXlt(time_vec, tz = local_tz)
    doy <- lt$yday + 1
    hour <- lt$hour + lt$min / 60 + lt$sec / 3600
    decl <- -23.44 * cos(2 * pi / 365 * (doy + 10)) * pi / 180
    lat_rad <- lats * pi / 180
    ha <- (hour + lons / 15 - 12) * 15 * pi / 180
    cos_sza <- sin(lat_rad) * sin(decl) + cos(lat_rad) * cos(decl) * cos(ha)
    cos_sza <- pmax(pmin(cos_sza, 1), -1)
    sza <- acos(cos_sza) * 180 / pi
    is_day <- sza < 90
  }
  if (type == "binary") {
    flag <- as.integer(is_day)
  } else if (type == "sun") {
    flag <- ifelse(is_day, "day", "night")
  } else {
    flag <- ifelse(is_day, "day", "shade")
  }
  if (verbose) cat("Time used by day_night_flag:", format(Sys.time() - t0, digits = 3), "\n")
  return(flag)
}
