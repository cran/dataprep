\name{day_night_flag}
\alias{day_night_flag}
\title{
Day/night flag
}
\description{
Creates a day/night indicator for each time point. If latitude and longitude are not provided, local time is used (daylight defined by an hour threshold). Otherwise, an approximate solar zenith angle is computed to determine whether the sun is above the horizon.
}
\usage{
day_night_flag(data, date_col = NULL, lat = NULL, lon = NULL,
               threshold = 6, type = c("binary", "sun", "shade"),
               local_tz = "UTC", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame with a time column.}
  \item{date_col}{Time column index or name. If \code{NULL}, the first column matching \code{date} is used.}
  \item{lat}{Latitude of the location(s). Can be a scalar or a vector of length equal to the number of rows.}
  \item{lon}{Longitude of the location(s). Same behavior as \code{lat}.}
  \item{threshold}{Hour threshold used for local-time based classification (default 6). Only used if latitude/longitude are missing.}
  \item{type}{Output type: \code{"binary"} returns 1/0, \code{"sun"} returns "day"/"night", \code{"shade"} returns "day"/"shade".}
  \item{local_tz}{Time zone for local time calculation (default UTC).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
When latitude/longitude are supplied, the function computes the solar zenith angle using a simplified astronomical formula. Values with a solar zenith angle below 90 degrees are considered day.
}
\value{
A vector of day/night flags.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
day_night_flag(data[1:100, c(1, 4, 17:19)], date_col = 1, lat = 40, lon = -105)
}
