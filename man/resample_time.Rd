\name{resample_time}
\alias{resample_time}
\title{
Resample time series to a coarser period
}
\description{
Aggregates time series data to a specified period (hour, day, or month) using a chosen summary statistic. The function requires a time column.
}
\usage{
resample_time(data, cols = NULL, date_col = NULL,
              period = "hour", fun = "mean", na.rm = TRUE,
              verbose = FALSE)
}
\arguments{
  \item{data}{A data frame with a time column.}
  \item{cols}{Column indices or names to aggregate.}
  \item{date_col}{Time column index or name.}
  \item{period}{Target period: \code{"hour"}, \code{"day"}, or \code{"month"}.}
  \item{fun}{Aggregation function: \code{"mean"}, \code{"sum"}, \code{"min"}, \code{"max"}, \code{"median"}, or \code{"sd"}.}
  \item{na.rm}{Logical; whether to remove NA values before aggregation.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with aggregated time series.
}
\examples{
resample_time(data[1:500, c(1, 4, 17:19)], cols = 3:5, date_col = 1, period = "hour", fun = "mean")
}