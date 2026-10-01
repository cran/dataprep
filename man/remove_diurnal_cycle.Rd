\name{remove_diurnal_cycle}
\alias{remove_diurnal_cycle}
\title{
Remove diurnal cycle
}
\description{
Removes the mean diurnal (or other periodic) cycle by subtracting the average value for each time period (hour, month, or day). The grouping is based on a time column.
}
\usage{
remove_diurnal_cycle(data, cols = NULL, date_col = NULL,
                     by = "hour", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame with a time column.}
  \item{cols}{Column indices or names to adjust.}
  \item{date_col}{Time column index or name.}
  \item{by}{Period for cycle removal: \code{"hour"}, \code{"month"}, or \code{"day"}.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with diurnal cycle removed.
}
\examples{
remove_diurnal_cycle(data[1:200, c(1, 4, 17:19)], cols = 3:5, date_col = 1, by = "hour")
}