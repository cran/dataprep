\name{shorvalu}
\alias{shorvalu}
\title{
Interpolation with values to refer to within short periods
}
\description{
Performs linear interpolation of missing values within short time segments, constrained by the availability of observed values in the same segment. This method ensures that interpolation is based on nearby valid data.
}
\usage{
shorvalu(data, cols = NULL, intervals = 30, units = "mins",
         date_col = NULL, cores = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector. If a vector, simple linear interpolation is applied.}
  \item{cols}{Column indices or names of numeric variables to interpolate. Must be continuous.}
  \item{intervals}{Time gap used to define short periods.}
  \item{units}{Time unit for \code{intervals}: "secs", "mins", "hours", "days", "weeks".}
  \item{date_col}{Time column index or name. If \code{NULL}, automatically detected.}
  \item{cores}{Number of CPU cores for parallel computation (passed to C++).}
  \item{verbose}{Logical; if \code{TRUE}, prints timing and interpolation counts.}
}
\value{
A data frame with missing values filled by linear interpolation within each period.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
shorvalu(condextr(obsedele(data[1:250, c(1, 4, 17:19)], cols = 3:5, group = 2),
                  cols = 3:5, group = 2),
         cols = 3:5)
}