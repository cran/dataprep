\name{condextr}
\alias{condextr}
\title{
Remove outliers using point-by-point weighed outlier removal by conditional extremum
}
\description{
An outlier removal method that considers each potential outlier individually, using conditional thresholds based on top and bottom percentiles and error margins. It is a more refined alternative to simple percentile-based removal.
}
\usage{
condextr(data, cols = NULL, group = NULL, top = 0.995,
top.error = 0.1, top.magnitude = 0.2, bottom = 0.0025, bottom.error = 0.2,
bottom.magnitude = 0.4, interval = 10, by = "min", half = 30,
times = 10, date_col = NULL, cores = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector. If a vector, outlier removal is applied directly.}
  \item{cols}{Column indices or names of numeric variables.}
  \item{group}{Grouping column index or name. Outlier removal is performed within each group.}
  \item{top, top.error, top.magnitude}{Parameters for the top threshold.}
  \item{bottom, bottom.error, bottom.magnitude}{Parameters for the bottom threshold.}
  \item{interval}{Number of outlier marking iterations between observation deletions.}
  \item{by}{Time unit for observation deletion. An invalid unit string raises an error.}
  \item{half}{Half window size in minutes for observation deletion.}
  \item{times}{Number of observation deletions.}
  \item{date_col}{Time column index or name.}
  \item{cores}{Number of CPU cores passed to the C++ backend. \code{NULL} (default) lets the backend choose based on data size.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing and deletion counts.}
}
\details{
The method marks values as outliers if they are the maximum or minimum and exceed a threshold that depends on a high percentile plus an error margin. After a specified number of marking steps, observation deletion is applied to remove rows with excessive missing values created by outlier removal.
}
\value{
A data frame with outliers removed.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
condextr(obsedele(data[1:500, c(1, 4, 17:19)], cols = 3:5, group = 2),
         cols = 3:5, group = 2)
}
