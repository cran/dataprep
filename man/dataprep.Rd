\name{dataprep}
\alias{dataprep}
\title{
Data preprocessing with multiple steps in one function
}
\description{
Performs variable deletion (\code{varidele}), observation deletion (\code{obsedele}), conditional extremum outlier removal (\code{condextr}), and short-period interpolation (\code{shorvalu}) in sequence.
}
\usage{
dataprep(data, cols = NULL, group = NULL, optimal = FALSE,
interval = 10, times = 10, fraction = 0.25,
top = 0.995, top.error = 0.1, top.magnitude = 0.2,
bottom = 0.0025, bottom.error = 0.2, bottom.magnitude = 0.4, by = "min",
half = 30, intervals = 30, date_col = NULL, cores = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing numeric columns and optionally a grouping column.}
  \item{cols}{Column indices or names of numeric variables to process.}
  \item{group}{Grouping column index or name.}
  \item{optimal}{Logical; if \code{TRUE}, \code{optisolu} is used to find optimal \code{interval} and \code{times}.}
  \item{interval}{Number of outlier-marking rounds between two observation-deletion rounds inside \code{condextr}.}
  \item{times}{Number of observation-deletion rounds in \code{condextr}.}
  \item{fraction}{Missing proportion threshold for variable deletion.}
  \item{top, top.error, top.magnitude, bottom, bottom.error, bottom.magnitude}{Outlier removal parameters.}
  \item{by}{Time unit for observation deletion. An invalid unit string raises an error.}
  \item{half}{Half window size in minutes for observation deletion.}
  \item{intervals}{Time gap for interpolation periods.}
  \item{date_col}{Time column index or name. If \code{NULL}, automatically detected.}
  \item{cores}{Number of CPU cores passed to \code{obsedele()},
    \code{condextr()}, and \code{optisolu()}. \code{NULL} (default)
    lets each backend choose based on data size.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing and deletion/interpolation counts.}
}
\value{
A preprocessed data frame.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
dataprep(data[1:60, c(1, 4, 18:19)], cols = 3:4, group = 2, interval = 2, times = 1, cores = 1)
}
