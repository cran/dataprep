\name{optisolu}
\alias{optisolu}
\title{
Find optimal combination of interval and times for condextr
}
\description{
Searches over a grid of \code{interval} and \code{times} values to find
the combination that yields better outlier removal than the traditional
percentile method, based on sample deletion ratio (SDR), outlier removal
ratio (ORR), and signal-to-noise ratio (SNR).
}
\usage{
optisolu(data, cols = NULL, group = NULL, interval = 35, times = 10,
top = 0.995, top.error = 0.1, top.magnitude = 0.2,
bottom = 0.0025, bottom.error = 0.2, bottom.magnitude = 0.4,
by = "min", half = 30, date_col = NULL, cores = NULL,
verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing numeric columns and optionally a
    grouping column.}
  \item{cols}{Column indices or names of numeric variables.}
  \item{group}{Grouping column index or name.}
  \item{interval}{Maximum interval value to test (from 1 to \code{interval}).}
  \item{times}{Maximum times value to test (from 1 to \code{times}).}
  \item{top}{Top percentile threshold.}
  \item{top.error}{Error margin for the top threshold.}
  \item{top.magnitude}{Magnitude margin for the top threshold.}
  \item{bottom}{Bottom percentile threshold.}
  \item{bottom.error}{Error margin for the bottom threshold.}
  \item{bottom.magnitude}{Magnitude margin for the bottom threshold.}
  \item{by}{Time unit used when computing observation deletion.}
  \item{half}{Half window size in minutes for observation deletion.}
  \item{date_col}{Time column index or name.}
  \item{cores}{Number of CPU cores.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
This function can be computationally intensive. It compares
\code{\link{condextr}} results with \code{\link{percoutl}} results.
}
\value{
A data frame with columns: \code{case}, \code{interval}, \code{times},
\code{sdr}, \code{orr}, \code{snr}, \code{index}, \code{relaindex}, and
\code{optimal} (logical).
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes
   particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
\donttest{
optisolu(data[1:50, c(1, 4, 18:19)], cols = 3:4, group = 2,
         interval = 2, times = 1)
}
}