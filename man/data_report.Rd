\name{data_report}
\alias{data_report}
\title{
Generate a simple data quality report
}
\description{
Computes a data quality summary and returns it invisibly as a list. When \code{verbose = TRUE}, the summary is also printed to the console.
}
\usage{
data_report(data, cols = NULL, date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{cols}{Optional column indices or names for the numeric columns to include in the report. If NULL, all numeric columns are used.}
  \item{date_col}{Reserved for future use; currently ignored (passed through to \code{\link{na_diagnose}}).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress and timing messages.}
}
\value{
Invisibly returns a list containing data dimensions, variable types, missing value diagnosis, and descriptive statistics.
}
\examples{
data_report(data[1:200, c(1, 4, 17:19)], cols = 3:5, date_col = 1)
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
