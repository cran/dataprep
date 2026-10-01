\name{percdata}
\alias{percdata}
\title{
Calculate top and bottom percentiles of selected variables
}
\description{
Computes percentiles (quantiles) for selected numeric columns. By default, six bottom percentiles (0th to 0.5th) and six top percentiles (99.5th to 100th) are calculated. The function supports grouping.
}
\usage{
percdata(data, cols = NULL, group = NULL, diff = 0.1,
        part = "both", na.rm = TRUE, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector. If a vector, a data frame is created internally.}
  \item{cols}{Column indices or names of numeric variables. If \code{NULL}, all numeric columns are used.}
  \item{group}{Optional grouping column index or name. If provided, percentiles are computed separately for each group.}
  \item{diff}{Common difference between quantile probabilities. Default is 0.1.}
  \item{part}{Which parts to return: \code{"both"} (default), \code{"bottom"}, or \code{"top"}. For backward compatibility, \code{part} also accepts the integer codes \code{2} (\code{"both"}), \code{0} (\code{"bottom"}), and \code{1} (\code{"top"}).}
  \item{na.rm}{Logical; if \code{TRUE}, NA values are removed before quantile computation.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing message.}
}
\details{
The function uses \code{stats::quantile()} on each selected column. For grouped data, percentiles are computed within each group.
}
\value{
A data frame with rows corresponding to percentiles and columns corresponding to variables (plus a \code{percentile} column). If grouped, an additional grouping column is present.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
percdata(data, cols = 27:61, group = 4)
}