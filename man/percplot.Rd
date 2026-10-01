\name{percplot}
\alias{percplot}
\title{Plot top and bottom percentiles of selected variables}
\description{
Plots percentiles computed by \code{percdata} using \code{ggplot2}.
}
\usage{
percplot(data, cols = NULL, group = NULL, diff = 0.1,
         part = "both", ncol = NULL, num_xaxis = "auto",
         verbose = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{cols}{Column indices or names of numeric variables.}
  \item{group}{Grouping column.}
  \item{diff}{Difference between quantile probabilities.}
  \item{part}{Which part to plot: \code{"both"}, \code{"bottom"}, or \code{"top"}. For backward compatibility, \code{part} also accepts the integer codes \code{2} (\code{"both"}), \code{0} (\code{"bottom"}), and \code{1} (\code{"top"}).}
  \item{ncol}{Number of columns in facet layout.}
  \item{num_xaxis}{How to treat numeric column names on the x-axis.
    \code{"auto"}: use \code{"log"} if column names are numeric and
    evenly spaced on a log scale with a range ratio >= 1000, otherwise
    keep them as factor levels. \code{"log"} or \code{"numeric"}: force
    the corresponding scale. \code{"character"}, \code{"factor"},
    \code{"keep"}, or \code{FALSE}: keep as character/factor.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing message.}
}
\value{
A \code{ggplot} object.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes
   particle number concentrations in SMEAR I Varrio forest.
2. Wickham, H. 2016. ggplot2: elegant graphics for data analysis.
   Springer-Verlag New York.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
percplot(data, cols = 5:65, group = 4)
percplot(data, cols = 5:65, group = 4, num_xaxis = "numeric")
}