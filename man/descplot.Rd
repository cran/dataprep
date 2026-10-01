\name{descplot}
\alias{descplot}
\title{View descriptive statistics via plot}
\description{
Applies \code{descdata} to the selected columns and produces a plot showing the descriptive statistics. The plot is generated using \code{ggplot2} and a suitable melt function.
}
\usage{
descplot(data, cols = NULL, stats = 1:9, first = "variables",
         ncol = NULL, num_xaxis = "log", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing numeric columns to describe.}
  \item{cols}{Column indices or names of numeric variables. If \code{NULL}, all numeric columns are used.}
  \item{stats}{Statistics to plot, as in \code{descdata}.}
  \item{first}{Name for the variable identifier column (passed to \code{descdata}).}
  \item{ncol}{Number of columns in the facet layout (passed to \code{facet_wrap}).}
  \item{num_xaxis}{How to treat numeric column names on the x-axis. Options:
    \code{"log"} (default): if all selected column names are numeric, convert to numeric and use log10 scale;
    \code{"numeric"}: convert to numeric but use linear scale;
    \code{"character"}, \code{"factor"}, \code{"keep"}, or \code{FALSE}: keep as character/factor (no conversion).}
  \item{verbose}{Logical; if \code{TRUE}, prints timing message.}
}
\details{
This function first calls \code{descdata} and then melts the result into a long format suitable for \code{ggplot2}. If the column names of the original data are numeric, the plot uses a logarithmic x-axis and line geometry; otherwise, a bar plot is produced. The \code{num_xaxis} parameter allows overriding the default log transform.
}
\value{
A \code{ggplot} object displaying the descriptive statistics.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}. It includes particle number concentrations in SMEAR I Varrio forest.
2. Wickham, H. 2016. ggplot2: elegant graphics for data analysis. Springer-Verlag New York.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
descplot(data, cols = 5:65)
descplot(data, cols = 5:65, stats = c("min","max","IQR"), num_xaxis = "numeric")
}