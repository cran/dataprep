\name{melt}
\alias{melt}
\title{
Turn variable names and values into two columns
}
\description{
Turn the names and values of all pending variables into two columns. These variables are inversely selected inside function (\code{cols}) and waiting to be melted. After melting, the data format changes from wide to long.
}
\usage{
melt(data, cols = NULL)
}
\arguments{
  \item{data}{
  A data frame to melt, from the column \code{start} to the column \code{end}.
}
  \item{cols}{
  Inversely selected columns inside function, except which are columns waiting to be melted.
}
}
\details{
  This function (\code{dataprep::melt}) will be used when \code{reshape2} is not installed.
}
\value{
A long-format data frame from its original wide format.
}
\references{
1. Example data is from https://smear.avaa.csc.fi/download. It includes particle number concentrations in SMEAR I Varrio forest.

2. Wickham, H. 2007. Reshaping data with the reshape package. Journal of Statistical Software, 21(12):1-20.
}
\author{
Chun-Sheng Liang <liangchunsheng@lzu.edu.cn>
}
\examples{
# The first pending variable contains only NA
melt(data,1:4)

# Number concentrations of modes and total particles are not NA
melt(data1,1:2)
}

