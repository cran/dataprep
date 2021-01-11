\name{descdata}
\alias{descdata}
\title{
Fast descriptive statistics
}
\description{
It describes data using R basic functions, without calling other packages to avoid redundant calculations, which is faster.
}
\usage{
descdata(data, start = NULL, end = NULL, stats= 1:9, first = "variables")
}
\arguments{
  \item{data}{
  A data frame to describe, from the column \code{start} to the column \code{end}.
}
  \item{start}{
  The column number of the first variable to describe.
}
  \item{end}{
  The column number of the last variable to describe.
}
  \item{stats}{
  Selecting or rearranging the items from the 9 statistics, i.e., n, na, mean, sd, median, trimmed, min, max, and IQR. It can be a vector or a single value, in 'character' or 'numeric' class.
}
  \item{first}{
  The name of the first column of the output. It is the general name of the items (variables).
}
}
\details{
This function can be used for different types of data, as long as the variables are numeric. Because it describes the data frame from the column \code{start} to the column \code{end}, the variables need to be linked together instead of being scattered.
}
\value{
A data frame of descriptive statistics:
\item{size}{default general name of items (variables). Users can define it via the parameter first.}
\item{n}{number of valid cases}
\item{na}{number of invalid cases}
\item{mean}{mean of each item}
\item{sd}{standard deviation}
\item{median}{median of each item}
\item{trimmed}{trimmed mean (with trim defaulting to .1)}
\item{min}{minimum of each item}
\item{max}{maximum of each item}
\item{IQR}{interquartile range of each item}
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::descplot}
}
\examples{
# Variable names are essentially numeric
descdata(data,5,65)
# Use numbers to select statistics
descdata(data,5,65,c(2,7:9))
# Use characters to select statistics
descdata(data,5,65,c('na','min','max','IQR'))

# When type of variable names is character
descdata(data1,3,7)
# Use numbers to select statistics
descdata(data1,3,7,c(2,7:9))
# Use characters to select statistics
descdata(data1,3,7,c('na','min','max','IQR'))
}

