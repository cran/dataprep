\name{descplot}
\alias{descplot}
\title{
View the descriptive statistics via plot
}
\description{
It applies to an original (a raw) data and produces a plot to describe the data with 9 statistics including n, na, mean, sd, median, trimmed, min, max, and IQR.
}
\usage{
descplot(data, start = NULL, end = NULL, stats= 1:9, first = "variables")
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
This function will describe the data first using descdata. Then, A plot to show the result will be produced using the package \code{ggplot2} (coupled with self-defined \code{melt} or \code{reshape2::melt} to melt the intermediate data). The variables from \code{start} to \code{end} need to be linked together instead of being scattered.
}
\value{
A plot to show the descriptive result of the data, including:
\item{size}{default general name of items (variables). Users can define it via the parameter \code{first}.}
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

2. Wickham, H. 2007. Reshaping data with the reshape package. Journal of Statistical Software, 21(12):1-20.

3. Wickham, H. 2009. ggplot2: Elegant Graphics for Data Analysis. http://ggplot2.org: Springer-Verlag New York.

4. Wickham, H. 2016. ggplot2: elegant graphics for data analysis. Springer-Verlag New York.
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::descdata} and \code{dataprep::melt}
}
\examples{
# Line plots for variable names that are essentially numeric
descplot(data,5,65)
# Use numbers to select statistics
descplot(data,5,65,c(2,7:9))
# Use characters to select statistics
descplot(data,5,65,c('na','min','max','IQR'))

# Bar charts for type of variable names that is character
descplot(data1,3,7)
# Use numbers to select statistics
descplot(data1,3,7,7:9)
# Use characters to select statistics
descplot(data1,3,7,c('min','max','IQR'))
}

