\name{percplot}
\alias{percplot}
\title{
Plot the top and bottom percentiles of each selected variable
}
\description{
The top and bottom percentiles of selected variables calculated by \code{percdata} can be plotted by \code{percplot} that offers a vivid check of possible outliers. It uses \code{reshape2::melt} or \code{dataprep::melt} to melt the data and uses \code{ggplot2} and \code{scales} to plot the data.
}
\usage{
percplot(data, start = NULL, end = NULL, group = NULL, ncol = NULL,
diff = 0.1, part = 'both')
}
\arguments{
  \item{data}{
  A data frame to calculate percentiles, from the column \code{start} to the column \code{end}.
}
  \item{start}{
  The column number of the first variable to calculate percentiles for.
}
  \item{end}{
  The column number of the last variable to calculate percentiles for.
}
  \item{group}{
  The column number of the grouping variable. It can be selected according to whether the data needs to be processed in groups. If grouping is not required, leave it default (NULL); if grouping is required, set \code{group} as the column number (position) where the grouping variable is located. If there are more than one grouping variable, it can be turned into a longer group through combination and transformation in advance.
}
  \item{ncol}{
  The total columns of the plot.
}
  \item{diff}{
  The common difference between \code{quantile}'s \code{probs}. Default is 0.1.
}
  \item{part}{
  The option of plotting bottom and/or top percentiles (parts). Default is 'both', or 2 for both bottom and top parts. Setting it as 'bottom' or 0 for bottom part and 'top' or 1 for top part.
}
}
\details{
Four scenes are considered according to the scales of x and y axes, namely the ranges of x and y values. For example, the code, \code{sd(diff(log(as.numeric(as.character(names(data[, start:end]))))))/mean(diff(log(as.numeric(as.character(names(data[, start:end])))))) < 0.1 & max(data[, start:end], na.rm = T)/min(data[, start:end], na.rm = T) > = 10^3}, means that the coefficient of variation of the lagged differences of \code{log(x)} is below 0.1 and meanwhile the maximum y is 1000 times greater than or equal to the minimum y.
}
\value{
  Top (highest or greatest) and bottom (lowest or smallest) percentiles are plotted.
  \item{0th }{Quantile with \code{probs = 0}}
  \item{0.1th }{Quantile with \code{probs = 0.001}}
  \item{0.2th }{Quantile with \code{probs = 0.002}}
  \item{0.3th }{Quantile with \code{probs = 0.003}}
  \item{0.4th }{Quantile with \code{probs = 0.004}}
  \item{0.5th }{Quantile with \code{probs = 0.005}}
  \item{99.5th }{Quantile with \code{probs = 0.995}}
  \item{99.6th }{Quantile with \code{probs = 0.996}}
  \item{99.7th }{Quantile with \code{probs = 0.997}}
  \item{99.8th }{Quantile with \code{probs = 0.998}}
  \item{99.9th }{Quantile with \code{probs = 0.999}}
  \item{100th }{Quantile with \code{probs = 1}}
}
\references{
1. Example data is from https://smear.avaa.csc.fi/download. It includes particle number concentrations in SMEAR I Varrio forest.

2. Wickham, H. 2007. Reshaping data with the reshape package. Journal of Statistical Software, 21(12):1-20.

3. Wickham, H. 2009. ggplot2: Elegant Graphics for Data Analysis. http://ggplot2.org: Springer-Verlag New York.

4. Wickham, H. 2016. ggplot2: elegant graphics for data analysis. Springer-Verlag New York.

5. Wickham, H. 2017. scales: Scale Functions for Visualization. 0.5.0 ed. https://github.com/hadley/scales.

6. Wickham, H. & Seidel, D. 2019. scales: Scale Functions for Visualization. R package version 1.1.0. https://CRAN.R-project.org/package=scales.
}
\author{
Chun-Sheng Liang <liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::percdata} and \code{dataprep::melt}
}
\examples{
# Plot
percplot(data,5,65,4)

# Plot
percplot(data1,3,7,2)
}


