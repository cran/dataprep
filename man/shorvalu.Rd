\name{shorvalu}
\alias{shorvalu}
\title{
Interpolation with values to refer to within short periods
}
\description{
Time gaps and available values are considered in NA interpolation by \code{shorvalu}. Thus, more reliable interpolation is realized with these constraints and the successive using of \code{obsedele} in the preceding outlier removal.
}
\usage{
shorvalu(data, start, end, intervals = 30)
}
\arguments{
  \item{data}{
  A data frame containing outliers. Its columns from \code{start} to \code{end} will be checked.
}
  \item{start}{
  The column number of the first selected variable.
}
  \item{end}{
  The column number of the last selected variable.
}
  \item{intervals}{
  The time gap of dividing periods as groups, is 30 (minutes) by default. This confines the interpolation inside short periods so that each interpolation has observed value(s) to refer to within every half an hour.
}
}
\details{
It offers a robust interpolation method based on considering time gaps and available values.
}
\value{
A data frame with missing values being replaced linearly within short periods and with values to refer to.
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.

4. Zeileis, A. & Grothendieck, G. 2005. zoo: S3 infrastructure for regular and irregular time series. Journal of Statistical Software, 14(6):1-27.

5. Zeileis, A., Grothendieck, G. & Ryan, J.A. 2019. zoo: S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations). R package version 1.8-6. https://cran.r-project.org/web/packages/zoo/.
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\examples{
shorvalu(condextr(obsedele(data[1:250,c(1,4,17:19)],3,5,2,cores=2),
3,5,2,cores=2),3,5)
}

