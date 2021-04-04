\name{dataprep}
\alias{dataprep}
\title{
Data preprocessing with multiple steps in one function
}
\description{
The four steps, i.e., variable deletion by \code{varidele}, observation deletion by \code{obsedele}, outlier removal by \code{condextr}, and missing value interpolation by \code{shorvalu} can be finished in \code{dataprep}.
}
\usage{
dataprep(data, start = NULL, end = NULL, group = NULL, optimal = FALSE,
interval = 10, times = 10, fraction = 0.25,
top = 0.995, top.error = 0.1, top.magnitude = 0.2,
bottom = 0.0025, bottom.error = 0.2, bottom.magnitude = 0.4, by = "min",
half = 30, intervals = 30, cores = NULL)
}
\arguments{
  \item{data}{
  A data frame containing outliers (and missing values). Its columns from \code{start} to \code{end} will be checked.
}
  \item{start}{
  The column number of the first selected variable.
}
  \item{end}{
  The column number of the last selected variable.
}
  \item{group}{
    The column number of the grouping variable. It can be selected according to whether the data needs to be processed in groups. If grouping is not required, leave it default (NULL); if grouping is required, set \code{group} as the column number (position) where the grouping variable is located. If there are more than one grouping variable, it can be turned into a longer group through combination and transformation in advance.
}
  \item{optimal}{
A Boolean to decide whether the \code{optisolu} should be used to find optimal \code{interval} and \code{times} for \code{condextr}.
}
  \item{interval}{
  The interval of observation deletion, i.e. the number of outlier deletions before each observation deletion, is 10 by default.
}
  \item{times}{
  The number of observation deletions in outlier removal is 10 by default.
}
  \item{fraction}{
  The proportion of missing values of variables. Default is 0.25.
}
  \item{top}{
  The top percentile is 0.995 by default.
}
  \item{top.error}{
  The top allowable error coefficient is 0.1 by default.
}
  \item{top.magnitude}{
  The order of magnitude coefficient of the top error is 0.2 by default.
}
  \item{bottom}{
  The bottom percentile is 0.0025 by default.
}
  \item{bottom.error}{
  The bottom allowable error coefficient is 0.2 by default.
}
  \item{bottom.magnitude}{
  The order of magnitude coefficient of the bottom error is 0.4 by default.
}
  \item{by}{
  The time extension unit by is a minute ("min") by default. The user can specify other time units. For example, "5 min" means that the time extension unit is 5 minutes.
}
  \item{half}{
  Half window size of hourly moving average. It is 30 (minutes) by default, which is determined by the time expansion unit minute ("min").
}
  \item{intervals}{
  The time gap of dividing periods as groups, is 30 (minutes) by default. This confines the interpolation inside short periods so that each interpolation has observed value(s) to refer to within every half an hour.
}
  \item{cores}{
  The number of CPU cores.
}
}
\details{
If optimal = T, relatively much more time will be needed to finish the data preprocessing, but it gets better final result.
}
\value{
A preprocessed data frame after variable deletion, observation deletion, outlier removal, and missing value interpolation.
}
\references{
1. Example data is from https://smear.avaa.csc.fi/download. It includes particle number concentrations in SMEAR I Varrio forest.

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.

4. Dowle, M., Srinivasan, A., Gorecki, J., Short, T., Lianoglou, S., Antonyan, E., 2017. data.table: Extension of 'data.frame', 1.10.4-3 ed, http://r-datatable.com.

5. Dowle, M., Srinivasan, A., 2021. data.table: Extension of `data.frame`. R package version 1.14.0. https://CRAN.R-project.org/package=data.table.

6. Wallig, M., Microsoft & Weston, S. 2020. foreach: Provides Foreach Looping Construct. R package version 1.5.0. https://CRAN.R-project.org/package=foreach.

7. Ooi, H., Corporation, M. & Weston, S. 2019. doParallel: Foreach Parallel Adaptor for the 'parallel' Package. R package version 1.0.15. https://CRAN.R-project.org/package=doParallel.

8. Zeileis, A. & Grothendieck, G. 2005. zoo: S3 infrastructure for regular and irregular time series. Journal of Statistical Software, 14(6):1-27.

9. Zeileis, A., Grothendieck, G. & Ryan, J.A. 2019. zoo: S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations). R package version 1.8-6. https://cran.r-project.org/web/packages/zoo/.
}

\author{
Chun-Sheng Liang <liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::varidele}, \code{dataprep::obsedele}, \code{dataprep::condextr}, \code{dataprep::shorvalu} and \code{dataprep::optisolu}
}

\examples{
# Combine 4 steps in one function
# In dataprep(data,5,65,4), 26 variables, 1097 outliers and 699 observations will be deleted
# Besides, 6012 missing values will be replaced
# Setting optimal=T will get optimized result
# Here, for executing time reason, a smaller example is used to show
dataprep(data[1:60,c(1,4,18:19)],3,4,2,
interval=2,times=1,cores=2)

# Check if results are the same
identical(shorvalu(condextr(obsedele(varidele(
data[1:60,c(1,4,18:19)],3,4),3,4,2,cores=2),3,4,2,
interval=2,times=1,cores=2),3,4),
dataprep(data[1:60,c(1,4,18:19)],3,4,2,
interval=2,times=1,cores=2))
}

