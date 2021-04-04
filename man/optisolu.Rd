\name{optisolu}
\alias{optisolu}
\title{
Find an optimal combination of \code{interval} and \code{times} for \code{condextr}
}
\description{
Optimal values of \code{interval} and \code{times} in the proposed conditional extremum based outlier removal method, i.e., \code{condextr} can be searched out after comparing with the traditional "one size fits all" percentile deletion method in deleting outliers. Three parameters are used for this comparison, including sample deletion ratio (SDR), outlier removal ratio (ORR), and signal-to-noise ratio (SNR).
}
\usage{
optisolu(data, start = NULL, end = NULL, group = NULL, interval = 35, times = 10,
top = 0.995, top.error = 0.1, top.magnitude = 0.2,
bottom = 0.0025, bottom.error = 0.2, bottom.magnitude = 0.4,
by = "min", half = 30, cores = NULL)
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
  \item{interval}{
  The interval of observation deletion, i.e., the number of outlier deletions before each observation deletion, is 35 by default. Its values from 1 to \code{interval} will be tested for optimal solution.
}
  \item{times}{
  The number of observation deletions in outlier removal is 10 by default. The values from 1 to \code{times} will be tested for optimal solution.
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
  \item{cores}{
  The number of CPU cores.
}
}
\details{
  The three ratios offer indices to show the quality of outlier removal methods. Besides, other parameters such as new outlier production (NOP) are also important. Since the preprocessing roadmap is based on the ideas of grouping, both flexible and strict constraints for outliers, and interpolation within short period and with effective observed values, the new outlier production is greatly restricted.
}
\value{
  A data frame indicating the quality of outlier remove by \code{condextr} with different values of \code{interval} and \code{times}. A total of 9 columns are listed in it.
\item{case }{Order of combination of \code{interval} and \code{times}}
\item{interval}{
  The interval of observation deletion, i.e., the number of outlier deletions before each observation deletion}
\item{times}{The number of observation deletions in outlier removal}
\item{sdr}{Sample deletion ratio (SDR)}
\item{orr}{Outlier removal ratio (ORR)}
\item{snr}{Signal-to-noise ratio (SNR)}
\item{index}{Quality level of outlier removal based on the three parameters}
\item{relaindex}{A relative form of the index}
\item{optimal}{A Boolean variable to show if the result of conditional extremum is better, in terms of all the three parameters, than the traditional "one size fits all" percentile deletion method in deleting outliers.}
}
\references{
1. Example data is from https://smear.avaa.csc.fi/download. It includes particle number concentrations in SMEAR I Varrio forest.

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.

4. Dowle, M., Srinivasan, A., Gorecki, J., Short, T., Lianoglou, S., Antonyan, E., 2017. data.table: Extension of 'data.frame', 1.10.4-3 ed, http://r-datatable.com.

5. Dowle, M., Srinivasan, A., 2021. data.table: Extension of `data.frame`. R package version 1.14.0. https://CRAN.R-project.org/package=data.table.

6. Wallig, M., Microsoft & Weston, S. 2020. foreach: Provides Foreach Looping Construct. R package version 1.5.0. https://CRAN.R-project.org/package=foreach.

7. Ooi, H., Corporation, M. & Weston, S. 2019. doParallel: Foreach Parallel Adaptor for the 'parallel' Package. R package version 1.0.15. https://CRAN.R-project.org/package=doParallel.
}
\author{
Chun-Sheng Liang <liangchunsheng@lzu.edu.cn>
}

\examples{
# Setting interval as 35 and times as 10 can find optimal solutions
# optisolu(obsedele(data[c(1:4,27:61)],5,39,4),5,39,4,35,10)
# Here, for executing time reason, a smaller example is used to show
# But too small interval and times will not get optimal solutions
\donttest{
optisolu(data[1:50,c(1,4,18:19)],3,4,2,2,1,cores=2)
}
}

