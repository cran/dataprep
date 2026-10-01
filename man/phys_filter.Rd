\name{phys_filter}
\alias{phys_filter}
\title{
Physical limit filtering
}
\description{
Sets values outside user-specified physical limits to \code{NA}. This is commonly used to remove impossible measurements, such as relative humidity outside 0--100\% or negative concentrations. The function supports per-column limits and grouping.
}
\usage{
phys_filter(data, cols = NULL, min_val = NULL, max_val = NULL,
            group = NULL, date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names of numeric variables to filter. If \code{NULL}, all numeric columns are used.}
  \item{min_val}{Lower limit(s). If a single value is given, it is applied to all selected columns. Otherwise, provide a vector with length equal to the number of selected columns.}
  \item{max_val}{Upper limit(s). Same behavior as \code{min_val}.}
  \item{group}{Optional grouping column (currently not used but reserved for consistency).}
  \item{date_col}{Optional time column (not used).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
Values strictly less than \code{min_val} or strictly greater than \code{max_val} are set to \code{NA}. If \code{min_val} or \code{max_val} is \code{NULL}, that bound is not checked.
}
\value{
A data frame or vector with out-of-range values set to \code{NA}.
}
\examples{
phys_filter(data[1:100, c(1, 4, 17:19)], cols = 3:5, min_val = 0, max_val = 1000)
}