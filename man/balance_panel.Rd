\name{balance_panel}
\alias{balance_panel}
\title{
Balance panel data
}
\description{
Transforms an unbalanced panel dataset into a balanced one by either keeping only complete units or filling missing time points with a specified value.
}
\usage{
balance_panel(data, unit_col = NULL, time_col = NULL,
              fill = NA_real_, method = c("fill", "complete"),
              verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing panel data.}
  \item{unit_col}{Column index or name identifying the individual unit (e.g., station ID, subject ID). If \code{NULL}, the first column is assumed.}
  \item{time_col}{Column index or name identifying the time point. If \code{NULL}, the first column matching \code{date} is used.}
  \item{fill}{Value used to fill missing combinations when \code{method = "fill"}.}
  \item{method}{Balancing method: \code{"fill"} adds rows for missing unit-time combinations and fills \strong{numeric} non-key columns with \code{fill}; non-numeric columns of the added rows are left as \code{NA}. \code{"complete"} keeps only units that have observations at every time point.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
The function assumes that the time column is sorted and contains no duplicates within a unit. If duplicates exist, they should be handled beforehand.
}
\value{
A balanced data frame.
}
\examples{
balance_panel(data[1:200, c(1, 4, 17:19)], unit_col = 2, time_col = 1, method = "complete")
}