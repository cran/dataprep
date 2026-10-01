\name{season_flag}
\alias{season_flag}
\title{
Season flag
}
\description{
Creates season, month, or quarter indicator from a time column. Useful for grouped analysis and preprocessing by season.
}
\usage{
season_flag(data, date_col = NULL, type = c("season", "month", "quarter"),
            verbose = FALSE)
}
\arguments{
  \item{data}{A data frame with a time column.}
  \item{date_col}{Time column index or name. If \code{NULL}, the first column matching \code{date} is used.}
  \item{type}{Type of flag: \code{"season"} (winter, spring, summer, autumn), \code{"month"} (abbreviated month), or \code{"quarter"} (Q1-Q4).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A character vector or factor containing the requested flag.
}
\examples{
season_flag(data[1:200, c(1, 4, 17:19)], date_col = 1, type = "season")
}