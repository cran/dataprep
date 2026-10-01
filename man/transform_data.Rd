\name{transform_data}
\alias{transform_data}
\title{
Transform and standardize numeric variables
}
\description{
Applies a transformation (log, sqrt, inverse, Box-Cox, Yeo-Johnson) or standardization (z-score, centering, scaling, min-max, robust scaling) to selected numeric columns. Group-wise transformation is supported.
}
\usage{
transform_data(data, cols = NULL, method = "log", group = NULL,
               lambda = 1.0, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame or matrix containing numeric variables.}
  \item{cols}{The column indices or names of selected variables. If NULL, all columns are used.}
  \item{method}{Transformation method. One of \code{"log"}, \code{"log1p"}, \code{"sqrt"}, \code{"inverse"}, \code{"boxcox"}, \code{"yeojohnson"}, \code{"zscore"}, \code{"center"}, \code{"scale"}, \code{"minmax"}, \code{"robust"}.}
  \item{group}{Optional grouping column for group-wise transformation.}
  \item{lambda}{Parameter for Box-Cox and Yeo-Johnson transformations (default 1.0).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
For \code{"boxcox"}, values must be positive. For \code{"yeojohnson"}, both positive and negative values are allowed. Standardization methods ignore NA values.
}
\value{
A data frame with transformed variables.
}
\examples{
# Log transformation
transform_data(data[1:100, c(1, 4, 17:19)], cols = 3:5, method = "log")
# Z-score standardization by group
transform_data(data[1:100, c(1, 4, 17:19)], cols = 3:5, method = "zscore", group = 2)
}