\name{filter_high_cor}
\alias{filter_high_cor}
\title{
Remove highly correlated variables
}
\description{
Identifies and removes numeric columns that have an absolute Pearson
correlation above a given threshold. Pairs are evaluated on the subset
of rows where both columns are non-missing.
}
\usage{
filter_high_cor(data, cols = NULL, cutoff = 0.9,
                method = "pearson", keep = "first",
                verbose = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{cols}{Column indices or names of numeric variables to check.
    If \code{NULL}, all numeric columns are used.}
  \item{cutoff}{Absolute correlation threshold. Variables with
    correlation \code{> cutoff} are candidates for removal.}
  \item{method}{Correlation method. Only \code{"pearson"} is currently
    implemented.}
  \item{keep}{Strategy for keeping one variable from each correlated
    pair: \code{"first"} keeps the first occurrence;
    \code{"highest_var"} keeps the one with higher variance.}
  \item{verbose}{Logical; if \code{TRUE}, prints removed column names.}
}
\details{
The function computes the pairwise-complete Pearson correlation matrix
for the selected numeric columns. For each pair with absolute
correlation above \code{cutoff}, the column to remove is determined by
the \code{keep} strategy.
}
\value{
A data frame with highly correlated variables removed.
}
\examples{
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- x1 * 0.9 + rnorm(n, sd = 0.2)
x4 <- x1 * 0.2 + rnorm(n)
df <- data.frame(x1, x2, x3, x4)
filter_high_cor(df, cutoff = 0.8)
}