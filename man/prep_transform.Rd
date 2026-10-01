\name{prep_transform}
\alias{prep_transform}
\title{
Apply a preprocessing plan to new data
}
\description{
Uses a preprocessing plan created by \code{prep_fit} to transform new data. All parameters (thresholds, statistics, etc.) are taken from the plan and are not re‑estimated, thus ensuring that the same preprocessing logic is applied consistently and without data leakage.
}
\usage{
prep_transform(plan, newdata, verbose = FALSE)
}
\arguments{
  \item{plan}{A preprocessing plan object returned by \code{prep_fit}.}
  \item{newdata}{A data frame containing the new data (e.g., test set) to be transformed. It must contain the same columns as the original training data used to build the plan.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress messages.}
}
\details{
The function iterates through the steps stored in \code{plan} and applies the corresponding operations using the saved parameters. For example, if the \code{"outlier"} step used group‑specific thresholds, those exact thresholds are applied to the new data grouped in the same way.
}
\value{
A data frame after applying the preprocessing steps specified in \code{plan}. The output contains the same variables as the training data after preprocessing.
}
\examples{
# Build plan
plan <- prep_fit(data[1:100, c(1, 4, 47:49)], cols = 3:5, group = 2)
# Transform new data
newdata <- data[101:200, c(1, 4, 47:49)]
cleaned <- prep_transform(plan, newdata)
}