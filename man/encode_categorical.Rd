\name{encode_categorical}
\alias{encode_categorical}
\title{
Encode categorical variables
}
\description{
Converts factor or character columns into numeric representations using label encoding, frequency encoding, or one-hot encoding. One-hot encoding creates binary indicator columns for each category.
}
\usage{
encode_categorical(data, cols = NULL, method = "label",
                   group = NULL, prefix = "enc_", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing categorical variables.}
  \item{cols}{The column indices or names of selected categorical variables. If \code{NULL} (default), all factor and character columns are selected automatically. An error is raised if any explicitly selected column is not categorical.}
  \item{method}{Encoding method. One of \code{"label"} (integer labels), \code{"frequency"} (frequency of each category), or \code{"onehot"} (binary indicator columns).}
  \item{group}{Optional grouping column for grouped frequency encoding.}
  \item{prefix}{Prefix for one-hot encoded column names (used only when \code{method = "onehot"}).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with encoded variables. For one-hot encoding, original categorical columns are replaced by binary columns.
}
\examples{
# Label encoding
encode_categorical(data[1:100, c(1, 4)], cols = 2, method = "label")
# One-hot encoding
encode_categorical(data[1:100, c(1, 4)], cols = 2, method = "onehot")
}