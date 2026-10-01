\name{prep_fit}
\alias{prep_fit}
\title{
Build a preprocessing plan on training data to prevent data leakage
}
\description{
Computes and stores all preprocessing parameters (such as missing fractions, outlier thresholds, imputation methods, and scaling statistics) using only the training data. The returned plan object can then be applied to new data via \code{prep_transform} without re-estimating any parameters, ensuring that information from the test set does not leak into the preprocessing steps.
}
\usage{
prep_fit(data, steps = c("varidele", "obsedele", "outlier", "impute", "scale"),
         cols = NULL, group = NULL, date_col = NULL,
         fraction = 0.25, top = 0.995, bottom = 0.0025,
         by = "min", half = 30, method_outlier = "iqr",
         coef = 1.5, method_impute = "linear",
         scale_method = "zscore", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing the training data. All preprocessing parameters are estimated from this data only.}
  \item{steps}{A character vector specifying the preprocessing steps to include. Available steps are \code{"varidele"}, \code{"obsedele"}, \code{"outlier"}, \code{"impute"}, and \code{"scale"}. The default is all five steps in that order.}
  \item{cols}{Column indices or names of numeric variables to be processed. If \code{NULL}, all numeric columns are selected.}
  \item{group}{Optional grouping column index or name used for grouped outlier detection, imputation, or scaling.}
  \item{date_col}{Optional time column index or name used by \code{obsedele} for time‑aware observation deletion.}
  \item{fraction}{Missing fraction threshold for variable deletion in the \code{"varidele"} step.}
  \item{top}{Top percentile used by the percentile outlier detection method.}
  \item{bottom}{Bottom percentile used by the percentile outlier detection method.}
  \item{by}{Time extension unit for observation deletion (see \code{obsedele}).}
  \item{half}{Half window size in minutes for consecutive missing value deletion (see \code{obsedele}).}
  \item{method_outlier}{Outlier detection method: \code{"iqr"}, \code{"mad"}, or \code{"percentile"}.}
  \item{coef}{Coefficient for IQR or MAD outlier detection.}
  \item{method_impute}{Missing value imputation method: \code{"linear"}, \code{"locf"}, \code{"nocb"}, \code{"mean"}, or \code{"median"}.}
  \item{scale_method}{Scaling method used in the \code{"scale"} step: \code{"zscore"}, \code{"minmax"}, or \code{"robust"}. Other values fall back to identity scaling (center = 0, scale = 1).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress messages.}
}
\details{
The function follows the \emph{fit-transform} paradigm. All thresholds, means, standard deviations, and group‑wise statistics are computed solely from \code{data} (training set). Applying these saved parameters to test data with \code{prep_transform} guarantees that no information from the test set influences the preprocessing, thereby preventing data leakage.
}
\value{
A list of class \code{prep_plan} containing all parameters learned from the training data. The list includes:
\itemize{
  \item \code{steps}: the ordered preprocessing steps.
  \item \code{params}: a list of parameters such as missing fractions, outlier thresholds, imputation method, and scaling center/scale values.
  \item \code{data_info}: original column names, selected column indices, and other metadata.
  \item \code{final_data}: the fully preprocessed training data (optional, useful for inspection).
}
}
\examples{
# Build a preprocessing plan using the first 100 rows as training data
plan <- prep_fit(data[1:100, c(1, 4, 47:49)], cols = 3:5, group = 2)
# Apply the plan to new data
newdata <- data[101:200, c(1, 4, 47:49)]
cleaned <- prep_transform(plan, newdata)
}