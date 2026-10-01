## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 5.5,
  out.width = "75%",
  fig.retina = 2
)

## -----------------------------------------------------------------------------
library(dataprep)
set.seed(1)

# The size-bin columns are the ones whose names are numeric
# (1.00, 1.12, ..., 1000). This helper returns their integer
# positions, excluding the four non-size columns (`date`,
# `tconc`, `TPNC`, `monthyear`).
size_bin_cols <- function(x) {
  grep("^[-+]?[0-9]*\\.?[0-9]+$", names(x))
}

## -----------------------------------------------------------------------------
train <- data[1:5000, c("date", "monthyear", "7.94", "8.91", "10")]
test  <- data[5001:6000, c("date", "monthyear", "7.94", "8.91", "10")]

plan <- prep_fit(
  train,
  cols       = 3:5,
  group      = 2,
  steps      = c("varidele", "outlier", "impute", "scale"),
  fraction   = 0.5,
  method_outlier = "iqr",
  method_impute  = "linear",
  scale_method   = "zscore"
)

str(plan, max.level = 2)

## -----------------------------------------------------------------------------
test_clean <- prep_transform(plan, test)
head(test_clean)

## -----------------------------------------------------------------------------
names(plan)
names(plan$params)

## ----eval = FALSE-------------------------------------------------------------
# steps = c("varidele", "obsedele", "outlier", "impute", "scale")

## -----------------------------------------------------------------------------
test_reordered <- test[, c("date", "10", "8.91", "7.94", "monthyear")]
test_reordered_clean <- prep_transform(plan, test_reordered)
identical(names(test_reordered_clean), names(test_clean))

## ----error = TRUE-------------------------------------------------------------
try({
test_missing <- test[, c("date", "monthyear", "7.94", "8.91")]
prep_transform(plan, test_missing)
})

## -----------------------------------------------------------------------------
res <- dataprep(
  data[1:1000, ],
  cols     = size_bin_cols(data[1:1000, ]),
  group    = 4,
  interval = 5,
  times    = 3
)
dim(res)

## -----------------------------------------------------------------------------
data_report(data1, cols = 3:7, verbose = TRUE)
invisible(data_report(data1, cols = 3:7))

## ----eval = FALSE-------------------------------------------------------------
# # 1. Inspect the raw data
# data_report(data, cols = size_bin_cols(data), verbose = TRUE)
# 
# # 2. Fit a plan on the training split
# train <- data[1:5000, ]
# plan  <- prep_fit(
#   train,
#   cols     = size_bin_cols(train),
#   group    = 4,
#   steps    = c("varidele", "obsedele", "outlier", "impute", "scale"),
#   fraction = 0.5,
#   method_outlier = "iqr",
#   method_impute  = "linear",
#   scale_method   = "zscore"
# )
# 
# # 3. Apply the same plan to the test split
# test       <- data[5001:6000, ]
# test_clean <- prep_transform(plan, test)
# 
# # 4. Model on the cleaned training set,
# #    predict on the cleaned test set
# fit  <- lm(`7.94` ~ `8.91` + `10`,
#            data = plan$final_data)
# pred <- predict(fit, newdata = test_clean)

## -----------------------------------------------------------------------------
sessionInfo()

