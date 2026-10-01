## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 6, fig.height = 5.5,
                      out.width = "75%", fig.retina = 2)

## -----------------------------------------------------------------------------
library(dataprep)

## -----------------------------------------------------------------------------
df <- data.frame(
  date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:4 * 600,
  x    = c(1, NA, NA, NA, 5)
)

obsedele(df, cols = "x", half = 30)

## -----------------------------------------------------------------------------
df <- data.frame(
  date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 600,
  x    = c(1, NA, NA, NA, NA, NA, NA, NA, NA, 5),
  y    = c(NA, NA, NA, NA, 2, NA, NA, NA, NA, NA)
)
nrow(obsedele(df, cols = c("x", "y"), half = 60))

## -----------------------------------------------------------------------------
result_015 <- dataprep::dataprep(data, cols = 5:65, group = 4)
result_017 <- dataprep(data, cols = 5:65, group = 4)

kept_only_by_017 <- dplyr::anti_join(result_017, result_015, by = "date")
kept_only_by_015 <- dplyr::anti_join(result_015, result_017, by = "date")

## -----------------------------------------------------------------------------
sessionInfo()

