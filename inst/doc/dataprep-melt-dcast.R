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

## ----eval = FALSE-------------------------------------------------------------
# Sys.setenv(DATAPREP_RUN_BENCHMARK = "1")
# source(system.file("benchmark_melt_dcast.R", package = "dataprep"))

## -----------------------------------------------------------------------------
df <- data.frame(
  id       = 1:3,
  category = factor(c("a", "b", "c")),
  v1       = c(1.1, 2.2, 3.3),
  v2       = c(4.4, 5.5, 6.6)
)
melt(df, id.vars = c("id", "category"))

## -----------------------------------------------------------------------------
melt(df, measure.vars = c("v1", "v2"))

## -----------------------------------------------------------------------------
melt(df)

## -----------------------------------------------------------------------------
df_na <- data.frame(
  id = 1:3,
  x  = c(1, NA, 3),
  y  = c(4, 5, NA)
)
melt(df_na, id.vars = "id",
     variable.name = "var", value.name = "val",
     na.rm = TRUE)

## -----------------------------------------------------------------------------
wide50 <- data.frame(id = 1:100,
                     matrix(rnorm(100 * 50), ncol = 50))
res_row <- melt(wide50, id.vars = "id", major = "row")
res_col <- melt(wide50, id.vars = "id", major = "col")
identical(as.data.frame(res_row), as.data.frame(res_col))

## -----------------------------------------------------------------------------
options(dataprep.cores = 4L)
melt(df, id.vars = "id")
options(dataprep.cores = NULL)

## -----------------------------------------------------------------------------
long <- melt(df, id.vars = c("id", "category"))
dcast(long, id = c("id", "category"),
      variable = "variable", value = "value")

## -----------------------------------------------------------------------------
dcast(long, formula = id + category ~ variable,
      value.var = "value")

## -----------------------------------------------------------------------------
dcast(long, id = c("id", "category"),
      variable = "variable", value = "value",
      fill = 0)

## -----------------------------------------------------------------------------
long_dup <- data.frame(
  id       = c(1, 1, 2),
  variable = c("x", "x", "x"),
  value    = c(1, 2, 3)
)
dcast(long_dup, id = "id",
      variable = "variable", value = "value",
      fun.aggregate = mean)

## -----------------------------------------------------------------------------
long_na <- data.frame(
  id       = c(1, 1, 2, 2),
  variable = c("x", "y", "x", "y"),
  value    = c(1, NA, 3, 4)
)
dcast(long_na, id = "id",
      variable = "variable", value = "value",
      na.rm = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# Sys.setenv(DATAPREP_RUN_BENCHMARK = "1")
# source(system.file("benchmark_melt_dcast.R", package = "dataprep"))
# melt_all_engines(10000L, n_id = 1L, n_val = 9L)
# dcast_all_engines(1000L, n_id = 2L, n_val = 5L)

## -----------------------------------------------------------------------------
wide  <- data.frame(id = 1:5, a = rnorm(5), b = rnorm(5))
long  <- melt(wide, id.vars = "id")
back  <- dcast(long, id = "id",
               variable = "variable", value = "value")
all.equal(as.data.frame(back)[order(back$id), c("a", "b")],
          wide[, c("a", "b")],
          tolerance = 1e-12)

## -----------------------------------------------------------------------------
sessionInfo()

