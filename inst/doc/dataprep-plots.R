## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 5.5,
  out.width = "80%",
  fig.retina = 2
)

## -----------------------------------------------------------------------------
library(dataprep)
library(ggplot2)

## ----fig.height = 5-----------------------------------------------------------
descplot(data1, cols = 3:7) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 4-----------------------------------------------------------
descplot(data1, cols = 3:7,
         stats = c("na", "min", "max", "IQR")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 5-----------------------------------------------------------
descplot(data1, cols = 3:7) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 3-----------------------------------------------------------
descplot(data1, cols = 3:7, stats = c("min", "max", "IQR")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 4.5---------------------------------------------------------
descplot(data, cols = 5:65)

## ----fig.height = 4-----------------------------------------------------------
descdata(data1, cols = 3:7, stats = c(2, 3, 4, 7:9))

## ----fig.height = 6, fig.width = 7--------------------------------------------
percplot(data1, cols = 3:7, group = 2) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 3, fig.width = 7--------------------------------------------
percplot(data1, cols = 3:7, group = 2, part = "top") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 3, fig.width = 7--------------------------------------------
percplot(data1, cols = 3:7, group = 2, part = "bottom") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 4, fig.width = 7--------------------------------------------
percplot(data, cols = 5:65, group = 4) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----fig.height = 4, fig.width = 7--------------------------------------------
percplot(data, cols = 5:65, group = 4, num_xaxis = "numeric") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## -----------------------------------------------------------------------------
percdata(data1, cols = 3:7, group = 2, part = "top")

## ----fig.height = 4, fig.width = 7--------------------------------------------
percplot(data1, cols = 3:7, group = 2) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::labs(title = "Percentile plots by month",
                x = "Variable", y = "Value") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

## ----eval = FALSE-------------------------------------------------------------
# # 1. Overview of the whole table
# data_report(data, cols = 5:65)
# 
# # 2. Per-column NA run statistics
# na_diagnose(data, cols = 5:65)
# 
# # 3. Descriptive statistics of the raw data
# descplot(data, cols = 5:65)
# 
# # 4. Percentile curves of the raw data
# percplot(data, cols = 5:65, group = 4)

## ----eval = FALSE-------------------------------------------------------------
# cleaned <- dataprep(data, cols = 5:65, group = 4)
# 
# percplot(
#   rbind(
#     transform(data[names(cleaned)], g = "original"),
#     transform(cleaned,              g = "preprocessed")
#   ),
#   cols  = 5:ncol(cleaned),
#   group = ncol(cleaned) + 1
# )

## -----------------------------------------------------------------------------
sessionInfo()

