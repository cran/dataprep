## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataprep)
library(ggplot2)
library(scales)

## ----fig.height=4, fig.show='hold', fig.width=6-------------------------------
# Descriptive statistics
descplot(data,5,65)

## ----fig.height=3, fig.show='hold', fig.width=4-------------------------------
# Selected descriptive statistics, equal to descdata(data,5,65,c('na','min','max','IQR'))
descplot(data,5,65,c(2,7:9))

## ----fig.height=5, fig.show='hold', fig.width=6-------------------------------
# Descriptive statistics
descplot(data1,3,7)+
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,vjust=1.1))

## ----fig.height=2.5, fig.show='hold', fig.width=6-----------------------------
# Selected descriptive statistics, equal to descplot(data1,3,7,c('min','max','IQR'))
descplot(data1,3,7,7:9)+
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,vjust=1.1))

## ----fig.height=4, fig.show='hold', fig.width=6-------------------------------
# Top and bottom percentiles
percplot(data,5,65,4)

## ----fig.height=2.5, fig.show='hold', fig.width=6-----------------------------
# Top percentiles
percplot(data,5,65,4,part=1)

## ----fig.height=2.5, fig.show='hold', fig.width=6-----------------------------
# Bottom percentiles
percplot(data,5,65,4,part=0)

## ----fig.height=5, fig.show='hold', fig.width=6-------------------------------
# Top and bottom percentiles
percplot(data1,3,7,2)+
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,vjust=1.1))

## ----fig.height=3, fig.show='hold', fig.width=6-------------------------------
# Top percentiles
percplot(data1,3,7,2,part=1)+
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,vjust=1.1))

## ----fig.height=3, fig.show='hold', fig.width=6-------------------------------
# Bottom percentiles
percplot(data1,3,7,2,part=0)+
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,vjust=1.1))

