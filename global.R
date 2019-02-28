library(shiny)
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)
library(DT)
library(ssdtools)
library(rhandsontable)
library(shinyjs)
library(RColorBrewer)
library(shinycssloaders)

source("translation.R")
source("functions.R")

# objects
pals <- brewer.pal.info[which(brewer.pal.info$category == "qual"),] %>% row.names

extra.dists <- "pareto"
default.dists <- c("lnorm", "llog", "gompertz", "lgumbel", "gamma", "weibull")

