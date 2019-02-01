library(shiny)
library(dplyr)
library(readr)
library(slackr)
library(magrittr)
library(ggplot2)
library(DT)
library(ssdtools)
library(rhandsontable)
library(shinyjs)
library(RColorBrewer)
library(shinycssloaders)

# configure slackr
slackr::slackr_setup(config_file = "./.slackr")

# objects
pals <- brewer.pal.info[which(brewer.pal.info$category == "qual"),] %>% row.names

source("translation.R")

extra.dists <- "pareto"
default.dists <- c("lnorm", "llog", "gompertz", "lgumbel", "gamma", "weibull")

# functions
label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"

inline = function (x) {
  tags$div(style = "display:inline-block;", x)
}

hint <- function(x) HTML(paste0("<font color='grey'>", x, "</font>"))

zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}







