## upload button
library(shiny)
library(shinyjs)
library(rdrop2)
library(dplyr)
library(readr)
library(slackr)
library(lubridate)
library(magrittr)
library(scales)
library(DT)
library(ssdca)
library(shinyWidgets)

helpers <- 'helpers/'

source(paste0(helpers, 'functions.R'), local = T)
source(paste0(helpers, 'auth.R'), local = T)

extra.dists <- c("burr", "pareto")
default.dists <- c("lnorm", "llog", "gompertz", "lgumbel", "gamma", "weibull")

# red mandatory star
label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"

inline = function (x) {
  tags$div(style="display:inline-block;", x)
}







