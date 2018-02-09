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

extra.dists <- c("pareto")
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
  tags$div(style = "display:inline-block;", x)
}

messages <- c("hmmmm...", "something's not right...", "there's a problem...", "oh dear...", "bit of an issue...")

tech.info <- "This webpage fits species sensitivity distributions to concentration data. 
The user is able to select more than one distribution and plot the individual fits. 
The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling 
statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), 
Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc)
and the Bayesian Information Criterion (bic). The prediction is the model averaged (using aicc) 
estimate of the fit with 95% bootstrap confidence intervals. 
The percent hazard concentration is the concentration of the chemical 
which is predicted to affect that percent of the species tested.

The webpage which was developed by Seb Dalgarno is powered by the ssdca R package by Joe Thorley and Carl Schwarz. 
For more information see https://github.com/bcgov/ssdca."






