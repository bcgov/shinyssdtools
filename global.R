library(shiny)
library(dplyr)
library(readr)
library(slackr)
library(magrittr)
library(DT)
library(ssdca)
library(shinythemes)
library(rhandsontable)

# configure slackr
slackr::slackr_setup(config_file = "./.slackr")

# objects
messages <- c("hmmmm...", "something's not right...", "there's a problem...", "oh dear...", "bit of an issue...")

tech.info <- "This webpage fits species sensitivity distributions to concentration data. 
              The user is able to select more than one distribution and plot the individual fits. 
              The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling 
              statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), 
              Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc)
              and the Bayesian Information Criterion (bic). The prediction is the model averaged (using aicc) 
              estimate of the fit. 
              The percent hazard concentration is the concentration of the chemical 
              which is predicted to affect that percent of the species tested.
              The webpage which was developed by Seb Dalgarno is powered by the ssdca R package by Joe Thorley and Carl Schwarz. 
              For more information see https://github.com/bcgov/ssdca."

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

create_error <- function(message, header = "random") {
  showModal(modalDialog(
    footer = modalButton("Got it"),
    title = ifelse(header == "random", sample(messages, 1), header),
    message
  ))
}

hint <- function(x) HTML(paste0("<font color='grey'>", "Hint: ", x, "</font>"))








