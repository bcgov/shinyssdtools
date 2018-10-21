library(shiny)
library(dplyr)
library(readr)
library(slackr)
library(magrittr)
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

tech.info <- "This webpage fits species sensitivity distributions to concentration data. 
              The user is able to select more than one distribution and plot the individual fits. <br/><br/>
The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling 
statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), 
Akaike’s Information Criterion (aic), Akaike’s Information Criterion corrected for sample size (aicc), 
Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight).
The prediction is the model averaged (using aicc) 
estimate of the fit. 
The percent hazard concentration is the concentration of the chemical 
which is predicted to affect that percent of the species tested.<br/><br/>
To cite package ‘ssdtools’ in publications use:<br/>
Joe Thorley and Carl Schwarz (2018). ssdtools: Species Sensitivity
Distributions. R package version 0.0.1.9002.<br/><br/>
To cite the web app use:<br/>
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. 
Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://poissonconsulting.shinyapps.io/ssdtools/"

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







