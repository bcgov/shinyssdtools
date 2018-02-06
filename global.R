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
library(shinyWidgets)

helpers <- 'helpers/'

source(paste0(helpers, 'functions.R'), local = T)
source(paste0(helpers, 'auth.R'), local = T)

# red mandatory star
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"







