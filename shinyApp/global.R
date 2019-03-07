# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

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

