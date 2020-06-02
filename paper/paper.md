---
title: 'shinyssdtools: A web application for fitting Species Sensitivity Distributions'
date: '2020-06-02'
authors:
- affiliation: 1
  name: Seb Dalgarno
  orcid: 0000-0002-3658-4517
affiliations:
  index: 1
  name: Poisson Consulting, Nelson, British Columbia
bibliography: paper.bib
tags:
   - R
   - Shiny
   - ssdtools
   - species sensitivity distributions
output: 
  html_document: 
    keep_md: yes
---

# Summary

`shinyssdtools` is a Shiny [@shiny] web application for fitting Species Sensitivity Distributions (SSDs).
The application provides a Graphical User Interface (GUI) to the `ssdtools` R package [@ssdtools].

SSDs are used to estimate the concentration of a chemical that affects a certain percentage (typically 5%) of the species considered [@posthuma_species_2001]. 
The `ssdtools` R package allows model averaging using information-theoretic criteria and the construction of confidence intervals using bootstrapping [@ssdtools].

The `shinyssdtools` web application, which is available in French or English, provides access to the core functionality of `ssdtools` via five navigational tabs. 

1. Data
   - Upload a dataset or enter data manually.
1. Fit
   - Select distributions.
   - Calculate information-theoretic criteria.
1. Predict
   - Estimate the concentration that affects a specific percentage of the species.
   - Calculate confidence limits using bootstrapping.
1. R code
   - Copy the R code required to reproduce the results.
1. User guide
   - Step-by-step guide to proper use of the application.
   
The web application also allows the user to download plots and tables, calculate goodness-of-fit criteria, and estimate the percentage of species affected by a specific concentration.

The `shinyssdtools` web application is available at https://bcgov-env.shinyapps.io/ssdtools/.
The web application is bundled as an R package [@r] to allow user to easily install and run locally using the following R code:

```r
install.packages("remotes")
remotes::install_github(“bcgov/shinyssdtools”)
shinyssdtools::run_ssdtools_app()
```

![shinyssdtools user interface](shinyssdtools_ui.png)

# Acknowledgements

We acknowledge contributions from Angeline Tillmanns, Marianne Métivier, Andy Teucher, David Fox, Carl Schwarz and Joe Thorley.
Development of `shinyssdtools` was funded by the Ministry of Environment and Climate Change Strategy, British Columbia.

# References
