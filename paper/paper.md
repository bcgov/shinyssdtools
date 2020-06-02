title: 'Shinyssdtools: An R package and web application for fitting Species Sensitivity Distributions'

authors:

- affiliation: 1
  name: Seb Dalgarno
  orcid: 0000-0002-3658-4517
- affiliation: 1
  name: Joe Thorley
  orcid: 0000-0002-7683-4592
  date: '2020-06-02'
  bibliography: paper.bib

tags:

- R
- Shiny
- ssdtools
- species sensitivity distributions

affiliations:

- index: 1
  name: Poisson Consulting Ltd., Nelson, Canada

# Summary

`shinyssdtools` is an R package [@r] and web application built using the Shiny R package [@shiny], a web application framework for R. The application provides access to functionality of the `ssdtools` R package [@ssdtools] to users who are unfamiliar with, or inexperienced using R, via a user-friendly interface. `ssdtools` is used to fit Species Sensitivity Distributions (SSDs) to estimate the percent of species affected by a given concentration of a chemical.

The core functionality of `ssdtools` is exposed to the user via five navigational tabs. A user is able to choose whether to view content in French or English. The navigational tabs include:

1. Data
   - Upload a dataset, add data via an interactive spreadsheet, or use a demo dataset.
1. Fit
   - Select distributions and view and download plot of fitted distributions.
   - View and download goodness of fit table.
1. Predict
   - Select the threshold % species affected to calculate estimated hazard concentration or select concentration to calculate the percentage of species affected by a specified concentration.
   - View and download plot.
   - Calculate confidence limits and view and download as table.
1. R code
   - Copy the R code required to reproduce results.
1. User guide
   - Step-by-step guide to proper use of the application.

The `shinyssdtools` web application can be run locally by installing the `shinyssdtools` R package, or online at: https://bcgov-env.shinyapps.io/ssdtools/. The following code will run the app locally:

```r
# install.packages("remotes")
# remotes::install_github(“bcgov/shinyssdtools”)
library(shinyssdtools)
run_ssdtools_app()
```

![shinyssdtools user interface](shinyssdtools_ui.png)

# Acknowledgements

We acknowledge contributions from Angeline Tillmanns, Marianne Métivier, and Andy Teucher.
Development of `shinyssdtools` was funded by the Ministry of Environment and Climate Change Strategy, British Columbia.

# References
