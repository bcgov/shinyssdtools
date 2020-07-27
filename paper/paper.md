---
title: 'shinyssdtools: A web application for fitting Species Sensitivity Distributions (SSDs)'
date: '2020-06-02'
authors:
- affiliation: 1
  name: Seb Dalgarno
  orcid: 0000-0002-3658-4517
affiliations:
- name: Poisson Consulting, Nelson, British Columbia
  index: 1
bibliography: paper.bib
tags:
   - R
   - Shiny
   - ssdtools
   - species sensitivity distributions
---

# Summary

`shinyssdtools` is a Shiny [@shiny] web application and R package for fitting Species Sensitivity Distributions (SSDs). It was developed for the Province of British Columbia with input from the governments of Canada as well as Australia and New Zealand. The shinyssdtools app has been used by the governments of B.C. and Canada to derive water quality benchmarks.

SSDs are used to estimate the concentration of a chemical that affects a certain percentage (typically 5%) of the species considered [@posthuma_species_2001]. 
The `ssdtools` R package [@ssdtools] allows model averaging using information-theoretic criteria and the construction of confidence intervals using bootstrapping [@ssdtools].

`shinyssdtools` provides access to the core functionality in the `ssdtools` R package, and the following functionality in addition: translation to French; generation of R code to reproduce results from a session; further customization of plot and table outputs; download of tables and plot outputs. 

### Graphical User Interface

The `shinyssdtools` web application has six navigational tabs: 

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
1. About
   - version information, explanation of abbreviations and references
1. User guide
   - Step-by-step guide to proper use of the application.

![shinyssdtools user interface](shinyssdtools_ui.png)

# Installation

The `shinyssdtools` application is available at https://bcgov-env.shinyapps.io/ssdtools/.
`shinyssdtools` is bundled as an R package [@r] to allow the user to install and run locally using just three lines of R code:

```r
install.packages('remotes')
remotes::install_github('bcgov/shinyssdtools')
shinyssdtools::run_ssdtools_app()
```

# Contribution

The similarly named `shinyssd` is an alternative open source Shiny web application to fit SSDs that is also bundled as an R package [@dandrea_shinyssd_2019].
`shinyssdtools` contributes by being bilingual; providing additional distributions including the gamma, Gompertz and log-Gumbel; by allowing the user to model average and by providing the R code to replicate the analysis.

# Acknowledgements

We acknowledge contributions from Angeline Tillmanns, Marianne Métivier, Andy Teucher, David Fox, Carl Schwarz and Joe Thorley.
Development of `shinyssdtools` was funded by the Ministry of Environment and Climate Change Strategy, British Columbia. The governments of British Columbia, Canada and Australia and New Zealand have also contributed to its development.

## References
