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
The species sensitivity distribution (SSD) is the most widely used method for getting water quality benchmarks to characterize effects of chemical contaminants for water quality or ecological risk assessment [@fox_2020]. This typically involves estimating the concentration of a chemical that affects a 5% of the species considered [@posthuma_species_2001]. The `ssdtools` R package, developed for the British Columbia Ministry of Environment and Climate Change Strategy [@ssdtools], has recently advanced SSD methods by providing model averaging using information-theoretic criteria and the construction of confidence intervals using bootstrapping [@fox_2020]. 

`shinyssdtools` is a web-based graphical user interface (GUI) and R package built with the Shiny web framework [@shiny]. It provides access to the core functionality in the `ssdtools` R package via a simple and modern user interface. It also provides the following functionality in addition to `ssdtools`: translation to French; generation of R code to reproduce results from a session; further customization of plot and table outputs; download of tables and plot outputs. `shinyssdtools` can be accessed from the web or it can be run locally by installing the R package. The advantage of using a web application over standalone software is that it is possible to ensure that the user is using the most up-to-date version. The advantage of developing the application as an R package is that the source code is openly visible and is available for local modification. 

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

# Statement of Need
`ssdtools` is an R package that has provided recent advances in SSD methodology [@fox_2020]. `shinyssdtools` provides access to this functionality via a simple, modern GUI without requiring users to be familiar with the R programming language. Data can be easily uploaded to the application, the interface can be viewed in multiple languages (French and English) and R code output is provided so that analyses can be reproduced or shared. `shinyssdtools` was developed for the Province of British Columbia with input from the governments of Canada as well as Australia and New Zealand and has been used by the governments of B.C. and Canada to derive water quality benchmarks.

# Contribution
SSD Toolbox is standalone software performing similar functionality to `shinyssdtools` developed by the US Environmental Protection Agency [@ssdtoolbox_2020]. It can be downloaded as a Windows executable file and requires installation of version 9.5 of the MATLAB® Runtime Compiler (MCR) from Mathworks, which requires 3.75GB of hard disk space. `shinyssdtools` provides a more appealing user interface, is open-source, is operating-system-agnostic and does not require local installation of bulky software. 

The similarly named `shinyssd` is an alternative open source Shiny web application to fit SSDs that is also bundled as an R package [@dandrea_shinyssd_2019]. `shinyssdtools` contributes by being bilingual; providing additional distributions including the gamma, Gompertz and log-Gumbel; by allowing the user to model average and by providing the R code to replicate the analysis.

# Acknowledgements

We acknowledge contributions from Angeline Tillmanns, Marianne Métivier, Andy Teucher, David Fox, Carl Schwarz and Joe Thorley.
Development of `shinyssdtools` was funded by the Ministry of Environment and Climate Change Strategy, British Columbia. The governments of British Columbia, Canada and Australia and New Zealand have also contributed to its development.

## References
