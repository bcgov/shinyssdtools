---
title: 'shinyssdtools: A web application for fitting Species Sensitivity Distributions (SSDs)'
date: '2020-11-18'
authors:
- affiliation: 1
  name: Seb Dalgarno
  orcid: 0000-0002-3658-4517
affiliations:
- name: Poisson Consulting Ltd., Nelson, British Columbia
  index: 1
bibliography: paper.bib
tags:
   - R
   - Shiny
   - ssdtools
   - species sensitivity distributions
---

# Summary
The species sensitivity distribution (SSD) is the most widely used method for getting water quality benchmarks to characterize effects of chemical contaminants for water quality or ecological risk assessment [@fox_2020].
This typically involves estimating the concentration of a chemical that affects 5% of the species considered [@posthuma_species_2001].
The `ssdtools` R package [@ssdtools] has recently advanced SSD methods by providing model averaging using information-theoretic criteria and the construction of confidence intervals using bootstrapping [@fox_2020]. 

`shinyssdtools` is a web-based graphical user interface (GUI) to the `ssdtools` R package.
`shinyssdtools`, which was developed in the Shiny web framework [@shiny], is an R package in its own right.
As well as providing access to the core functionality in `ssdtools`, it also offers the following value-added features: a bilingual (English/French) interface, customization and downloads of plots and tables and generation of R scripts for reproducibility. 
`shinyssdtools` can be accessed from the web or it can be run locally by installing the R package.

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
`ssdtools` is an R package that has provided recent advances in SSD methodology [@fox_2020]. 
`shinyssdtools` provides access to this functionality via a simple, modern GUI without requiring users to be familiar with the R programming language. 
Data can be easily uploaded to the application, the interface can be viewed in multiple languages (French and English) and R code output is provided so that analyses can be reproduced or shared. 
`shinyssdtools` was initially developed for the Province of British Columbia with input from the governments of Canada and Australia and has been used by the governments of British Columbia and Canada to derive water quality benchmarks.

# Contribution
 
All historical and existing SSD software was recently reviewed by @fox_2020 who considered `ssdtools`, `shinyssdtools` and `SSD Toolbox` to be the most important contributions because they provide model averaging.

`SSD Toolbox` is standalone software performing similar functionality to `shinyssdtools` developed by the US Environmental Protection Agency [@ssdtoolbox_2020]. 
It can be downloaded as a Windows executable file and requires installation of version 9.5 of the MATLAB® Runtime Compiler (MCR) from Mathworks, which requires 3.75 GB of hard disk space. 
`shinyssdtools` provides a more appealing user interface, is open-source and does not require local installation of bulky software. 

The similarly named `shinyssd` is an alternative open source Shiny web application to fit SSDs that is also bundled as an R package [@dandrea_shinyssd_2019]. 
`shinyssdtools` contributes by being bilingual; providing additional distributions including the gamma, Gompertz and log-Gumbel; by allowing the user to model average and by providing R scripts to replicate the analysis.

# Acknowledgements

We acknowledge contributions from Angeline Tillmanns, Marianne Métivier, Andy Teucher, David Fox, Carl Schwarz and Joe Thorley.
Development of `shinyssdtools` was initially funded by the Ministry of Environment and Climate Change Strategy, British Columbia. The governments of British Columbia, Canada and Australia have also contributed to its development.

## References
