
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyssdtools

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02848/status.svg)](https://doi.org/10.21105/joss.02848)
[![R-CMD-check](https://github.com/bcgov/shinyssdtools/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/shinyssdtools/actions)
<!-- badges: end -->

Shiny web application for fitting Species Sensitivity Distributions
(SSDs).

### Summary

`shinyssdtools` provides a Graphical User Interface (GUI) to the
[ssdtools](https://cran.r-project.org/package=ssdtools) R package. The
app can be run locally by cloning the repository and running
`shiny::shinyAppDir(".")` or by visiting [this
site](https://bcgov-env.shinyapps.io/ssdtools/).

In addition to providing access to core functionality in the
[ssdtools](https://cran.r-project.org/package=ssdtools) R package,
`shinyssdtools` provides:

1.  translation to French
2.  generation of R code required to reproduce results
3.  further customization of plot and table outputs

## Utilization

Once cloned run

``` r
# install.packages("shiny")
shiny::shinyAppDir(".")
```

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/ssdtools-shiny/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.
