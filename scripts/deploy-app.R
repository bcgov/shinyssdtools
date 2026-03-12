# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

# install ssdtools
# install.packages("ssdtools")
# get cran versions of poisson pkgs
pak::pak("universals")
pak::pak("chk")
remotes::install_github("poissonconsulting/ssdtools")
renv::snapshot()

# build user guide/about for all languages
rmarkdown::render("inst/extdata/user-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/user-fr.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/user-es.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-fr.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-es.md", output_format = "html_fragment")

# build helpfiles from user guides (single source of truth)
source("scripts/build-helpfiles.R")

# deploy to poissonconsulting test site
rsconnect::deployApp(
  account = "poissonconsulting",
  appName = "shinyssdtools-dev",
  forceUpdate = TRUE
)

# deploy to poissonconsulting live site
rsconnect::deployApp(
  account = "poissonconsulting",
  appName = "shinyssdtools",
  forceUpdate = TRUE
)

# deploy to bcgov test site
rsconnect::deployApp(
  account = "bcgov-env",
  appName = "shinyssdtools_test",
  forceUpdate = TRUE
)

# deploy to bcgov live site
rsconnect::deployApp(
  account = "bcgov-env",
  appName = "ssdtools",
  forceUpdate = TRUE
)
