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

# 1. Install all dependencies from CRAN (not r-universe) to avoid dev versions
#    Local dev environment may have r-universe packages with ephemeral dev
#    versions that shinyapps.io can't install.
options(repos = c(CRAN = "https://cran.r-project.org"))
desc <- read.dcf("DESCRIPTION", fields = c("Depends", "Imports"))
pkgs <- trimws(unlist(strsplit(
  paste(na.omit(as.character(desc)), collapse = ", "),
  ","
)))
pkgs <- gsub("\\s*\\(.*\\)", "", pkgs)
base_pkgs <- rownames(installed.packages(priority = c("base", "recommended")))
pkgs <- pkgs[pkgs != "R" & nchar(pkgs) > 0 & !pkgs %in% base_pkgs]
renv::rebuild(packages = pkgs)
renv::snapshot()

# 2. Build user guide/about HTML for all languages
rmarkdown::render("inst/extdata/user-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/user-fr.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/user-es.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-fr.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-es.md", output_format = "html_fragment")

# 3. Build helpfiles from user guides (single source of truth)
source("scripts/build-helpfiles.R")

# 4. Deploy to poissonconsulting test site
rsconnect::deployApp(
  account = "poissonconsulting",
  appName = "shinyssdtools-dev",
  forceUpdate = TRUE
)

# 5. Deploy to poissonconsulting live site
rsconnect::deployApp(
  account = "poissonconsulting",
  appName = "shinyssdtools",
  forceUpdate = TRUE
)

# 6. Deploy to bcgov test site (requires bcgov-env account access)
# rsconnect::deployApp(
#   account = "bcgov-env",
#   appName = "shinyssdtools_test",
#   forceUpdate = TRUE
# )

# 7. Deploy to bcgov live site (requires bcgov-env account access)
# rsconnect::deployApp(
#   account = "bcgov-env",
#   appName = "ssdtools",
#   forceUpdate = TRUE
# )
