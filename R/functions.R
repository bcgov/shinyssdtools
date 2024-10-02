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

utils::globalVariables(c("."))

# functions
label_mandatory <- function(label) {
  tagList(label, span("*", class = "mandatory_star"))
}

inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}

hint <- function(x)
  HTML(paste0("<font color='grey'>", x, "</font>"))

zero_range <- function(x, tol = .Machine$double.eps^0.5) {
  if (length(x) == 1) {
    return(TRUE)
  }
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

estimate_hc <- function(x, percent) {
  ssdtools::ssd_hc(x, proportion = percent / 100)$est
}

estimate_hp <- function(x, conc) {
  ssdtools::ssd_hp(x, conc = conc)$est
}

ssd_hc_ave <- function(x, percent, nboot) {
  dist <- ssdtools::ssd_hc(
    x,
    proportion = percent / 100,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hc(
      x,
      proportion = percent / 100,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}

ssd_hp_ave <- function(x, conc, nboot) {
  dist <- ssdtools::ssd_hp(
    x,
    conc = conc,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hp(
      x,
      conc = conc,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}
