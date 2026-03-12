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

test_data <- clean_ssd_data(boron.data)
data_mod <- mock_data_module(data = test_data)

fit_args <- list(
  translations = reactive(test_translations),
  lang = reactive("english"),
  data_mod = data_mod,
  big_mark = reactive(","),
  decimal_mark = reactive("."),
  main_nav = reactive("fit")
)

test_that("fit obect is valid", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      fit <- returned$fit_dist()
      expect_s3_class(fit, "fitdists")
      expect_equal(length(fit), 2)
      expect_equal(names(fit), c("lnorm", "gamma"))
    }
  )
})

test_that("has_fit is TRUE after successful fit", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      expect_true(returned$has_fit())
    }
  )
})

# Goodness-of-Fit Table Tests -------------------------------------------------

test_that("gof table is valid", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      gof <- returned$gof_table()
      expect_true(is.data.frame(gof))
      expect_equal(nrow(gof), 2)
      expect_snapshot_data(gof, "gof-table")

      expect_true("aicc" %in% names(gof))
      expect_true("bic" %in% names(gof))
    }
  )
})

test_that("fit plot is valid", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        selectUnit = "mg/L",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = ""
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      vdiffr::expect_doppelganger("fit-plot-default", plot)
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

test_that("ffit plot with different distributions", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma", "lgumbel", "weibull"),
        selectUnit = "",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = ""
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      vdiffr::expect_doppelganger("fit-plot-multiple-dists", plot)
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

test_that("fit plot includes custom title", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        selectUnit = "mg/L",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = "Boron Toxicity"
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      expect_true(has_plot_title(plot))
      expect_equal(get_plot_title(plot), "Boron Toxicity")
    }
  )
})

test_that("fit respects selected distributions", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      fit <- returned$fit_dist()
      expect_equal(length(fit), 1)
      expect_equal(names(fit), "gamma")
    }
  )
})

test_that("server reutrns all expected values", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1,
        selectUnit = "mg/L",
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = "Test"
      )
      session$flushReact()

      returned <- session$returned

      expect_true(is.reactive(returned$fit_dist))
      expect_true(is.reactive(returned$fit_plot))
      expect_true(is.reactive(returned$gof_table))
      expect_true(is.reactive(returned$has_fit))
      expect_true(is.reactive(returned$conc_column))
      expect_true(is.reactive(returned$units))
      expect_true(is.reactive(returned$dists))
      expect_true(is.reactive(returned$rescale))
      expect_true(is.reactive(returned$title))

      expect_equal(returned$conc_column(), "Conc")
      expect_equal(returned$units(), "mg/L")
      expect_equal(returned$dists(), c("lnorm", "gamma"))
      expect_false(returned$rescale())
      expect_equal(returned$title(), "Test")
    }
  )
})

# rescale affects parameter estimates (tidy()) output not ssd_gof or hc estimates
test_that("param ests change when rescale toggled", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      fit_no_rescale <- returned$fit_dist()
      params_no_rescale <- ssdtools::tidy(fit_no_rescale)$est

      session$setInputs(
        rescale = TRUE,
        updateFit = 2
      )
      session$flushReact()

      fit_rescaled <- returned$fit_dist()
      params_rescaled <- ssdtools::tidy(fit_rescaled)$est

      expect_false(identical(params_no_rescale, params_rescaled))
    }
  )
})

test_that("validation fails if wrong conc col", {
  testServer(
    mod_fit_server,
    args = fit_args,
    {
      session$setInputs(
        selectConc = "Species",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1,
        selectUnit = "mg/L",
      )
      session$flushReact()

      expect_false(iv$is_valid())
    }
  )
})
