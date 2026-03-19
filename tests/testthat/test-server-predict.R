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

test_fit <- ssdtools::ssd_fit_bcanz(test_data, dists = c("lnorm", "gamma"))
fit_mod <- mock_fit_module(fit = test_fit, conc_column = "Conc", units = "")

predict_args <- list(
  translations = reactive(test_translations),
  lang = reactive("english"),
  data_mod = data_mod,
  fit_mod = fit_mod,
  big_mark = reactive(","),
  decimal_mark = reactive("."),
  main_nav = reactive("predict")
)

test_that("predictions are valid", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned

      pred <- returned$predictions()
      expect_true(is.data.frame(pred))
      expect_true(nrow(pred) > 0)
      expect_true("est" %in% names(pred))
      expect_true("dist" %in% names(pred))

      expect_true(is.numeric(pred$est))
      expect_true(all(!is.na(pred$est)))

      expect_true(all(is.na(pred$lcl)))
      expect_snapshot_data(pred, "predict-predictions-no-cl")
    }
  )
})

test_that("has_predict is TRUE after generating predictions", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        bootSamp = "100"
      )
      session$flushReact()

      returned <- session$returned
      expect_true(returned$has_predict())
    }
  )
})

# Threshold Tests -------------------------------------------------------------

test_that("predictions change with % affected", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned
      pred_vals <- returned$threshold_values()
      pred_perc <- pred_vals$percent
      pred_conc <- pred_vals$conc

      session$setInputs(thresh = "10")
      session$flushReact()

      pred_vals_10 <- returned$threshold_values()
      pred_perc_10 <- pred_vals_10$percent
      pred_conc_10 <- pred_vals_10$conc

      expect_identical(pred_perc_10, 10)
      expect_false(identical(pred_perc, pred_perc_10))
      expect_false(identical(pred_conc, pred_conc_10))
    }
  )
})

test_that("predictions change when switch thresh type", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned
      pred_vals <- returned$threshold_values()
      pred_perc <- pred_vals$percent
      pred_conc <- pred_vals$conc

      session$setInputs(
        threshType = "Fraction",
        conc = 1.0
      )
      session$flushReact()

      pred_vals_frac <- returned$threshold_values()
      pred_perc_frac <- pred_vals_frac$percent
      pred_conc_frac <- pred_vals_frac$conc

      expect_identical(pred_conc_frac, 1.0)
      expect_false(identical(pred_perc, pred_perc_frac))
      expect_false(identical(pred_conc, pred_conc_frac))
    }
  )
})

# Plot Tests ------------------------------------------------------------------

test_that("model ave plot is valid", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "5",
          includeCi = FALSE,
          title = "",
          xaxis = "Concentration",
          yaxis = "Species affected (%)",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "TRUE"
        )
      })
      session$flushReact()

      returned <- session$returned
      plot <- returned$model_average_plot()

      vdiffr::expect_doppelganger("predict-plot-default", plot)
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

test_that("xbreaks works when no column matches guess_sp (#103)", {
  # Dataset with Value/Taxon columns - no "sp" match so selectLabel defaults to "-none-"
  taxon_data <- data.frame(
    Value = c(0.002, 0.002, 0.0234, 0.051, 0.068, 0.107, 0.261, 0.334, 1.4, 5.18),
    Taxon = c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5", "Sp6", "Sp7", "Sp8", "Sp9", "Sp10")
  )
  taxon_data <- clean_ssd_data(taxon_data)
  taxon_fit <- ssdtools::ssd_fit_bcanz(taxon_data, left = "Value", dists = c("lnorm", "gamma"))
  taxon_args <- list(
    translations = reactive(test_translations),
    lang = reactive("english"),
    data_mod = mock_data_module(data = taxon_data),
    fit_mod = mock_fit_module(fit = taxon_fit, conc_column = "Value"),
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  )

  testServer(
    mod_predict_server,
    args = taxon_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "0.1",
          includeCi = FALSE,
          selectLabel = "-none-",
          selectColour = "-none-",
          selectShape = "-none-",
          xlog = TRUE
        )
      })
      session$flushReact()

      xbreaks <- plot_model_average_xbreaks()
      expect_true(is.null(xbreaks) || is.numeric(xbreaks))
    }
  )
})

test_that("model ave plot when change thresh", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "1",
          includeCi = FALSE,
          title = "",
          xaxis = "Concentration",
          yaxis = "Species affected (%)",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "TRUE"
        )
      })
      session$flushReact()

      returned <- session$returned
      plot <- returned$model_average_plot()

      vdiffr::expect_doppelganger("predict-plot-thresh-1", plot)
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

# Confidence Interval Tests ---------------------------------------------------

test_that("CL table is valid", {
  set_test_seed()
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "5"
      )
      session$flushReact()

      session$setInputs(getCl = 1)
      session$flushReact()

      returned <- session$returned

      expect_true(returned$has_cl())
      cl_table <- returned$predict_cl()
      expect_true(is.data.frame(cl_table))
      expect_true(nrow(cl_table) > 0)

      expect_true("lcl" %in% names(cl_table))
      expect_true("ucl" %in% names(cl_table))

      expect_snapshot_data(cl_table, "predict-cl-table")
    }
  )
})

test_that("predictions include lcl/ucl when Get CL clicked", {
  set_test_seed()
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        bootSamp = 5
      )
      session$flushReact()

      session$setInputs(getCl = 1, includeCi = TRUE)
      session$flushReact()

      returned <- session$returned

      pred <- returned$predictions()
      expect_true(is.data.frame(pred))
      expect_true(all(!is.na(pred$lcl)))
      expect_true(all(!is.na(pred$ucl)))

      expect_snapshot_data(pred, "predict-predictions-with-cl")
    }
  )
})

test_that("model ave plot includes CL", {
  set_test_seed()
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "5",
          includeCi = FALSE,
          bootSamp = "5",
          title = "",
          xaxis = "Concentration",
          yaxis = "Species affected (%)",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "TRUE"
        )

        session$flushReact()

        returned <- session$returned
        plot_before <- returned$model_average_plot()
        expect_false(has_confidence_intervals(plot_before))

        # Generate CL data
        session$setInputs(getCl = 1)
        session$flushReact()

        # CL generated but not included on plot yet
        plot_after_cl <- returned$model_average_plot()
        expect_false(has_confidence_intervals(plot_after_cl))

        # Toggle checkbox to include CL on plot
        session$setInputs(includeCi = TRUE)
        session$flushReact()
      })
      plot_with_ci <- returned$model_average_plot()

      expect_true(has_confidence_intervals(plot_with_ci))
      vdiffr::expect_doppelganger("predict-plot-with-ci", plot_with_ci)
    }
  )
})

test_that("model ave plot with line style CI", {
  set_test_seed()
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "5",
          includeCi = TRUE,
          bootSamp = "5",
          title = "",
          xaxis = "Concentration",
          yaxis = "Species affected (%)",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "FALSE"
        )
        session$flushReact()

        session$setInputs(getCl = 1)
        session$flushReact()
      })

      returned <- session$returned
      plot <- returned$model_average_plot()

      vdiffr::expect_doppelganger("predict-plot-with-ci-lines", plot)
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

test_that("CL table changes with bootstrap samples", {
  set_test_seed()
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "5",
        getCl = 1
      )
      session$flushReact()

      returned <- session$returned
      pred <- returned$predictions()
      expect_equal(returned$cl_nboot(), 5)
      expect_equal(unique(pred$nboot), 5)

      session$setInputs(
        bootSamp = "10",
        getCl = 2
      )
      session$flushReact()

      expect_equal(returned$cl_nboot(), 10)
      pred <- returned$predictions()
      expect_equal(unique(pred$nboot), 10)
    }
  )
})

# Number Formatting Tests -----------------------------------------------------

test_that("estimates use English number formatting", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      estConc <- output$estConc
      hcConc <- output$hcConc

      if (grepl("[.,]", estConc)) {
        expect_true(grepl("\\.", estConc) || !grepl(",", estConc))
      }
    }
  )
})

test_that("estimates use French number formatting", {
  test_trans_fr <- translations
  test_trans_fr$trans <- test_trans_fr[["french"]]

  predict_args_fr <- predict_args
  predict_args_fr$translations <- reactive(test_trans_fr)
  predict_args_fr$lang <- reactive("french")
  predict_args_fr$big_mark <- reactive(" ")
  predict_args_fr$decimal_mark <- reactive(",")

  testServer(
    mod_predict_server,
    args = predict_args_fr,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      estConc <- output$estConc
      hcConc <- output$hcConc

      if (grepl("[.,]", estConc)) {
        expect_true(grepl(",", estConc))
      }
    }
  )
})

# Module Return Values Tests --------------------------------------------------

test_that("server returns all expected reactive values", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        ribbonStyle = "TRUE",
        title = "Test"
      )
      session$flushReact()

      returned <- session$returned

      expect_true(is.reactive(returned$predictions))
      expect_true(is.reactive(returned$threshold_values))
      expect_true(is.reactive(returned$threshold_type))
      expect_true(is.reactive(returned$include_ci))
      expect_true(is.reactive(returned$ribbon))
      expect_true(is.reactive(returned$title))
      expect_true(is.reactive(returned$has_predict))
      expect_true(is.reactive(returned$cl_requested))
      expect_true(is.reactive(returned$cl_nboot))

      expect_equal(returned$threshold_type(), "Concentration")
      expect_false(returned$include_ci())
      expect_true(returned$ribbon())
      expect_equal(returned$title(), "Test")
    }
  )
})

test_that("cl_requested is FALSE without clicking Get CL button", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "100"
      )
      session$flushReact()

      returned <- session$returned
      expect_false(returned$cl_requested())
    }
  )
})

test_that("cl_nboot stores bootstrap count when Get CL clicked", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "100"
      )
      session$flushReact()

      session$setInputs(getCl = 1)
      session$flushReact()

      returned <- session$returned
      expect_true(returned$cl_requested())
      expect_equal(returned$cl_nboot(), 100)
    }
  )
})

test_that("ribbon reactive returns correct boolean value", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE
      )
      returned <- session$returned

      session$setInputs(ribbonStyle = "TRUE")
      expect_true(returned$ribbon())

      session$setInputs(ribbonStyle = "FALSE")
      expect_false(returned$ribbon())
    }
  )
})


test_that("threshold_type reactive returns selected type", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      returned <- session$returned

      session$setInputs(threshType = "Concentration")
      expect_equal(returned$threshold_type(), "Concentration")

      session$setInputs(threshType = "Fraction")
      expect_equal(returned$threshold_type(), "Fraction")
    }
  )
})

test_that("threshold_values returns percent and conc", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      session$flushReact()

      returned <- session$returned
      thresholds <- returned$threshold_values()
      expect_type(thresholds, "list")
      expect_true("percent" %in% names(thresholds))
      expect_true("conc" %in% names(thresholds))
    }
  )
})

test_that("include_ci reflects checkbox state", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      returned <- session$returned

      # Initially unchecked
      session$setInputs(includeCi = FALSE)
      expect_false(returned$include_ci())

      # Check the box
      session$setInputs(includeCi = TRUE)
      expect_true(returned$include_ci())
    }
  )
})

test_that("title reactive returns input value", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      returned <- session$returned

      session$setInputs(title = "Copper Toxicity")
      expect_equal(returned$title(), "Copper Toxicity")

      session$setInputs(title = "")
      expect_equal(returned$title(), "")
    }
  )
})
