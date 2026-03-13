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

# Testing Helper Functions for shinyssdtools
# ===========================================
# This file contains utility functions for snapshot testing and mock data creation

# CSV Snapshot Helpers --------------------------------------------------------

#' Save a data frame or tibble to a temporary CSV file
#'
#' Rounds numeric columns to 8 decimal places to prevent minor floating-point
#' differences from causing snapshot test failures.
#'
#' @param x A data frame or tibble
#' @return Character path to the temporary CSV file
save_csv <- function(x) {
  # Round numeric columns to prevent floating-point snapshot differences
  x <- dplyr::mutate(x, dplyr::across(where(is.numeric), ~ round(.x, 8)))

  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

#' Expect a data frame to match a CSV snapshot
#'
#' This function saves the data frame as CSV and compares it to a stored snapshot.
#' CSV format provides better diffs than JSON for tabular data.
#'
#' @param x A data frame or tibble to snapshot
#' @param name The name for the snapshot file (without extension)
#'
#' @examples
#' \dontrun{
#' gof_table <- predict(fit)
#' expect_snapshot_data(gof_table, "boron_gof_table")
#' }
expect_snapshot_data <- function(x, name) {
  testthat::skip_on_os("windows") # Line ending differences
  path <- save_csv(x)
  testthat::expect_snapshot_file(path, paste0(name, ".csv"))
}

#' Expect a fitdists object to match snapshot
#'
#' Specialized helper for ssdtools fitdists objects that extracts key components
#' for comparison while ignoring unstable elements.
#'
#' @param fit A fitdists object from ssd_fit_bcanz()
#' @param name The name for the snapshot
#'
#' @examples
#' \dontrun{
#' fit <- ssd_fit_bcanz(boron.data, dists = c("gamma", "lnorm"))
#' expect_snapshot_fit(fit, "boron_fit_gamma_lnorm")
#' }
expect_snapshot_fit <- function(fit, name) {
  # Extract stable components for comparison
  fit_summary <- list(
    distributions = names(fit),
    n_dists = length(fit),
    aic = ssdtools::ssd_gof(fit)$aicc,
    estimates = lapply(fit, function(x) x$estimate)
  )

  testthat::expect_snapshot_value(fit_summary, style = "json2", name = name)
}

# Shared Test Fixtures --------------------------------------------------------

#' Standard English translations for testServer tests
#'
#' Use this instead of duplicating translation setup in each test file.
test_translations <- shinyssdtools:::translations
test_translations$trans <- test_translations[["english"]]

# Mock Data Helpers -----------------------------------------------------------

#' Create a mock data module return value for testing
#'
#' @param data A data frame (default: boron.data)
#' @param toxicant_name Character string for toxicant name (default: "")
#' @return A list mimicking data module outputs
mock_data_module <- function(data = NULL, toxicant_name = "") {
  if (is.null(data)) {
    data <- data.frame(Conc = c(1, 2, 3), Group = c("A", "B", "C"))
  }
  list(
    data = shiny::reactive(data),
    clean_data = shiny::reactive(data),
    data_cols = shiny::reactive(names(data)),
    has_data = shiny::reactive(!is.null(data)),
    toxicant_name = shiny::reactive(toxicant_name)
  )
}

#' Create a mock fit module return value for testing
#'
#' @param fit A fitdists object (if NULL, creates a default boron fit)
#' @param title Character string for plot title (default: "")
#' @param conc_column Character string for concentration column name (default: "Conc")
#' @param units Character string for units (default: "")
#' @return A list mimicking fit module outputs
mock_fit_module <- function(
  fit = NULL,
  title = "",
  conc_column = "Conc",
  units = ""
) {
  if (is.null(fit)) {
    # Create simple mock data for fitting
    mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
    fit <- ssdtools::ssd_fit_bcanz(
      mock_data,
      dists = c("gamma", "lnorm")
    )
  }

  list(
    fit_dist = shiny::reactive(fit),
    has_fit = shiny::reactive(!is.null(fit)),
    fit_plot = shiny::reactive(NULL),
    gof_table = shiny::reactive(ssdtools::ssd_gof(fit)),
    conc_column = shiny::reactive(conc_column),
    units = shiny::reactive(units),
    title = shiny::reactive(title)
  )
}

# Validation Helpers ----------------------------------------------------------

#' Check if a ggplot object has a title
#'
#' @param plot A ggplot2 object
#' @return Logical indicating whether plot has a title
has_plot_title <- function(plot) {
  !is.null(plot$labels$title)
}

#' Extract title from a ggplot object
#'
#' @param plot A ggplot2 object
#' @return Character string of the title, or NULL if no title
get_plot_title <- function(plot) {
  plot$labels$title
}

#' Check if a ggplot has confidence intervals (ribbon or lines)
#'
#' @param plot A ggplot2 object
#' @return Logical indicating whether plot has CI layer
has_confidence_intervals <- function(plot) {
  layers <- sapply(plot$layers, function(x) class(x$geom)[1])
  any(layers %in% c("GeomXribbon"))
}

# Seed Management Helpers -----------------------------------------------------

#' Set seed for reproducible bootstrap operations
#'
#' Use this in testServer contexts where bootstrap sampling occurs
#'
#' @param seed Integer seed value (default: 12345)
#'
#' @examples
#' \dontrun{
#' test_that("prediction with CI is reproducible", {
#'   set_test_seed()
#'   # ... test code with bootstrap sampling
#' })
#' }
set_test_seed <- function(seed = 12345) {
  withr::local_seed(seed)
}

# AppDriver Helpers -----------------------------------------------------------

#' Wait for app to process data loading
#'
#' @param app An AppDriver instance
#' @param timeout Timeout in milliseconds (default: 5000)
wait_for_data <- function(app, timeout = 5000) {
  app$wait_for_idle(timeout = timeout)
  has_data <- app$get_value(output = "data_mod-has_data")
  if (!has_data) {
    stop("Data did not load within timeout period")
  }
  invisible(app)
}

#' Wait for app to complete model fitting
#'
#' @param app An AppDriver instance
#' @param timeout Timeout in milliseconds (default: 10000)
wait_for_fit <- function(app, timeout = 10000) {
  app$wait_for_idle(timeout = timeout)
  has_fit <- app$get_value(output = "fit_mod-has_fit")
  if (!has_fit) {
    stop("Fit did not complete within timeout period")
  }
  invisible(app)
}

#' Wait for app to generate predictions
#'
#' @param app An AppDriver instance
#' @param timeout Timeout in milliseconds (default: 10000)
wait_for_predict <- function(app, timeout = 10000) {
  app$wait_for_idle(timeout = timeout)
  has_predict <- app$get_value(output = "predict_mod-has_predict")
  if (!has_predict) {
    stop("Prediction did not complete within timeout period")
  }
  invisible(app)
}

#' Set bootstrap samples in selectizeInput using JavaScript
#'
#' The bootSamp selectizeInput has create=TRUE but set_inputs() doesn't handle
#' custom values properly. This helper uses JavaScript to add and select a custom value.
#'
#' @param app An AppDriver instance
#' @param value Character string or numeric value for bootstrap samples
#' @param module_id Module namespace prefix (default: "predict_mod")
#'
#' @examples
#' \dontrun{
#' app <- AppDriver$new()
#' set_bootstrap_samples(app, "5")
#' app$wait_for_idle()
#' }
set_bootstrap_samples <- function(app, value, module_id = "predict_mod") {
  value <- as.character(value)
  input_id <- paste0(module_id, "-bootSamp")

  js_code <- sprintf(
    "
    $('#%s')[0].selectize.clear();
    $('#%s')[0].selectize.addOption({value: '%s', text: '%s'});
    $('#%s')[0].selectize.setValue('%s');
    $('#%s')[0].selectize.trigger('change');
  ",
    input_id,
    input_id,
    value,
    value,
    input_id,
    value,
    input_id
  )

  app$run_js(js_code)
  invisible(app)
}

#' Create AppDriver for workflow tests with standard settings
#'
#' @param name Character name for the test (used in snapshots)
#' @return An AppDriver instance with deferred cleanup
#'
#' @examples
#' \dontrun{
#' test_that("workflow test", {
#'   app <- create_workflow_app("test-name")
#'   # Test code here
#'   # app$stop() called automatically via defer
#' })
#' }
create_workflow_app <- function(name) {
  app <- shinytest2::AppDriver$new(
    app_dir = system.file("app", package = "shinyssdtools"),
    variant = shinytest2::platform_variant(),
    name = name,
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 60000
  )
  app
}

