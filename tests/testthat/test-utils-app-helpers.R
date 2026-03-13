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

# Essential Unit Tests for Utility Functions
# ============================================

# clean_nboot() ---------------------------------------------------------------

test_that("clean_nboot removes separators from numbers", {
  expect_equal(clean_nboot("1,000"), 1000)
  expect_equal(clean_nboot("10 000"), 10000)
  expect_equal(clean_nboot("500"), 500)
})

# append_unit() ---------------------------------------------------------------

test_that("append_unit handles empty unit and normal unit", {
  expect_equal(append_unit("Concentration", ""), "Concentration")
  expect_equal(append_unit("Concentration", "mg/L"), "Concentration (mg/L)")
})

# guess_conc() ----------------------------------------------------------------

test_that("guess_conc finds conc column or falls back to numeric", {
  expect_equal(guess_conc(c("Species", "Conc", "Group")), "Conc")
  expect_equal(guess_conc(c("Species", "concentration", "Group")), "concentration")

  data <- data.frame(Species = c("A", "B", "C"), Value = c(1, 2, 3))
  expect_equal(guess_conc(names(data), data), "Value")

  expect_true(is.na(guess_conc(c("Species", "Group"))))
})

# guess_sp() ------------------------------------------------------------------

test_that("guess_sp finds species column or returns NA", {
  expect_equal(guess_sp(c("Species", "Conc", "Group")), "Species")
  expect_equal(guess_sp(c("sp", "Conc")), "sp")
  expect_true(is.na(guess_sp(c("Organism", "Conc"))))
})

# zero_range() ----------------------------------------------------------------

test_that("zero_range identifies identical vs different values", {
  expect_true(zero_range(c(5, 5, 5, 5)))
  expect_false(zero_range(c(1, 2, 3)))
})

# estimate_time() -------------------------------------------------------------

test_that("estimate_time returns preset values for English and French", {
  expect_equal(estimate_time(500, "english"), "10 seconds")
  expect_equal(estimate_time(1000, "english"), "20 seconds")
  expect_equal(estimate_time(5000, "english"), "2 minutes")

  expect_equal(estimate_time(500, "french"), "10 secondes")
  expect_equal(estimate_time(1000, "french"), "20 secondes")
  expect_equal(estimate_time(5000, "french"), "2 minutes")
})

# tr() ------------------------------------------------------------------------

test_that("tr extracts translation by id", {
  trans <- data.frame(
    id = c("ui_test1", "ui_test2"),
    trans = c("Test 1", "Test 2"),
    stringsAsFactors = FALSE
  )

  expect_equal(tr("ui_test1", trans), "Test 1")
  expect_equal(tr("ui_test2", trans), "Test 2")
})

# safe_try() ------------------------------------------------------------------

test_that("safe_try returns result on success or NULL on error", {
  expect_equal(safe_try(1 + 1), 2)
  expect_null(safe_try(stop("error")))
})

# Threshold Calculations ------------------------------------------------------

test_that("calculate_threshold_percent and calculate_threshold_conc work correctly", {
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_bcanz(test_data, dists = "lnorm", silent = TRUE)

  # Test basic percent calculation
  result_percent <- calculate_threshold_percent(fit, 10)
  expect_type(result_percent, "double")
  expect_true(result_percent >= 0 && result_percent <= 100)

  # Test basic conc calculation
  result_conc <- calculate_threshold_conc(fit, 5)
  expect_type(result_conc, "double")
  expect_true(result_conc > 0)

  # Test that they are inverse operations
  thresh_percent <- 5
  conc <- calculate_threshold_conc(fit, thresh_percent)
  percent_back <- calculate_threshold_percent(fit, conc)
  expect_equal(percent_back, thresh_percent, tolerance = 0.1)
})

# format_r_code() -------------------------------------------------------------

test_that("format_r_code converts double quotes to single quotes", {
  code_lines <- c('x <- "hello"', 'y <- "world"')
  result <- format_r_code(code_lines)

  expect_match(result, "'hello'")
  expect_match(result, "'world'")
  expect_false(grepl('"hello"', result))
})
