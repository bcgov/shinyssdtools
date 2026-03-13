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

library(shinytest2)

test_that("data upload: test tox1", {
  app <- create_workflow_app("csv-upload-custom")
  withr::defer(app$stop())

  app$upload_file(`data_mod-uploadData` = "test-files/test_tox1.csv")
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  app$set_inputs(main_nav = "fit")
  wait_for_fit(app)

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit)

  conc_input <- app$get_value(input = "fit_mod-selectConc")
  expect_equal(conc_input, "Value")
})

test_that("data upload: test tox2", {
  app <- create_workflow_app("csv-upload-grouped")
  withr::defer(app$stop())

  app$upload_file(`data_mod-uploadData` = "test-files/test_tox2.csv")
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  app$set_inputs(main_nav = "fit")
  wait_for_fit(app)

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit)

  gof_table_rendered <- app$get_js(
    "!!document.querySelector('#fit_mod-tableGof .dataTable')"
  )
  expect_true(gof_table_rendered)
})

test_that("data upload: test tox3", {
  app <- create_workflow_app("csv-upload-grouped")
  withr::defer(app$stop())

  app$upload_file(`data_mod-uploadData` = "test-files/test_tox3.csv")
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  app$set_inputs(main_nav = "fit")
  wait_for_fit(app)

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit)

  gof_table_rendered <- app$get_js(
    "!!document.querySelector('#fit_mod-tableGof .dataTable')"
  )
  expect_true(gof_table_rendered)
})

test_that("data upload: insufficient conc values", {
  app <- create_workflow_app("csv-upload-insufficient")
  withr::defer(app$stop())

  app$upload_file(
    `data_mod-uploadData` = "test-files/test_insufficient_data.csv"
  )
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  # shinyvalidate error list not FALSE, but TRUE if has fit
  expect_false(isTRUE(has_fit))
})

test_that("data upload: missing conc values", {
  app <- create_workflow_app("csv-upload-insufficient")
  withr::defer(app$stop())

  app$upload_file(
    `data_mod-uploadData` = "test-files/test_missing_conc.csv"
  )
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_false(isTRUE(has_fit))
})
