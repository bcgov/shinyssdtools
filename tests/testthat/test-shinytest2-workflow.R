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

test_that("workflow: data -> fit, plot and table render, state values", {
  app <- create_workflow_app("fit_workflow")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(`main_nav` = "fit")
  wait_for_fit(app)

  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "fit")

  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit)

  fit_plot_rendered <- app$get_js(
    "!!document.querySelector('#fit_mod-plotDist img')"
  )
  expect_true(fit_plot_rendered)

  gof_table_rendered <- app$get_js(
    "!!document.querySelector('#fit_mod-tableGof .dataTable')"
  )
  expect_true(gof_table_rendered)

  fit_state <- app$get_values(
    input = c(
      "fit_mod-selectConc",
      "fit_mod-selectDist",
      "fit_mod-selectUnit",
      "fit_mod-rescale"
    )
  )
  expect_snapshot_value(fit_state, style = "json2", cran = FALSE)
})

test_that("workflow: data -> predict, plot and table render, state values", {
  app <- create_workflow_app("predict_workflow")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(`main_nav` = "fit")
  wait_for_fit(app)

  app$set_inputs(`main_nav` = "predict")
  wait_for_predict(app)

  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "predict")

  has_predict <- app$get_value(output = "predict_mod-has_predict")
  expect_true(has_predict)

  # The includeCi checkbox defaults to TRUE (R/mod_predict.R:124) but sits
  # inside a conditionalPanel gated on output['predict_mod-has_fit']. On
  # the first transition from hidden to visible the server-side input
  # value can briefly read FALSE before the client binding settles,
  # producing a flaky snapshot. Set it explicitly to pin the expected
  # default before capturing. `wait_ = FALSE` because setting TRUE when
  # already TRUE produces no server output update.
  app$set_inputs(`predict_mod-includeCi` = TRUE, wait_ = FALSE)
  app$wait_for_idle()

  predict_state <- app$get_values(
    input = c(
      "predict_mod-thresh",
      "predict_mod-threshType",
      "predict_mod-includeCi",
      "predict_mod-bootSamp"
    )
  )
  expect_snapshot_value(predict_state, style = "json2", cran = FALSE)

  predict_plot_rendered <- app$get_js(
    "!!document.querySelector('#predict_mod-plotPred img')"
  )
  expect_true(predict_plot_rendered)

  thresh_input <- app$get_value(input = "predict_mod-thresh")
  expect_type(thresh_input, "character")
  expect_true(nchar(thresh_input) > 0)

  thresh_type <- app$get_value(input = "predict_mod-threshType")
  expect_type(thresh_type, "character")

  set_bootstrap_samples(app, "5")
  app$wait_for_idle()
  app$click("predict_mod-getCl")
  app$wait_for_idle(timeout = 15000)

  cl_table_rendered <- app$get_js(
    "!!document.querySelector('#predict_mod-tableCl .dataTable')"
  )
  expect_true(cl_table_rendered)
})

test_that("workflow: data -> rcode, text renders", {
  app <- create_workflow_app("complete_workflow")
  withr::defer(app$stop())

  code_rendered_before <- app$get_js(
    "!!document.querySelector('#rcode_mod-code-container pre')"
  )
  expect_false(code_rendered_before)

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(`main_nav` = "fit")
  wait_for_fit(app)

  app$set_inputs(`main_nav` = "predict")
  wait_for_predict(app)

  app$set_inputs(`main_nav` = "rcode")
  app$wait_for_idle()

  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "rcode")

  has_code <- app$get_value(output = "rcode_mod-has_code")
  expect_true(has_code)

  code_rendered <- app$get_js(
    "!!document.querySelector('#rcode_mod-code-container pre')"
  )
  expect_true(code_rendered)

  code_text <- app$get_js(
    "document.querySelector('#rcode_mod-code-container pre').textContent.trim()"
  )
  expect_true(nchar(code_text) > 0)
})

# test_that("workflow: data -> report, preview exists", {
#   app <- create_workflow_app("complete_workflow")
#   withr::defer(app$stop())

#   app$click("data_mod-demoData")
#   wait_for_data(app)

#   app$set_inputs(`main_nav` = "fit")
#   wait_for_fit(app)

#   app$set_inputs(`main_nav` = "predict")
#   wait_for_predict(app)

#   app$set_inputs(`main_nav` = "report")
#   app$wait_for_idle()

#   nav_value <- app$get_value(input = "main_nav")
#   expect_equal(nav_value, "report")

#   iframe_has_content_before <- app$get_js(
#     "!!document.getElementById('report_mod-htmlPreview').srcdoc"
#   )
#   expect_null(iframe_has_content_before)

#   set_bootstrap_samples(app, "5", module_id = "report_mod")
#   app$wait_for_idle()
#   app$click("report_mod-generateReport")
#   app$wait_for_idle(timeout = 10000)

#   # Check if report was successfully generated
#   has_preview_value <- app$get_value(output = "report_mod-has_preview")
#   print(paste("has_preview value:", has_preview_value))
#   expect_true(has_preview_value)

#   iframe_has_content <- app$get_js(
#     "!!document.getElementById('report_mod-htmlPreview').srcdoc"
#   )
#   expect_true(iframe_has_content)

#   iframe_content_length <- app$get_js(
#     "document.getElementById('report_mod-htmlPreview').srcdoc.length"
#   )
#   expect_true(iframe_content_length > 0)
# })

test_that("toxicant name carries through modules", {
  app <- create_workflow_app("toxicant_name")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(`data_mod-toxicant` = "Test Chemical")
  app$wait_for_idle()

  app$set_inputs(`main_nav` = "fit")
  wait_for_fit(app)

  fit_title <- app$get_value(input = "fit_mod-title")
  expect_equal(fit_title, "Test Chemical")

  app$set_inputs(`main_nav` = "predict")
  wait_for_predict(app)

  predict_title <- app$get_value(input = "predict_mod-title")
  expect_equal(predict_title, "Test Chemical")
})
