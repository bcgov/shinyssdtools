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

test_that("app loads successfully", {
  app <- create_workflow_app("app_loads")
  withr::defer(app$stop())

  expect_true(app$get_js("!!document.querySelector('.navbar')"))
  expect_true(app$get_js("!!document.getElementById('main_nav')"))
})

test_that("demo data loads and displays", {
  app <- create_workflow_app("demo_data_workflow")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  table_rendered <- app$get_js(
    "!!document.querySelector('#data_mod-viewUpload .dataTable')"
  )
  expect_true(table_rendered)
})

test_that("language switch", {
  app <- create_workflow_app("language-switching")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(main_nav = "fit")
  wait_for_fit(app)

  has_fit_before <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit_before)

  app$click("french")
  app$wait_for_idle()

  has_fit_french <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit_french)
})

test_that("downloads work", {
  app <- create_workflow_app("download-tests")
  withr::defer(app$stop())

  app$click("data_mod-demoData")
  wait_for_data(app)

  app$set_inputs(main_nav = "fit")
  wait_for_fit(app)

  # fit downloads
  app$click("fit_mod-fitDownloadBtn")
  app$wait_for_idle(duration = 500)
  fit_plot_path <- app$get_download("fit_mod-fitDlPlot")
  expect_true(file.exists(fit_plot_path))
  expect_match(fit_plot_path, "\\.png$")
  # plot should be at least 5kb
  expect_gt(file.info(fit_plot_path)$size, 5000)

  app$click("fit_mod-fitDownloadBtnTbl")
  app$wait_for_idle(duration = 500)
  gof_table_path <- app$get_download("fit_mod-fitDlCsv")
  expect_true(file.exists(gof_table_path))
  expect_match(gof_table_path, "\\.csv$")
  expect_gt(file.info(gof_table_path)$size, 100)
  # test has some data, not empty
  gof_data <- readr::read_csv(gof_table_path, show_col_types = FALSE)
  expect_gt(nrow(gof_data), 0)

  app$set_inputs(main_nav = "predict")
  wait_for_predict(app)

  app$click("predict_mod-predDownloadBtn")
  app$wait_for_idle(duration = 500)
  predict_plot_path <- app$get_download("predict_mod-predDlPlot")
  expect_true(file.exists(predict_plot_path))
  expect_match(predict_plot_path, "\\.png$")
  expect_gt(file.info(predict_plot_path)$size, 5000)

  set_bootstrap_samples(app, "5")
  app$wait_for_idle(duration = 500)
  app$click("predict_mod-getCl")
  app$wait_for_idle(duration = 500, timeout = 15000)

  app$click("predict_mod-predDownloadBtnTbl")
  app$wait_for_idle(duration = 500)
  cl_table_path <- app$get_download("predict_mod-predDlCsv")
  expect_true(file.exists(cl_table_path))
  expect_match(cl_table_path, "\\.csv$")
  expect_gt(file.info(cl_table_path)$size, 100)
  cl_data <- readr::read_csv(cl_table_path, show_col_types = FALSE)
  expect_gt(nrow(cl_data), 0)

  # app$set_inputs(main_nav = "report")
  # app$wait_for_idle()

  # set_bootstrap_samples(app, "5", module_id = "report_mod")
  # app$wait_for_idle()
  # app$click("report_mod-generateReport")
  # app$wait_for_idle(timeout = 10000)

  # app$click("report_mod-reportDownloadBtnReport")
  # app$wait_for_idle()
  # report_pdf_path <- app$get_download("report_mod-reportDlPdf")
  # expect_true(file.exists(report_pdf_path))
  # # test has some content
  # expect_match(report_pdf_path, "\\.pdf$")
  # expect_gt(file.info(report_pdf_path)$size, 1000)
  # pdf_header <- readBin(report_pdf_path, "raw", n = 4)
  # expect_equal(rawToChar(pdf_header), "%PDF")
})
