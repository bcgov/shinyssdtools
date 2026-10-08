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

test_that("has_numeric_concentration returns TRUE for numeric data", {
  expect_true(has_numeric_concentration(c(1, 2, 3, 4, 5, 6, 7, 8)))
  expect_true(has_numeric_concentration(1:10))
})

test_that("has_numeric_concentration returns FALSE for non-numeric data", {
  expect_false(has_numeric_concentration(c("a", "b", "c")))
  expect_false(has_numeric_concentration(as.factor(1:8)))
})

test_that("has_no_missing_concentration returns TRUE when no NA values", {
  expect_true(has_no_missing_concentration(c(1, 2, 3, 4, 5, 6, 7, 8)))
  expect_true(has_no_missing_concentration(1:10))
})

test_that("has_no_missing_concentration returns FALSE when NA values present", {
  expect_false(has_no_missing_concentration(c(1, 2, NA, 4, 5, 6, 7, 8)))
  expect_false(has_no_missing_concentration(c(NA, NA, NA)))
})

test_that("has_positive_concentration returns TRUE for all positive values", {
  expect_true(has_positive_concentration(c(1, 2, 3, 4, 5, 6, 7, 8)))
  expect_true(has_positive_concentration(c(0.1, 0.5, 1.5, 2.3, 3.4, 4.5, 5.6, 6.7)))
})

test_that("has_positive_concentration returns FALSE for zero or negative values", {
  expect_false(has_positive_concentration(c(0, 1, 2, 3, 4, 5, 6, 7)))
  expect_false(has_positive_concentration(c(-1, 2, 3, 4, 5, 6, 7, 8)))
  expect_false(has_positive_concentration(c(1, 2, 3, 4, 5, 6, 7, -8)))
})

test_that("has_positive_concentration handles NA with na.rm", {
  expect_true(has_positive_concentration(c(1, 2, NA, 4, 5, 6, 7, 8)))
})

test_that("has_finite_concentration returns TRUE for finite values", {
  expect_true(has_finite_concentration(c(1, 2, 3, 4, 5, 6, 7, 8)))
  expect_true(has_finite_concentration(c(0.1, 100, 1000, 10000, 5, 6, 7, 8)))
})

test_that("has_finite_concentration returns FALSE for infinite values", {
  expect_false(has_finite_concentration(c(Inf, 2, 3, 4, 5, 6, 7, 8)))
  expect_false(has_finite_concentration(c(-Inf, 2, 3, 4, 5, 6, 7, 8)))
  expect_false(has_finite_concentration(c(1, 2, 3, 4, 5, 6, 7, Inf)))
})

test_that("has_min_concentration returns TRUE for >= 6 values", {
  expect_true(has_min_concentration(c(1, 2, 3, 4, 5, 6)))
  expect_true(has_min_concentration(1:10))
  expect_true(has_min_concentration(c(1, 1, 1, 1, 1, 1))) # 6 values, even if identical
})

test_that("has_min_concentration returns FALSE for < 6 values", {
  expect_false(has_min_concentration(c(1, 2, 3, 4, 5)))
  expect_false(has_min_concentration(c(1)))
  expect_false(has_min_concentration(numeric(0)))
})

test_that("has_not_all_identical returns TRUE for non-identical values", {
  expect_true(has_not_all_identical(c(1, 2, 3, 4, 5, 6)))
  expect_true(has_not_all_identical(c(1.0, 1.1, 2.0, 3.0, 4.0, 5.0)))
  expect_true(has_not_all_identical(c(1, 1, 2, 3, 4, 5))) # some duplicates but not all same
})

test_that("has_not_all_identical returns FALSE for identical values", {
  expect_false(has_not_all_identical(c(1, 1, 1, 1, 1, 1)))
  expect_false(has_not_all_identical(c(5.5, 5.5, 5.5)))
  expect_false(has_not_all_identical(rep(100, 10)))
})
