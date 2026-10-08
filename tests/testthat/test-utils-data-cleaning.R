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

# Internal Functions ----------------------------------------------------------

test_that("remove_blank_headers removes X1, X2 columns", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Species = c("A", "B", "C"),
    X1 = c(NA, NA, NA),
    X2 = c("", "", "")
  )

  result <- remove_blank_headers(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
})

test_that("remove_empty_columns removes all-NA and all-empty columns", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Species = c("A", "B", "C"),
    Empty1 = c(NA, NA, NA),
    Empty2 = c("", "", "")
  )

  result <- remove_empty_columns(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
})

test_that("remove_empty_rows removes all-NA and all-empty rows", {
  df <- data.frame(
    Concentration = c(1, NA, 3),
    Species = c("A", NA, "C")
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 2)
  expect_equal(result$Concentration, c(1, 3))
  expect_equal(result$Species, c("A", "C"))
})

test_that("remove_empty_rows preserves rows with partial data", {
  df <- data.frame(
    Concentration = c(1, 2, NA),
    Species = c("A", NA, "C")
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 3)
})

# Integration Test ------------------------------------------------------------

test_that("clean_ssd_data applies full cleaning pipeline", {
  # Test with realistic messy CSV data
  df <- data.frame(
    Concentration = c(1, 2, NA, 4, NA),
    Species = c("A", "B", NA, "D", ""),
    X1 = c(NA, NA, NA, NA, NA),
    Empty = c("", "", "", "", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_ssd_data(df)

  # Should remove X1 and Empty columns, and rows 3 and 5
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
  expect_equal(nrow(result), 3)
  expect_equal(result$Concentration, c(1, 2, 4))
  expect_equal(result$Species, c("A", "B", "D"))
})
