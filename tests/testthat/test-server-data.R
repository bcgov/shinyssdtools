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

# testServer Tests for Data Module
# ==================================
# Tests for reactive logic in mod_data_server using testServer

# Demo Data Tests -------------------------------------------------------------

# basic demo data functionality works ------------------------------------
test_that("mod_data_server loads demo data when demo button clicked", {
  testServer(
    mod_data_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english")
    ),
    {
      # Initially no data
      expect_false(has_data())

      # Click demo data button
      session$setInputs(demoData = 1)
      session$flushReact()

      expect_true(has_data())

      # Check data structure
      data <- clean_data()
      expect_true(is.data.frame(data))
      expect_true(nrow(data) == nrow(boron.data))
      expect_equal(names(data), c("Species", "Conc", "Group"))
    }
  )
})

# toxicant name passed through -------------------------------------------
test_that("toxicant name can be changed", {
  testServer(
    mod_data_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english")
    ),
    {
      # Initially no data
      # Click demo data button
      session$setInputs(demoData = 1, toxicant = "test")
      session$flushReact()

      returned <- session$returned
      expect_equal(returned$toxicant_name(), "test")
    }
  )
})

# Module Return Values Tests --------------------------------------------------
test_that("mod_data_server returns all expected reactive values", {
  testServer(
    mod_data_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      returned <- session$returned
      # Check all return values exist
      expect_true(is.reactive(returned$data))
      expect_true(is.reactive(returned$clean_data))
      expect_true(is.reactive(returned$has_data))
      expect_true(is.reactive(returned$toxicant_name))

      # Check they can be called
      expect_no_error(returned$data())
      expect_no_error(returned$clean_data())
      expect_no_error(returned$has_data())
      expect_no_error(returned$toxicant_name())
    }
  )
})

# Translation Tests -----------------------------------------------------------
test_that("demo data has French column names when language is French", {
  # Set up French translations
  test_trans_fr <- translations
  test_trans_fr$trans <- test_trans_fr[["french"]]

  testServer(
    mod_data_server,
    args = list(
      translations = reactive(test_trans_fr),
      lang = reactive("french")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      data <- clean_data()
      expect_true(is.data.frame(data))
      expect_equal(
        names(data),
        c("Espèce", "Conc", "Groupe")
      )
    }
  )
})
