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

# Make internal functions available for testing by explicitly
# importing them from the package namespace into the test environment
.ns <- asNamespace("shinyssdtools")

# List of internal functions that tests need access to
internal_fns <- c(
  "mod_data_server", "mod_fit_server", "mod_predict_server",
  "clean_nboot", "append_unit", "guess_conc", "guess_sp",
  "zero_range", "estimate_time", "tr", "safe_try",
  "calculate_threshold_percent", "calculate_threshold_conc",
  "format_r_code", "translations", "boron.data"
)

# Import each function into the test environment
for (fn in internal_fns) {
  if (exists(fn, envir = .ns, inherits = FALSE)) {
    assign(fn, get(fn, envir = .ns), envir = .GlobalEnv)
  }
}
