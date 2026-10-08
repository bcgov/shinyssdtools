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

#' Print plot silently
#' @param x A plot object (typically ggplot2)
#' @return NULL (invisibly prints plot)
#' @keywords internal
silent_plot <- function(x) {
  suppressMessages(suppressWarnings(print(x)))
}

#' Get available concentration units
#' @return Character vector of unit strings with Unicode symbols
#' @keywords internal
units <- function() {
  c(
    "",
    "mg/L",
    "\u00b5g/L",
    "ng/L",
    "%",
    "mg/kg",
    "\u00b5g/kg",
    "mol/L",
    "mmol/L",
    "\u00b5mol/L"
  )
}

#' Append unit to label text
#' @param x Character string label text
#' @param unit Character string unit (e.g., "mg/L")
#' @return Character string with unit appended in parentheses, or original if unit empty
#' @keywords internal
append_unit <- function(x, unit) {
  if (unit == "") {
    return(x)
  }
  paste0(x, " (", unit, ")")
}
