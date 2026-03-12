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

utils::globalVariables(c(".", "at_bound", "computable", "log_lik", "aic", "aicc", "bic"))

#' Safely try an expression and return NULL on error
#' @param expr An R expression to evaluate
#' @param silent Logical indicating whether to suppress error messages (default: TRUE)
#' @return The result of expr on success, NULL on error
#' @keywords internal
safe_try <- function(expr, silent = TRUE) {
  result <- try(expr, silent = silent)
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  result
}

#' Clean bootstrap sample count from formatted string
#' @param x Character string representing a number with separators
#' @return Integer value with separators removed
#' @keywords internal
clean_nboot <- function(x) {
  as.integer(gsub("(,|\\s)", "", x))
}

#' Remove Excel/Numbers blank header columns (X1, X2, etc.)
#' @param data A data frame or tibble
#' @return A tibble with X1, X2, ... X200 columns removed
#' @keywords internal
remove_blank_headers <- function(data) {
  data %>%
    dplyr::select(-dplyr::any_of(paste0("X", 1:200)))
}

#' Remove completely empty columns
#' @param data A data frame or tibble
#' @return A tibble with empty columns removed
#' @keywords internal
remove_empty_columns <- function(data) {
  data %>%
    dplyr::as_tibble() %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x) | .x == "")))
}

#' Remove completely empty rows
#' @param data A data frame or tibble
#' @return A tibble with empty rows removed
#' @keywords internal
remove_empty_rows <- function(data) {
  data %>%
    dplyr::as_tibble() %>%
    dplyr::filter(
      if (ncol(.) > 0) {
        !dplyr::if_all(dplyr::everything(), ~ is.na(.x) | .x == "")
      } else {
        TRUE
      }
    )
}

#' Clean uploaded data
#' @param data A data frame or tibble
#' @return A cleaned tibble
#' @export
clean_ssd_data <- function(data) {
  data %>%
    remove_blank_headers() %>%
    remove_empty_columns() %>%
    remove_empty_rows()
}

#' Check if concentration values are numeric
#' @param x A vector
#' @return TRUE if numeric, FALSE otherwise
#' @keywords internal
has_numeric_concentration <- function(x) {
  is.numeric(x)
}

#' Check if concentration values have no missing values
#' @param x A vector
#' @return TRUE if no missing values, FALSE otherwise
#' @keywords internal
has_no_missing_concentration <- function(x) {
  !anyNA(x)
}

#' Check if concentration values are all positive
#' @param x A numeric vector
#' @return TRUE if all positive, FALSE otherwise
#' @keywords internal
has_positive_concentration <- function(x) {
  all(x > 0, na.rm = TRUE)
}

#' Check if concentration values are all finite
#' @param x A numeric vector
#' @return TRUE if all finite, FALSE otherwise
#' @keywords internal
has_finite_concentration <- function(x) {
  all(is.finite(x))
}

#' Check if there are at least 6 concentration values
#' @param x A vector
#' @return TRUE if at least 6 values, FALSE otherwise
#' @keywords internal
has_min_concentration <- function(x) {
  length(x) >= 6
}

#' Check if concentration values are not all identical
#' @param x A numeric vector
#' @return TRUE if values are not all identical, FALSE otherwise
#' @keywords internal
has_not_all_identical <- function(x) {
  !zero_range(x)
}

#' Estimate bootstrap computation time
#' @param nboot Integer number of bootstrap samples
#' @param lang Character string language code: "english", "french", or "spanish"
#' @return Character string with formatted time estimate
#' @keywords internal
estimate_time <- function(nboot, lang) {
  preset_df <- data.frame(
    n = c(500, 1000, 5000, 10000),
    english = c("10 seconds", "20 seconds", "2 minutes", "5 minutes"),
    french = c("10 secondes", "20 secondes", "2 minutes", "5 minutes"),
    spanish = c("10 segundos", "20 segundos", "2 minutos", "5 minutos")
  )

  if (nboot %in% preset_df$n) {
    return(preset_df[preset_df$n == nboot, ][[lang]])
  }

  # use linear model if not in preset:
  time_sec <- max(1, -12.89 + 0.0304 * nboot)

  if (time_sec < 60) {
    time_num <- round(time_sec)
    time_str <- switch(
      lang,
      "french" = paste(time_num, ifelse(time_num <= 1, "seconde", "secondes")),
      "spanish" = paste(time_num, ifelse(time_num <= 1, "segundo", "segundos")),
      paste(time_num, ifelse(time_num <= 1, "second", "seconds"))  # Default English
    )
  } else {
    time_num <- round(time_sec / 60, 1)
    time_str <- switch(
      lang,
      "french" = paste(time_num, ifelse(time_num <= 1, "minute", "minutes")),
      "spanish" = paste(time_num, ifelse(time_num <= 1, "minuto", "minutos")),
      paste(time_num, ifelse(time_num <= 1, "minute", "minutes"))  # Default English
    )
  }

  time_str
}

#' Get translation value by ID
#' @param id Character string translation identifier
#' @param trans Data frame with columns 'id' and 'trans'
#' @return Character vector of translation text(s)
#' @keywords internal
tr <- function(id, trans) {
  trans$trans[trans$id == id]
}

#' Create JavaScript conditional for Shiny output
#' @param x Character string output name
#' @param ns Shiny namespace function
#' @return Character string with JavaScript expression
#' @keywords internal
paste_js <- function(x, ns) {
  paste0("output['", ns(x), "']")
}

#' Guess species column name
#' @param name Character vector of column names
#' @return Character string of matched column name, or NA if no match
#' @keywords internal
guess_sp <- function(name) {
  name[grepl("sp", tolower(name))][1]
}

#' Guess concentration column name
#' @param name Character vector of column names
#' @param data Optional data frame to identify numeric columns
#' @return Character string of matched column name, or NA if no match
#' @keywords internal
guess_conc <- function(name, data = NULL) {
  # First try to find column with 'conc' in name
  conc_match <- name[grepl("conc", tolower(name))][1]
  if (!is.na(conc_match)) {
    return(conc_match)
  }

  # If no 'conc' match and data provided, find first numeric column
  if (!is.null(data)) {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    if (any(numeric_cols)) {
      return(names(data)[numeric_cols][1])
    }
  }

  # Return NA if no match found
  return(NA_character_)
}

#' Add mandatory field indicator to label
#' @param label Character string or tag for the label
#' @return tagList with label and asterisk span
#' @keywords internal
label_mandatory <- function(label) {
  tagList(label, span("*", class = "mandatory_star"))
}

#' Create inline-block div wrapper
#' @param x Shiny UI element(s) to wrap
#' @return tags$div with inline-block styling
#' @keywords internal
inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}

#' Create grey hint text
#' @param x Character string hint text
#' @return HTML object with grey font color
#' @keywords internal
hint <- function(x) {
  HTML(paste0("<font color='grey'>", x, "</font>"))
}

#' Check if values have zero range
#' @param x Numeric vector
#' @param tol Tolerance for comparison (default: sqrt of machine precision)
#' @return TRUE if range is zero (within tolerance), FALSE otherwise
#' @keywords internal
zero_range <- function(x, tol = .Machine$double.eps^0.5) {
  if (length(x) == 1) {
    return(TRUE)
  }
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

#' Estimate hazard concentration
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param percent Numeric percent of species affected (0-100 scale)
#' @return Numeric concentration estimate
#' @keywords internal
estimate_hc <- function(x, percent) {
  ssdtools::ssd_hc_bcanz(x, proportion = percent / 100)$est
}

#' Estimate hazard percent
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param conc Numeric concentration value
#' @return Numeric proportion estimate (0-1 scale)
#' @keywords internal
estimate_hp <- function(x, conc) {
  ssdtools::ssd_hp_bcanz(x, conc = conc, proportion = TRUE)$est
}

#' Calculate threshold percent from concentration with rounding
#' @param fit A fitdists object
#' @param conc Numeric concentration value
#' @param digits Number of significant digits for rounding (default: 3)
#' @return Numeric percent value (0-100 scale)
#' @keywords internal
calculate_threshold_percent <- function(fit, conc, digits = 3) {
  signif(estimate_hp(fit, conc), digits) * 100
}

#' Calculate threshold concentration from percent with rounding
#' @param fit A fitdists object
#' @param thresh Numeric threshold percent (0-100 scale)
#' @param digits Number of significant digits for rounding (default: 3)
#' @return Numeric concentration value
#' @keywords internal
calculate_threshold_conc <- function(fit, thresh, digits = 3) {
  signif(estimate_hc(fit, thresh), digits)
}

#' Calculate hazard concentration with confidence intervals
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param percent Numeric percent of species affected (0-100 scale)
#' @param nboot Integer number of bootstrap samples for confidence intervals
#' @return Data frame with columns: dist, est, se, lcl, ucl, wt
#' @keywords internal
ssd_hc_ave <- function(x, percent, nboot) {
  dist <- ssdtools::ssd_hc_bcanz(
    x,
    proportion = percent / 100,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hc_bcanz(
      x,
      proportion = percent / 100,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}

#' Calculate hazard percent with confidence intervals
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param conc Numeric concentration value
#' @param nboot Integer number of bootstrap samples for confidence intervals
#' @return Data frame with columns: dist, est, se, lcl, ucl, wt
#' @keywords internal
ssd_hp_ave <- function(x, conc, nboot) {
  dist <- ssdtools::ssd_hp_bcanz(
    x,
    conc = conc,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8,
    proportion = TRUE
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hp_bcanz(
      x,
      conc = conc,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8,
      proportion = TRUE
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}

#' Format R code with proper styling
#' @param code_lines Character vector of R code lines
#' @return Single character string with formatted, styled code
#' @keywords internal
format_r_code <- function(code_lines) {
  # Join lines into a single string
  code_text <- paste(code_lines, collapse = "\n")

  # Use styler to format the code
  # scope = "tokens" provides lighter-weight formatting focused on spacing/indentation
  formatted <- styler::style_text(code_text, scope = "tokens")

  # Convert formatted text back to a single string
  formatted_text <- paste(formatted, collapse = "\n")

  # Replace double quotes with single quotes
  # This is done after styling to maintain R syntax validity during formatting
  # Important for structure() output from dput() which uses double quotes
  formatted_text <- gsub('"', "'", formatted_text)

  formatted_text
}
