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

#' Plot species sensitivity distributions
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param ylab Character string for y-axis label
#' @param xlab Character string for x-axis label
#' @param text_size Numeric text size for plot elements
#' @param big.mark Character used as thousands separator (e.g., "," or " ")
#' @param decimal.mark Character used as decimal separator (e.g., "." or ",")
#' @param title Optional character string plot title (default: NULL)
#' @return ggplot2 object with SSD CDF plot
#' @keywords internal
plot_distributions <- function(
  x,
  ylab,
  xlab,
  text_size,
  big.mark,
  decimal.mark,
  title = NULL
) {
  gp <- ssdtools::ssd_plot_cdf(
    x,
    ylab = ylab,
    xlab = xlab,
    delta = Inf,
    average = NA,
    theme_classic = TRUE,
    text_size = text_size,
    big.mark = big.mark,
    decimal.mark = decimal.mark
  )

  if (!is.null(title) && title != "") {
    gp <- gp + ggplot2::ggtitle(title)
  }

  gp
}

#' Extract and format x-axis breaks from ggplot
#' @param gp A ggplot2 object
#' @return Numeric vector of sorted, rounded x-axis break values
#' @keywords internal
gp_xbreaks <- function(gp) {
  breaks <- ggplot2::ggplot_build(gp)$layout$panel_params[[1]]$x$breaks
  sort(signif(as.numeric(stats::na.omit(breaks)), 3))
}

#' Plot SSD predictions with confidence intervals
#' @param x A fitdists object from ssd_fit_bcanz()
#' @param pred Prediction data frame from predict()
#' @param conc Character string column name for concentration values
#' @param label Character string column name for point labels
#' @param colour Character string column name for point colors
#' @param shape Character string column name for point shapes
#' @param percent Numeric hazard concentration percent to display (0-100 scale)
#' @param label_adjust Numeric horizontal adjustment for point labels
#' @param xaxis Character string x-axis label
#' @param yaxis Character string y-axis label
#' @param title Character string plot title
#' @param xmin Numeric minimum x-axis limit (use NA for auto)
#' @param xmax Numeric maximum x-axis limit (use NA for auto)
#' @param palette Character string ggplot2 color palette name
#' @param legend_colour Character string legend title for color aesthetic
#' @param legend_shape Character string legend title for shape aesthetic
#' @param xbreaks Optional numeric vector of x-axis break points (default: NULL)
#' @param trans Character string axis transformation (e.g., "log10")
#' @param text_size Numeric text size for plot elements
#' @param label_size Numeric size for point labels
#' @param conc_value Numeric concentration value for threshold line
#' @param big.mark Character used as thousands separator
#' @param decimal.mark Character used as decimal separator
#' @param ci Logical whether to display confidence intervals (default: FALSE)
#' @param ribbon Logical whether to display CI as ribbon vs lines (default: TRUE)
#' @return ggplot2 object with SSD prediction plot
#' @keywords internal
plot_predictions <- function(
  x,
  pred,
  conc,
  label,
  colour,
  shape,
  percent,
  label_adjust,
  xaxis,
  yaxis,
  title,
  xmin,
  xmax,
  palette,
  legend_colour,
  legend_shape,
  xbreaks = NULL,
  trans,
  text_size,
  label_size,
  conc_value,
  big.mark,
  decimal.mark,
  ci = FALSE,
  ribbon = TRUE
) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }

  xlimits <- c(xmin, xmax)
  if (is.na(xmin) & is.na(xmax)) {
    xlimits <- NULL
  }

  gp <- ssdtools::ssd_plot(
    x,
    pred = pred,
    left = conc,
    label = label,
    shape = shape,
    color = colour,
    label_size = label_size,
    xlab = xaxis,
    ylab = yaxis,
    ci = ci,
    ribbon = ribbon,
    hc = proportion,
    shift_x = as.numeric(label_adjust),
    big.mark = big.mark,
    trans = trans,
    xlimits = xlimits,
    xbreaks = xbreaks,
    text_size = text_size,
    theme_classic = TRUE,
    decimal.mark = decimal.mark
  ) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape) +
    ggplot2::ggtitle(title)

  gp
}
