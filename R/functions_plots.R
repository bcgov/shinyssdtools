plot_distributions <- function(x, ylab, lang) {
  gp <- ssdtools::ssd_plot_cdf(x, ylab = ylab)
  if (lang == "French") {
    gp <- gp + ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, " %"), name = ylab)
  }
  gp
}

plot_predictions <- function(x, pred, conc, label, colour, shape, percent,
                             label_adjust, xaxis, yaxis, title, xmax, palette,
                             legend_colour, legend_shape, lang) {
  gp <- ssdtools::ssd_plot(x, pred,
    left = conc, label = label,
    color = colour, shape = shape, hc = percent, ci = TRUE,
    shift_x = label_adjust %>% as.numeric(),
    xlab = xaxis, ylab = yaxis
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA)
    ) +
    ggplot2::expand_limits(x = xmax) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape)
  if (lang == "French") {
    gp <- gp + ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, " %"), name = yaxis)
  }
  gp
}
