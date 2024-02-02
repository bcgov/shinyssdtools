plot_distributions <- function(x, ylab, xlab, text_size) {
  gp <- ssdtools::ssd_plot_cdf(x, ylab = ylab, xlab = xlab, delta = Inf)
  gp <- gp + ggplot2::theme(axis.text = ggplot2::element_text(size = text_size), 
                            axis.title = ggplot2::element_text(size = text_size),
                            legend.title = ggplot2::element_text(size = text_size),
                            legend.text = ggplot2::element_text(size = text_size))
  suppressMessages({
    gp <- gp + ggplot2::scale_y_continuous(
      labels = function(x) paste0(x * 100),
      name = ylab
    )
  })
  gp
}

plot_predictions <- function(x, pred, conc, label, colour, shape, percent,
                             label_adjust, xaxis, yaxis, title, xmin, xmax, palette,
                             legend_colour, legend_shape, xbreaks, trans, text_size, label_size) {
  proportion <- percent / 100
  if(!length(proportion))
    proportion <- NULL
  
  gp <- ssdtools::ssd_plot(x, pred,
    left = conc, label = label, xbreaks = xbreaks, size = label_size,
    color = colour, shape = shape, hc = proportion, ci = FALSE,
    shift_x = label_adjust %>% as.numeric(),
    xlab = xaxis, ylab = yaxis
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = "black"),
      axis.text = ggplot2::element_text(color = "black", size = text_size),
      axis.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA)
    ) +
    ggplot2::coord_trans(x = trans) +
    ggplot2::scale_x_continuous(name = xaxis, breaks = xbreaks, 
                                limits = c(xmin, xmax), labels = comma_signif) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape)

  suppressMessages({
    gp <- gp + ggplot2::scale_y_continuous(
      labels = function(x) paste0(x * 100),
      name = yaxis
    )
  })
  gp
}
