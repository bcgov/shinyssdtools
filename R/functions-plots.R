plot_distributions <- function(x, ylab, xlab, text_size) {
  gp <- ssdtools::ssd_plot_cdf(x,
    ylab = ylab, xlab = xlab,
    delta = Inf, average = NA, theme_classic = TRUE, text_size = text_size
  )
  gp
}

gp_xbreaks <- function(gp){
  breaks <- ggplot2::ggplot_build(gp)$layout$panel_params[[1]]$x$breaks
  sort(signif(as.numeric(na.omit(breaks)), 3))
}

plot_predictions <- function(x, pred, conc, label, colour, shape, percent,
                             label_adjust, xaxis, yaxis, title, xmin, xmax, palette,
                             legend_colour, legend_shape, xbreaks = NULL, trans, text_size,
                             label_size, conc_value, big.mark) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }

  xlimits <- c(xmin, xmax)
  if(is.na(xmin) & is.na(xmax))
    xlimits <- NULL
  
  gp <- ssdtools::ssd_plot(x, pred = pred,
    left = conc, label = label, shape = shape, color = colour,
    label_size = label_size, xlab = xaxis, ylab = yaxis,
    ci = FALSE, hc = proportion, shift_x = as.numeric(label_adjust),
    big.mark = big.mark, trans = trans, xlimits = xlimits, 
    xbreaks = xbreaks, text_size = text_size, theme_classic = TRUE
  ) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape) 
   
  gp
}
