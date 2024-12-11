label_comma <- function(x, digits = 3, big.mark = ",") {
  x <- signif(x, digits = digits)
  y <- as.character(x)
  bol <- !is.na(x) & as.numeric(x) >= 1000
  y[bol] <- stringr::str_replace_all(y[bol], "(\\d{1,1})(\\d{3,3}(?<=\\.|$))", paste0("\\1", big.mark, "\\2"))
  y
}

plot_distributions <- function(x, ylab, xlab, text_size) {
  gp <- ssdtools::ssd_plot_cdf(x,
    ylab = ylab, xlab = xlab,
    delta = Inf, average = NA
  )
  gp <-
    gp +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = text_size),
      axis.title = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size)
    )
  gp
}

bold_conc <- function(conc, breaks) {
  ifelse(breaks == conc, "bold", "plain")
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
    ci = FALSE, hc = proportion, shift_x = label_adjust %>% as.numeric(),
    big.mark = big.mark, trans = trans, xlimits = xlimits, 
    xbreaks = xbreaks, text_size = text_size
  ) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape)
   

  gp
}
