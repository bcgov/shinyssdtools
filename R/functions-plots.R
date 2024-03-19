plot_distributions <- function(x, ylab, xlab, text_size) {
  gp <- ssdtools::ssd_plot_cdf(x, ylab = ylab, xlab = xlab, 
                               delta = Inf, average = NA)
  gp <-
    gp +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = text_size),
      axis.title = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size)
    )

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
                             legend_colour, legend_shape, xbreaks, trans, text_size, 
                             label_size, conc_value) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }

  gp <- ssdtools::ssd_plot(x, pred,
    left = conc, label = label, xbreaks = xbreaks, size = label_size,
    color = colour, shape = shape, hc = proportion, ci = FALSE,
    shift_x = label_adjust %>% as.numeric(),
    xlab = xaxis, ylab = yaxis
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "black", size = text_size),
      axis.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
    ) +
    ggplot2::coord_trans(x = trans) +
    ggplot2::scale_x_continuous(
      name = xaxis, breaks = xbreaks,
      limits = c(xmin, xmax), 
      guide = guide_axis(n.dodge = 2),
      labels = function(lab) {
        do.call(
          expression,
          lapply(paste(lab), function(x){
            if(x == paste(conc_value)){
              y <- bquote(bold(.(x)))
            } else {
              y <- x
            }
            y
          } 
        ))
      }
    ) +
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
