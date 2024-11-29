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
  as.numeric(stats::na.omit(breaks))
}

plot_predictions <- function(x, pred, conc, label, colour, shape, percent,
                             label_adjust, xaxis, yaxis, title, xmin, xmax, palette,
                             legend_colour, legend_shape, xbreaks, trans, text_size,
                             label_size, conc_value, big.mark) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }

  gp <- ssdtools::ssd_plot(x, pred,
    left = conc, label = label, xbreaks = xbreaks, size = label_size,
    color = colour, shape = shape, hc = proportion, ci = FALSE,
    shift_x = label_adjust %>% as.numeric(),
    xlab = xaxis, ylab = yaxis, trans = trans
  ) +
    ggplot2::scale_x_continuous(
      name = xaxis, breaks = xbreaks,
      limits = c(xmin, xmax),
      labels = function(lab) {
        do.call(
          expression,
          lapply(lab, function(x) {
            mark <- label_comma(x, big.mark = big.mark)
            if (!is.na(x) & x == conc_value) {
              y <- paste0("\n", mark)
            } else {
              y <- mark
            }
            y
          })
        )
      }
    ) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape)
  
  # get breaks again for bold face as limits convert some to NA 
  actual_breaks <- gp_xbreaks(gp)
  
  gp <- 
    gp +
    ggplot2::ggtitle(title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "black", size = text_size),
      axis.text.x = ggplot2::element_text(face =  bold_conc(conc_value, actual_breaks)),
      axis.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
    ) 
   

  gp
}
