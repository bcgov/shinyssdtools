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
  if (conc %in% breaks) {
    b_pos <- purrr::map_int(conc, ~ which(. == breaks))
    b_vec <- rep("plain", length(breaks))
    b_vec[b_pos] <- "bold"
  } else {
    b_vec <- rep("plain", length(breaks))
  }
  b_vec
}

plot_predictions <- function(x, pred, conc, label, colour, shape, percent,
                             label_adjust, xaxis, yaxis, title, xmin, xmax, palette,
                             legend_colour, legend_shape, xbreaks, trans, text_size,
                             label_size, conc_value, big.mark) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }
  boldc <- bold_conc(conc_value, xbreaks)

  gp <- ssdtools::ssd_plot(x, pred,
    left = conc, label = label, xbreaks = xbreaks, size = label_size,
    color = colour, shape = shape, hc = proportion, ci = FALSE,
    shift_x = label_adjust %>% as.numeric(),
    xlab = xaxis, ylab = yaxis, trans = trans
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "black", size = text_size),
      axis.text.x = ggplot2::element_text(face = boldc),
      axis.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size),
      legend.title = ggplot2::element_text(size = text_size),
    ) +
    ggplot2::scale_x_continuous(
      name = xaxis, breaks = xbreaks,
      limits = c(xmin, xmax),
      labels = function(lab) {
        do.call(
          expression,
          lapply(lab, function(x) {
            mark <- label_comma(x, big.mark = big.mark)
            if (x == conc_value) {
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

  gp
}
