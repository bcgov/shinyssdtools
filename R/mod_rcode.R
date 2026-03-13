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

# R Code Module UI
mod_rcode_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    padding = "1rem",
    gap = "1rem",
    sidebar = sidebar(
      width = 350,
      div(
        h5(span(`data-translate` = "ui_tabcode", "Get R code")),
      ) %>%
        shinyhelper::helper(
          type = "markdown",
          content = "rcodeTab",
          size = "l",
          colour = color_primary,
          buttonLabel = "OK"
        ),
      conditionalPanel(
        condition = paste_js("has_code", ns),
        uiOutput(ns("copyButton"))
      )
    ),
    card(
      class = card_shadow,
      card_body(
        tagList(
          div(
            id = ns("code-container"),
            class = "r-code-container",
            style = "
              background-color: #f8f9fa; 
              color: #212529; 
              padding: 1.5rem; 
              border-radius: 8px; 
              border: 1px solid #dee2e6;
              font-family: 'Fira Code', 'Consolas', 'Monaco', 'Courier New', monospace; 
              font-size: 14px; 
              line-height: 1.5;
              max-height: 70vh;
              overflow-y: auto;
            ",
            tags$style(HTML(
              "
              .r-code-container pre {
                background: transparent !important;
                border: none !important;
                padding: 0 !important;
                margin: 0.5rem 0 !important;
                white-space: pre-wrap !important;
                word-wrap: break-word !important;
                font-family: inherit !important;
                font-size: inherit !important;
                color: inherit !important;
              }
            "
            )),
            uiOutput(ns("codeHead")),
            uiOutput(ns("codeData")),
            uiOutput(ns("codeFit")),
            uiOutput(ns("codePredPlot")),
            uiOutput(ns("codeSavePred")),
            uiOutput(ns("codePredCl"))
          )
        )
      )
    )
  )
}

mod_rcode_server <- function(id, translations, data_mod, fit_mod, predict_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    code_label <- reactive({
      label_val <- predict_mod$select_label()
      if (is.null(label_val) || label_val == "-none-") {
        return("NULL")
      }
      paste0("'", label_val %>% make.names(), "'")
    })

    code_colour <- reactive({
      colour_val <- predict_mod$select_colour()
      if (is.null(colour_val) || colour_val == "-none-") {
        return("NULL")
      }
      paste0("'", colour_val %>% make.names(), "'")
    })

    code_shape <- reactive({
      shape_val <- predict_mod$select_shape()
      if (is.null(shape_val) || shape_val == "-none-") {
        return("NULL")
      }
      paste0("'", shape_val %>% make.names(), "'")
    })

    code_hc <- reactive({
      threshold_vals <- predict_mod$threshold_values()
      show_hc <- predict_mod$check_hc()
      if (is.null(show_hc) || !show_hc || is.null(threshold_vals)) {
        return("NULL")
      }
      threshold_vals$percent / 100
    })

    # Get plot dimensions from user inputs in download popovers
    get_width <- reactive({
      predict_mod$width() %||% 6
    })

    get_width2 <- reactive({
      fit_mod$width() %||% 6
    })

    get_height <- reactive({
      predict_mod$height() %||% 4
    })

    get_height2 <- reactive({
      fit_mod$height() %||% 4
    })

    get_dpi <- reactive({
      predict_mod$dpi() %||% 300
    })

    get_dpi2 <- reactive({
      fit_mod$dpi() %||% 300
    })

    # Code section outputs
    output$ui_4help <- renderUI({
      trans <- translations()
      HTML(tr("ui_4help", trans))
    })

    # Individual code outputs with clean formatting
    output$codeHead <- renderUI({
      code_lines <- generate_head_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeData <- renderUI({
      code_lines <- generate_data_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeFit <- renderUI({
      code_lines <- generate_fit_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codePredPlot <- renderUI({
      code_lines <- generate_pred_plot_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeSavePred <- renderUI({
      code_lines <- generate_save_pred_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codePredCl <- renderUI({
      code_lines <- generate_pred_cl_code()

      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    # Helper functions to generate code sections (DRY principle)
    generate_head_code <- function() {
      req(data_mod$has_data())
      data <- data_mod$data()

      c(
        "# install.packages('ssdtools')",
        "library(ssdtools)",
        "library(ggplot2)",
        "library(dplyr)"
      )
    }

    generate_data_code <- function() {
      req(data_mod$has_data())
      clean_data <- data_mod$clean_data()
      data_str <- utils::capture.output(dput(clean_data)) %>%
        glue::glue_collapse()

      c(
        paste0("data <- ", data_str),
        "colnames(data) <- make.names(colnames(data))"
      )
    }

    generate_fit_code <- function() {
      req(fit_mod$has_fit())
      ylab <- fit_mod$yaxis_label()
      xlab <- fit_mod$xaxis_label()
      text_size <- fit_mod$text_size()
      dists_str <- paste0(
        "c(",
        paste0("'", fit_mod$dists(), "'", collapse = ", "),
        ")"
      )

      title <- fit_mod$title()
      has_title <- !is.null(title) && title != ""

      plot_code <- c(
        paste0("ssd_plot_cdf("),
        paste0("  dist,"),
        paste0("  ylab = '", ylab, "',"),
        paste0("  xlab = '", xlab, "',"),
        paste0("  delta = Inf,"),
        paste0("  average = NA,"),
        paste0("  theme_classic = TRUE,"),
        paste0("  text_size = ", text_size, ","),
        paste0("  big.mark = '", fit_mod$big_mark(), "',"),
        paste0("  decimal.mark = '", fit_mod$decimal_mark(), "'"),
        if (has_title) ") +" else ")"
      )

      if (has_title) {
        plot_code <- c(plot_code, paste0("  ggtitle('", title, "')"))
      }

      save_plot <- c(
        paste0("ggsave("),
        paste0("  'fit_dist_plot.png',"),
        paste0("  width = ", get_width2(), ","),
        paste0("  height = ", get_height2(), ","),
        paste0("  dpi = ", get_dpi2()),
        ")"
      )

      c(
        paste0("dist <- ssd_fit_bcanz("),
        paste0("  data,"),
        paste0("  left = '", fit_mod$conc_column() %>% make.names(), "',"),
        paste0("  dists = ", dists_str, ","),
        paste0("  silent = TRUE,"),
        paste0("  rescale = ", fit_mod$rescale()),
        ")",
        "",
        plot_code,
        save_plot,
        "",
        "ssd_gof(dist, wt = TRUE) %>%",
        "    dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
             dplyr::arrange(dplyr::desc(wt))"
      )
    }

    generate_pred_plot_code <- function() {
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())

      threshold_vals <- predict_mod$threshold_values()
      xmax <- predict_mod$x_max()
      xmin <- predict_mod$x_min()
      xlimits <- ifelse(
        is.na(xmin) & is.na(xmax),
        "NULL",
        paste0("c(", xmin, ", ", xmax, ")")
      )
      legend.colour <- ifelse(
        is.null(predict_mod$legend_colour()) ||
          predict_mod$legend_colour() == "-none-",
        "NULL",
        paste0("'", predict_mod$legend_colour(), "'")
      )
      legend.shape <- ifelse(
        is.null(predict_mod$legend_shape()) ||
          predict_mod$legend_shape() == "-none-",
        "NULL",
        paste0("'", predict_mod$legend_shape(), "'")
      )
      text_size <- predict_mod$text_size()
      xlab <- predict_mod$xaxis_label()
      ylab <- predict_mod$yaxis_label()
      title <- predict_mod$title()
      trans <- ifelse(predict_mod$x_log(), "log10", "identity")
      xbreaks <- paste0(
        "c(",
        paste(predict_mod$xbreaks(), collapse = ", "),
        ")"
      )

      # Check if CI should be included
      include_ci <- predict_mod$include_ci() && predict_mod$cl_requested()
      ci_value <- if (include_ci) "TRUE" else "FALSE"

      # Get ribbon value
      ribbon_value <- if (predict_mod$ribbon()) "TRUE" else "FALSE"

      # Generate predict() call
      if (include_ci) {
        nboot <- predict_mod$cl_nboot()
        predict_code <- c(
          paste0("pred <- predict("),
          paste0("  dist,"),
          paste0(
            "  proportion = unique(c(1:99, ",
            threshold_vals$percent,
            ") / 100),"
          ),
          paste0("  nboot = ", nboot, ","),
          "  ci = TRUE",
          ")"
        )
      } else {
        predict_code <- c(
          paste0("pred <- predict("),
          paste0("  dist,"),
          paste0(
            "  proportion = unique(c(1:99, ",
            threshold_vals$percent,
            ") / 100)"
          ),
          ")"
        )
      }

      c(
        predict_code,
        "",
        paste0("ssd_plot("),
        paste0("  data,"),
        paste0("  pred,"),
        paste0("  left = '", make.names(fit_mod$conc_column()), "',"),
        paste0("  label = ", code_label(), ","),
        paste0("  shape = ", code_shape(), ","),
        paste0("  color = ", code_colour(), ","),
        paste0("  label_size = ", predict_mod$label_size(), ","),
        paste0("  ylab = '", ylab, "',"),
        paste0("  xlab = '", xlab, "',"),
        paste0("  ci = ", ci_value, ","),
        paste0("  ribbon = ", ribbon_value, ","),
        paste0("  shift_x = ", predict_mod$adjust_label(), ","),
        paste0("  hc = ", code_hc(), ","),
        paste0("  big.mark = '", predict_mod$big_mark(), "',"),
        paste0("  decimal.mark = '", predict_mod$decimal_mark(), "',"),
        paste0("  trans = '", trans, "',"),
        paste0("  xlimits = ", xlimits, ","),
        paste0("  xbreaks = ", xbreaks, ","),
        paste0("  text_size = ", text_size, ","),
        paste0("  theme_classic = TRUE"),
        ") +",
        paste0("  ggtitle('", title, "') +"),
        paste0(
          "  scale_color_brewer(palette = '",
          predict_mod$palette(),
          "', name = ",
          legend.colour,
          ") +"
        ),
        paste0("  scale_shape(name = ", legend.shape, ")")
      )
    }

    generate_save_pred_code <- function() {
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())
      req(predict_mod$select_label())

      c(
        paste0("ggsave("),
        paste0("  'model_average_plot.png',"),
        paste0("  width = ", get_width(), ","),
        paste0("  height = ", get_height(), ","),
        paste0("  dpi = ", get_dpi()),
        ")"
      )
    }

    generate_pred_cl_code <- function() {
      req(predict_mod$has_cl())
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())

      threshold_vals <- predict_mod$threshold_values()

      nboot_clean <- clean_nboot(predict_mod$nboot()) %>% as.integer()

      if (predict_mod$threshold_type() != "Concentration") {
        form <- "ssd_hp_bcanz"
        arg <- "conc"
        thresh <- threshold_vals$conc
        c(
          paste0("cl_average <- ", form, "("),
          paste0("  dist,"),
          paste0("  ", arg, " = ", thresh, ","),
          paste0("  ci = TRUE,"),
          paste0("  nboot = ", nboot_clean, "L,"),
          paste0("  proportion = TRUE,"),
          paste0("  min_pboot = 0.8"),
          ")",
          "",
          paste0("cl_individual <- ", form, "("),
          paste0("  dist,"),
          paste0("  ", arg, " = ", thresh, ","),
          paste0("  ci = TRUE,"),
          paste0("  average = FALSE,"),
          paste0("  nboot = ", nboot_clean, "L,"),
          paste0("  proportion = TRUE,"),
          paste0("  min_pboot = 0.8"),
          ")",
          "",
          "dplyr::bind_rows(cl_average, cl_individual) %>%
             dplyr::arrange(dplyr::desc(wt))"
        )
      } else {
        form <- "ssd_hc_bcanz"
        arg <- "proportion"
        thresh <- threshold_vals$percent / 100
        c(
          paste0("cl_average <- ", form, "("),
          paste0("  dist,"),
          paste0("  ", arg, " = ", thresh, ","),
          paste0("  ci = TRUE,"),
          paste0("  nboot = ", nboot_clean, "L,"),
          paste0("  min_pboot = 0.8"),
          ")",
          "",
          paste0("cl_individual <- ", form, "("),
          paste0("  dist,"),
          paste0("  ", arg, " = ", thresh, ","),
          paste0("  ci = TRUE,"),
          paste0("  average = FALSE,"),
          paste0("  nboot = ", nboot_clean, "L,"),
          paste0("  min_pboot = 0.8"),
          ")",
          "",
          "dplyr::bind_rows(cl_average, cl_individual) %>%
             dplyr::arrange(dplyr::desc(wt))"
        )
      }
    }

    all_code <- reactive({
      code_sections <- list()

      code_sections$head <- try(generate_head_code(), silent = TRUE)
      code_sections$data <- try(generate_data_code(), silent = TRUE)
      code_sections$fit <- try(generate_fit_code(), silent = TRUE)
      code_sections$pred_plot <- try(generate_pred_plot_code(), silent = TRUE)
      code_sections$save_pred <- try(generate_save_pred_code(), silent = TRUE)
      code_sections$pred_cl <- try(generate_pred_cl_code(), silent = TRUE)

      # Filter out errors and empty sections
      valid_sections <- Filter(
        function(x) {
          !inherits(x, "try-error") && !is.null(x) && length(x) > 0
        },
        code_sections
      )

      if (length(valid_sections) == 0) {
        return("")
      }

      all_lines <- c()
      for (section in valid_sections) {
        if (length(all_lines) > 0) {
          all_lines <- c(all_lines, "", section)
        } else {
          all_lines <- section
        }
      }

      format_r_code(all_lines)
    })

    output$copyButton <- renderUI({
      trans <- translations()
      label <- tr("ui_copy", trans)
      code_text <- all_code()
      rclipboard::rclipButton(
        inputId = ns("copyCode"),
        label = tagList(
          bsicons::bs_icon("clipboard"),
          label
        ),
        clipText = code_text,
        class = "btn-primary"
      )
    })

    # for copy button
    has_code <- reactive({
      all_code() != ""
    })

    output$has_code <- has_code
    outputOptions(output, "has_code", suspendWhenHidden = FALSE)
  })
}
