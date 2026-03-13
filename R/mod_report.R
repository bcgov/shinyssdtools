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

# Report Module UI
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = paste_js('has_predict', ns = ns),
      layout_sidebar(
        padding = "1rem",
        gap = "1rem",
        sidebar = sidebar(
          width = 350,
          style = "height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden;",
          tagList(
            div(
              h5(span(`data-translate` = "ui_tabreport", "Get BCANZ report")),
            ) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "reportTab",
                size = "l",
                colour = color_primary,
                buttonLabel = "OK"
              ),
            textInput(
              ns("toxicant"),
              label = span(`data-translate` = "ui_4toxname", "Toxicant name"),
              value = ""
            ),
            selectizeInput(
              ns("bootSamp"),
              options = list(
                create = TRUE,
                createFilter = "^(?:[1-9][0-9]{0,3}|10000)$"
              ),
              label = span(
                `data-translate` = "ui_3samples",
                "Bootstrap samples"
              ),
              choices = c("500", "1,000", "5,000", "10,000"),
              selected = "10,000"
            ),
            actionButton(
              ns("generateReport"),
              label = tagList(
                bsicons::bs_icon(
                  "file-earmark-text",
                  class = color_button_icon
                ),
                span(`data-translate` = "ui_getreport", "Get Report")
              ),
              class = "btn-primary w-100"
            )
          )
        ),
        div(
          class = "p-3",
          conditionalPanel(
            condition = paste_js("has_preview", ns),
            card(
              class = card_shadow,
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_prevreport", "Preview report")
              ),
              card_body(
                padding = 25,
                ui_download_report(ns = ns),
                tags$iframe(
                  srcdoc = "",
                  id = ns("htmlPreview"),
                  style = "width: 100%; height: 600px; border: 1px solid #ddd; border-radius: 4px; background: white;",
                  sandbox = "allow-same-origin allow-scripts allow-popups allow-popups-to-escape-sandbox"
                )
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = paste0("!output['", ns("has_predict"), "']"),
      ui_dashbox(span(
        `data-translate` = "ui_hintpredict",
        "You have not successfully generated predictions yet. Run the 'Predict' tab first."
      ))
    )
  )
}

# Report Module Server
mod_report_server <- function(
  id,
  translations,
  lang,
  data_mod,
  fit_mod,
  predict_mod,
  shared_toxicant_name = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_predict <- predict_mod$has_predict
    outputOptions(output, "has_predict", suspendWhenHidden = FALSE)

    waiting_screen_report <- reactive({
      trans <- translations()
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(tr("ui_4gentitle", trans)),
          br(),
          h4(tr("ui_4genbody", trans))
        )
      )
    })

    pred_cl <- reactive({
      fit <- fit_mod$fit_dist()
      req(fit)
      nboot <- clean_nboot(input$bootSamp)
      avehc <- ssdtools::ssd_hc_bcanz(
        fit,
        proportion = c(0.01, 0.05, 0.1, 0.2),
        ci = TRUE,
        nboot = nboot,
        min_pboot = 0.8
      )
      avehc |>
        dplyr::mutate(HCx = .data$proportion * 100, PCx = (1 - .data$proportion) * 100) |>
        dplyr::select(.data$HCx, .data$PCx, .data$est, .data$se, .data$lcl, .data$ucl, .data$nboot, .data$pboot)
    })

    observe({
      current <- lang()
      nboot_value <- predict_mod$nboot()

      # Default choices based on language
      if (current == "french") {
        choices <- c("500", "1 000", "5 000", "10 000")
        standard_values <- c("500", "1000", "5000", "10000")
      } else {
        choices <- c("500", "1,000", "5,000", "10,000")
        standard_values <- c("500", "1000", "5000", "10000")
      }

      # Check if nboot_value is a custom value (not in standard list)
      nboot_clean <- clean_nboot(nboot_value)
      if (!is.null(nboot_value) && !nboot_clean %in% standard_values) {
        choices <- c(choices, nboot_value)
      }

      updateSelectizeInput(
        session,
        "bootSamp",
        choices = choices,
        selected = nboot_value
      )
    }) %>%
      bindEvent(lang(), predict_mod$nboot())

    # Update toxicant input when shared value changes from another module
    if (!is.null(shared_toxicant_name)) {
      observe({
        toxicant_name <- shared_toxicant_name()
        if (!is.null(toxicant_name) && toxicant_name != "" &&
            toxicant_name != input$toxicant) {
          updateTextInput(
            session,
            "toxicant",
            value = toxicant_name
          )
        }
      }) %>%
        bindEvent(shared_toxicant_name())

      # Update shared value when this module's input changes
      observe({
        shared_toxicant_name(input$toxicant)
      }) %>%
        bindEvent(input$toxicant)
    }

    params_list <- reactive({
      req(predict_mod$has_predict())
      req(fit_mod$has_fit())

      toxicant <- input$toxicant
      data <- data_mod$data()
      dists <- fit_mod$dists()
      fit_plot <- fit_mod$fit_plot()
      gof_table <- fit_mod$gof_table()
      model_average_plot <- predict_mod$model_average_plot()
      pred <- pred_cl()

      params <- list(
        toxicant = toxicant,
        data = data,
        dists = dists,
        fit_plot = fit_plot,
        gof_table = gof_table,
        model_average_plot = model_average_plot,
        pred_cl = pred
      )
      params
    })

    # Generate HTML report for preview
    report_preview_html <- reactive({
      waiter::waiter_show(
        html = waiting_screen_report(),
        color = color_secondary
      )

      on.exit(waiter::waiter_hide(), add = TRUE)

      trans <- translations()
      temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans))
      file.copy(
        system.file(
          package = "shinyssdtools",
          file.path("extdata", tr("ui_bcanz_file", trans))
        ),
        temp_report
      )

      temp_html <- tempfile(fileext = ".html")
      params <- params_list()

      # Create render environment and explicitly assign params
      render_env <- new.env(parent = globalenv())
      assign("params", params, envir = render_env)

      suppressMessages(
        rmarkdown::render(
          temp_report,
          output_format = "html_document",
          output_file = temp_html,
          params = params,
          envir = render_env,
          encoding = "utf-8",
          quiet = TRUE
        )
      )

      html_content <- readLines(temp_html, warn = FALSE)
      html_string <- paste(html_content, collapse = "\n")

      # Add target="_blank" to all links to open them in new tab instead of within iframe
      html_string <- gsub('<a href=', '<a target="_blank" href=', html_string, fixed = TRUE)

      html_string
    }) %>%
      bindEvent(input$generateReport)

    has_preview <- reactive({
      !is.null(report_preview_html())
    }) %>%
      bindEvent(report_preview_html())

    output$has_preview <- has_preview
    outputOptions(output, "has_preview", suspendWhenHidden = FALSE)

    # Update iframe content with HTML
    observe({
      html_content <- report_preview_html()
      if (!is.null(html_content)) {
        # Use JavaScript to safely update iframe srcdoc
        shinyjs::runjs(paste0(
          "
          var iframe = document.getElementById('",
          ns("htmlPreview"),
          "');
          if (iframe) {
            iframe.srcdoc = ",
          jsonlite::toJSON(html_content),
          ";
          }
        "
        ))
      }
    }) %>%
      bindEvent(report_preview_html())

    # Generate fresh PDF for download
    output$reportDlPdf <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".pdf")
      },
      content = function(file) {
        trans <- translations()
        temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans))
        file.copy(
          system.file(
            package = "shinyssdtools",
            file.path("extdata", tr("ui_bcanz_file", trans))
          ),
          temp_report
        )
        params <- params_list()
        render_env <- new.env(parent = globalenv())
        assign("params", params, envir = render_env)

        rmarkdown::render(
          temp_report,
          output_format = "pdf_document",
          output_file = file,
          params = params,
          envir = render_env,
          encoding = "utf-8"
        )
      }
    )

    # Reuse HTML preview for download
    output$reportDlHtml <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".html")
      },
      content = function(file) {
        writeLines(report_preview_html(), file)
      }
    )
  })
}
