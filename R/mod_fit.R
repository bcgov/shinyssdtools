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

# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = paste_js('has_data', ns = ns),
      layout_sidebar(
        padding = "1rem",
        gap = "1rem",
        sidebar = sidebar(
          width = 400,
          style = "height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden;",
          tagList(
            div(
              h5(span(`data-translate` = "ui_tabfit", "Fit distributions")),
            ) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "fitTab",
                size = "l",
                colour = color_primary,
                buttonLabel = "OK"
              ),
            tags$label(
              `for` = ns("selectConc"),
              class = "control-label",
              span(`data-translate` = "ui_2conc", "Concentration")
            ),
            selectInput(
              ns("selectConc"),
              label = NULL,
              choices = NULL,
              selected = NULL
            ),
            selectizeInput(
              ns("selectDist"),
              label = span(
                `data-translate` = "ui_2dist",
                "Select distributions to fit"
              ),
              multiple = TRUE,
              choices = c(default.dists, extra.dists),
              selected = default.dists,
              options = list(
                "plugins" = list("remove_button")
              )
            ),
            checkboxInput(
              ns("rescale"),
              label = span(`data-translate` = "ui_2rescale", "Rescale"),
              value = FALSE
            ),
            div(
              class = "mt-3",
              actionButton(
                ns("updateFit"),
                label = tagList(
                  uiOutput(ns("update_icon")),
                  span(`data-translate` = "ui_update_fit", "Update Fit")
                ),
                class = "btn-primary w-100"
              )
            ),
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                title = span(
                  `data-translate` = "ui_3plotopts",
                  "Plot formatting options"
                ),
                value = "plot_format_fit",
                selected = FALSE,
                selectInput(
                  ns("selectUnit"),
                  label = span(`data-translate` = "ui_2unit", "Select units"),
                  choices = units(),
                  selected = units()[1]
                ),
                textInput(
                  ns("xaxis2"),
                  label = span(`data-translate` = "ui_3xlab", "X-axis label"),
                  value = "Concentration"
                ),
                textInput(
                  ns("yaxis2"),
                  label = span(`data-translate` = "ui_3ylab", "Y-axis label"),
                  value = "Species affected (%)"
                ),
                numericInput(
                  ns("size2"),
                  label = span(`data-translate` = "ui_size", "Text size"),
                  value = 12,
                  min = 1,
                  max = 100
                ),
                textInput(
                  ns("title"),
                  value = "",
                  label = span(`data-translate` = "ui_3title", "Title")
                )
              )
            )
          )
        ),
        div(
          class = "p-3",
          conditionalPanel(
            condition = paste_js('has_fit', ns),
            card(
              class = card_shadow,
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2plot", "Plot Fitted Distributions")
              ),
              card_body(
                ui_download_popover(ns = ns),
                htmlOutput(ns("fitFail")),
                plotOutput(ns("plotDist"))
              )
            ),
            card(
              class = card_shadow,
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2table", "Goodness of Fit")
              ),
              card_body(
                padding = 25,
                ui_download_popover_table(ns = ns),
                div(
                  class = "table-responsive",
                  DT::dataTableOutput(ns("tableGof"))
                )
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = paste0("!output['", ns("has_data"), "']"),
      ui_dashbox(span(
        `data-translate` = "ui_hintdata",
        "You have not added a dataset."
      ))
    )
  )
}

mod_fit_server <- function(
  id,
  translations,
  lang,
  data_mod,
  big_mark,
  decimal_mark,
  main_nav
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_data <- data_mod$has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    waiter_gof <- ui_waiter(id = "tableGof", ns = ns)
    waiter_distplot <- ui_waiter(id = "plotDist", ns = ns)

    needs_update <- reactiveVal(FALSE)

    fit_trigger <- reactiveVal(0)

    # trigger if navigate to fit tab
    observe({
      if (main_nav() == "fit") {
        current_val <- isolate(fit_trigger())
        fit_trigger(current_val + 1)
      }
    }) %>%
      bindEvent(main_nav())

    # Also increment when manual update is needed
    observe({
      needs_update(FALSE)
      current_val <- isolate(fit_trigger())
      fit_trigger(current_val + 1)
    }) %>%
      bindEvent(input$updateFit)

    # Auto-update for critical changes
    observe({
      needs_update(FALSE)
      if (isolate(main_nav()) == "fit") {
        current_val <- isolate(fit_trigger())
        fit_trigger(current_val + 1)
      }
    }) %>%
      bindEvent(input$selectConc, data_mod$data(), ignoreInit = TRUE)

    # monitor if out of date
    observe({
      needs_update(TRUE)
    }) %>%
      bindEvent(input$selectDist, input$rescale)

    fit_dist <- reactive({
      req(fit_trigger() > 0)
      req(main_nav() == "fit")
      req(data_mod$data())
      req(input$selectConc)
      req(input$selectDist)
      req(iv$is_valid())

      waiter_gof$show()
      waiter_distplot$show()

      data <- data_mod$data()
      conc <- make.names(input$selectConc)
      dists <- input$selectDist
      rescale <- input$rescale

      safe_try(ssdtools::ssd_fit_bcanz(
        data,
        left = conc,
        dists = dists,
        silent = TRUE,
        rescale = rescale
      ))
    }) %>%
      bindCache(
        input$selectConc,
        input$selectDist,
        input$rescale,
        data_mod$data()
      ) %>%
      bindEvent(fit_trigger())

    # Dynamic icon for update button
    output$update_icon <- renderUI({
      if (needs_update()) {
        icon("refresh", class = paste(color_button_icon, "me-1"))
      } else {
        icon("check-circle", class = paste(color_button_icon, "me-1"))
      }
    }) %>%
      bindEvent(needs_update())

    observe({
      data <- data_mod$clean_data()
      choices <- names(data)
      selected <- guess_conc(choices, data)
      if (is.na(selected)) {
        selected <- choices[1]
      }
      updateSelectInput(
        session,
        "selectConc",
        choices = choices,
        selected = selected
      )
    }) %>%
      bindEvent(data_mod$clean_data())

    observe({
      toxicant_name <- data_mod$toxicant_name()
      if (!is.null(toxicant_name) && toxicant_name != "") {
        updateTextInput(
          session,
          "title",
          value = toxicant_name
        )
      }
    }) %>%
      bindEvent(data_mod$toxicant_name())

    observe({
      trans <- translations()
      updateTextInput(session, "yaxis2", value = tr("ui_2ploty", trans))
    }) %>%
      bindEvent(translations())

    # validation --------------------------------------------------------------
    iv <- InputValidator$new()

    iv$add_rule("selectConc", function(value) {
      trans <- translations()
      dat <- data_mod$data()

      conc_data <- dat[[value]]

      if (!has_numeric_concentration(conc_data)) {
        return(as.character(tr("ui_hintnum", trans)[1]))
      }
      if (!has_no_missing_concentration(conc_data)) {
        return(as.character(tr("ui_hintmiss", trans)[1]))
      }
      if (!has_positive_concentration(conc_data)) {
        return(as.character(tr("ui_hintpos", trans)[1]))
      }
      if (!has_finite_concentration(conc_data)) {
        return(as.character(tr("ui_hintfin", trans)[1]))
      }
      if (!has_min_concentration(conc_data)) {
        return(as.character(tr("ui_hint6", trans)[1]))
      }
      if (!has_not_all_identical(conc_data)) {
        return(as.character(tr("ui_hintident", trans)[1]))
      }

      NULL
    })

    iv$add_rule("selectDist", function(value) {
      trans <- translations()
      if (is.null(value) || length(value) == 0) {
        return(as.character(tr("ui_hintdist", trans)[1]))
      }
      NULL
    })

    iv$enable()

    # fit reactives and outputs -----------------------------------------------
    plot_dist <- reactive({
      dist <- fit_dist()
      req(dist)

      plot_distributions(
        dist,
        ylab = input$yaxis2,
        xlab = append_unit(input$xaxis2, input$selectUnit),
        text_size = input$size2,
        big.mark = big_mark(),
        decimal.mark = decimal_mark(),
        title = input$title
      )
    })

    table_gof <- reactive({
      dist <- fit_dist()
      req(dist)

      trans <- translations()
      gof <-
        ssdtools::ssd_gof(dist, wt = TRUE) %>%
        # Remove at_bound and computable columns
        dplyr::select(-at_bound, -computable) %>%
        # Round different columns to different sig figs
        dplyr::mutate(
          dplyr::across(c(log_lik, aic, aicc, bic), ~ signif(.x, 4))
        ) %>%
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
        dplyr::arrange(dplyr::desc(.data$wt))
      names(gof) <- gsub("weight", tr("ui_2weight", trans), names(gof))
      gof
    })

    # render plot and table - waiter stops when plot and table ready
    render_status <- reactiveValues(plot_ready = FALSE, table_ready = FALSE)

    observe({
      render_status$plot_ready <- FALSE
      render_status$table_ready <- FALSE
    }) %>%
      bindEvent(fit_dist())

    output$plotDist <- renderPlot(
      {
        result <- plot_dist()
        render_status$plot_ready <- TRUE
        result
      },
      alt = reactive({
        switch(
          lang(),
          "french" = "Graphique de distribution de sensibilit\u00e9 des esp\u00e8ces montrant les courbes de distribution ajust\u00e9es superpos\u00e9es aux donn\u00e9es de concentration observ\u00e9es pour chaque esp\u00e8ce. L'axe des x indique les valeurs de concentration et l'axe des y indique la proportion des esp\u00e8ces affect\u00e9es.",
          "spanish" = "Gr\u00e1fico de distribuci\u00f3n de sensibilidad de especies que muestra curvas de distribuci\u00f3n ajustadas superpuestas a los datos de concentraci\u00f3n de especies observados. El eje x muestra los valores de concentraci\u00f3n y el eje y muestra la proporci\u00f3n de especies afectadas.",
          "Species Sensitivity Distribution plot showing fitted distribution curves overlaid on observed species concentration data. The x-axis shows concentration values and the y-axis shows the proportion of species affected."
        )
      })
    )

    output$tableGof <- DT::renderDataTable({
      gof <- table_gof()
      trans <- translations()
      header_tooltips <- gof_header_tooltips(trans, lang())

      result <- DT::datatable(
        gof,
        options = list(
          dom = "t",
          processing = FALSE,
          autoWidth = FALSE,
          deferRender = TRUE,
          headerCallback = dt_header_tooltip_callback(header_tooltips)
        )
      )

      result <- dt_weight_color_bar(result, gof, trans)

      render_status$table_ready <- TRUE
      result
    })

    observe({
      if (render_status$plot_ready && render_status$table_ready) {
        waiter_distplot$hide()
        waiter_gof$hide()
      }
    }) %>%
      bindEvent(render_status$plot_ready, render_status$table_ready)

    # Notify when failed fits
    fit_fail <- reactive({
      dist <- fit_dist()
      paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
    }) %>%
      bindEvent(fit_dist())

    output$fitFail <- renderText({
      failed <- fit_fail()
      req(failed != "")
      HTML(paste0(
        "<font color='grey'>",
        paste(
          failed,
          tr("ui_hintfail", translations())
        ),
        "</font>"
      ))
    }) %>%
      bindEvent(fit_fail())

    # download handlers -------------------------------------------------------
    output$fitDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_distFitPlot.png"
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = plot_dist(),
          device = "png",
          width = input$width,
          height = input$height,
          dpi = input$dpi
        )
      }
    )

    output$fitDlRds <- downloadHandler(
      filename = function() {
        "ssdtools_fit_plot.rds"
      },
      content = function(file) {
        saveRDS(plot_dist(), file = file)
      }
    )

    output$fitDlCsv <- downloadHandler(
      filename = function() {
        "ssdtools_gof_table.csv"
      },
      content = function(file) {
        readr::write_csv(dplyr::as_tibble(table_gof()), file)
      }
    )

    output$fitDlXlsx <- downloadHandler(
      filename = function() {
        "ssdtools_gof_table.xlsx"
      },
      content = function(file) {
        writexl::write_xlsx(dplyr::as_tibble(table_gof()), file)
      }
    )

    # return values ------------------------------------------------------------
    has_fit <- reactive({
      iv$is_valid() &&
        !is.null(fit_dist()) &&
        !inherits(fit_dist(), "try-error")
    }) %>%
      bindEvent(fit_dist(), iv$is_valid())

    output$has_fit <- has_fit
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)

    return(
      list(
        fit_dist = fit_dist,
        fit_plot = plot_dist,
        gof_table = table_gof,
        big_mark = big_mark,
        decimal_mark = decimal_mark,
        conc_column = reactive({
          input$selectConc
        }),
        units = reactive({
          input$selectUnit
        }),
        dists = reactive({
          input$selectDist
        }),
        rescale = reactive({
          input$rescale
        }),
        yaxis_label = reactive({
          input$yaxis2
        }),
        xaxis_label = reactive({
          input$xaxis2
        }),
        text_size = reactive({
          input$size2
        }),
        title = reactive({
          input$title
        }),
        has_fit = has_fit,
        width = reactive({
          input$width
        }),
        height = reactive({
          input$height
        }),
        dpi = reactive({
          input$dpi
        })
      )
    )
  })
}
