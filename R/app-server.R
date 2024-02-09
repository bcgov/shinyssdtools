# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
app_server <- function(input, output, session) {
  ########### Reactives --------------------
  # --- upload data
  translation.value <- reactiveValues(
    lang = "English"
  )
  observeEvent(input$fr, {
    translation.value$lang <- "French"
  })
  observeEvent(input$en, {
    translation.value$lang <- "English"
  })

  trans <- reactive({
    if (translation.value$lang == "English") {
      translations$trans <- translations$english
      return(translations)
    }
    translations$trans <- translations$french
    translations
  })

  tr <- function(id, trans) {
    trans$trans[trans$id == id]
  }

  # --- upload data
  upload.values <- reactiveValues(
    upload_state = NULL
  )

  #  read/create handson table
  hot.values <- reactiveValues()
  hot_data <- reactive({
    if (!is.null(input$hot)) {
      DF <- rhandsontable::hot_to_r(input$hot)
      DF <- dplyr::mutate_if(DF, is.factor, as.character)
    } else {
      if (is.null(hot.values[["DF"]])) {
        DF <- data.frame(
          Concentration = rep(NA_real_, 10),
          Species = rep(NA_character_, 10),
          Group = rep(NA_character_, 10)
        )
      } else {
        DF <- hot.values[["DF"]]
      }
    }
    hot.values[["DF"]] <- DF
    DF
  })

  # read whichever dataset method user chooses
  read_data <- reactive({
    req(upload.values$upload_state)
    if (upload.values$upload_state == "upload") {
      data <- input$uploadData
      if (!grepl(".csv", data$name, fixed = TRUE)) {
        Sys.sleep(1)
        return(p("We're not sure what to do with that file type. Please upload a csv."))
      }
      return(readr::read_csv(data$datapath))
    } else if (upload.values$upload_state == "demo") {
      return(boron.data)
    } else if (upload.values$upload_state == "hot") {
      return(hot_data())
    }
  })

  clean_data <- reactive({
    data <- read_data()
    if (length(data)) {
      # remove any column names like X1, X2 (blank headers from excel/numbers)
      data[, colnames(data) %in% paste0("X", 1:200)] <- NULL
      # remove any rows with all NA
      data <- data[!(rowSums(is.na(data) | data == "") == ncol(data)), ]
    }
    data
  })

  # deal with unacceptable column names
  names_data <- reactive({
    data <- clean_data()
    names(data) %<>% make.names
    data
  })

  # --- Checks and hints for solving problems
  check_fit <- reactive({
    req(input$selectConc)
    req(input$selectDist)

    conc <- input$selectConc
    dist <- input$selectDist
    data <- clean_data()

    if (length(data[[conc]]) == 0L) {
      return(tr("ui_hintdata", trans()))
    }
    if (!is.numeric(data[[conc]])) {
      return(tr("ui_hintnum", trans()))
    }
    if (any(is.na(data[[conc]]))) {
      return(tr("ui_hintmiss", trans()))
    }
    if (any(data[[conc]] <= 0)) {
      return(tr("ui_hintpos", trans()))
    }
    if (any(is.infinite(data[[conc]]))) {
      return(tr("ui_hintfin", trans()))
    }
    if (zero_range(data[[conc]])) {
      return(tr("ui_hintident", trans()))
    }
    if (length(data[[conc]]) < 6) {
      return(tr("ui_hint6", trans()))
    }
    if (is.null(dist)) {
      return(tr("ui_hintdist", trans()))
    }
    ""
  })

  output$checkfit <- reactive({
    check_fit() != ""
  })
  outputOptions(output, "checkfit", suspendWhenHidden = FALSE)

  check_pred <- reactive({
    data <- clean_data()
    if ("Concentration" %in% names(data) && length(data[["Concentration"]]) == 0L) {
      return(tr("ui_hintdata", trans()))
    }
    if (is.null(input$selectConc)) {
      return(tr("ui_hintpred", trans()))
    }
    if (check_fit() != "") {
      return(tr("ui_hintfit", trans()))
    }
    ""
  })

  output$checkpred <- reactive({
    check_pred() != ""
  })
  outputOptions(output, "checkpred", suspendWhenHidden = FALSE)

  output$hintFi <- renderText(hint(check_fit()))
  output$hintPr <- renderText(hint(check_pred()))

  # --- render column choices
  column_names <- reactive({
    names(clean_data())
  })

  guess_conc <- reactive({
    name <- column_names()
    name[grepl("conc", name %>% tolower())][1]
  })

  guess_spp <- reactive({
    name <- column_names()
    name[grepl("sp", name %>% tolower())][1]
  })

  code_label <- reactive({
    if (input$selectLabel == "-none-") {
      return("NULL")
    }
    paste0("'", input$selectLabel %>% make.names(), "'")
  })

  code_colour <- reactive({
    if (input$selectColour == "-none-") {
      return("NULL")
    }
    paste0("'", input$selectColour %>% make.names(), "'")
  })

  code_shape <- reactive({
    if (input$selectShape == "-none-") {
      return("NULL")
    }
    paste0("'", input$selectShape %>% make.names(), "'")
  })

  code_hc <- reactive({
    if (!input$checkHc) {
      return("NULL")
    }
    thresh_rv$percent / 100
  })

  # --- get values
  get_width <- reactive({
    ifelse(input$selectWidth == 0, 1, input$selectWidth)
  })

  get_width2 <- reactive({
    ifelse(input$selectWidth2 == 0, 1, input$selectWidth2)
  })

  get_height <- reactive({
    ifelse(input$selectHeight == 0, 1, input$selectHeight)
  })

  get_height2 <- reactive({
    ifelse(input$selectHeight2 == 0, 1, input$selectHeight2)
  })

  get_dpi <- reactive({
    if (input$selectDpi > 3000) {
      return(3000)
    }
    if (input$selectDpi == 0) {
      return(1)
    }
    input$selectDpi
  })

  get_dpi2 <- reactive({
    if (input$selectDpi2 > 3000) {
      return(3000)
    }
    if (input$selectDpi2 == 0) {
      return(1)
    }
    input$selectDpi2
  })

  # --- fit distributions
  fit_dist <- reactive({
    req(input$selectConc)
    req(input$selectDist)
    req(check_fit() == "")
    data <- names_data()
    conc <- input$selectConc %>% make.names()
    dist <- input$selectDist
    x <- try(ssdtools::ssd_fit_dists(data,
      left = conc,
      dists = input$selectDist,
      silent = TRUE,
      reweight = FALSE,
      min_pmix = 0,
      nrow = 6L,
      computable = input$computable,
      # need to get inverse of at_boundary_ok value due to wording of label
      at_boundary_ok = !input$at_boundary_ok,
      rescale = input$rescale
    ), silent = TRUE)
    if (inherits(x, "try-error")) {
      x <- NULL
    }
    x
  })

  plot_dist <- reactive({
    dist <- fit_dist()
    plot_distributions(dist, ylab = input$yaxis2, xlab = input$xaxis2, text_size = input$size2)
  })

  table_gof <- reactive({
    req(fit_dist())
    dist <- fit_dist()
    gof <- 
      ssdtools::ssd_gof(dist) %>%
      dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
      dplyr::arrange(dplyr::desc(.data$weight))
    names(gof) <- gsub("weight", tr("ui_2weight", trans()), names(gof))
    gof
  })

  fit_fail <- reactive({
    req(input$selectDist)
    dist <- fit_dist()
    x <- paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
    x
  })

  # --- predict and model average
  predict_hc <- reactive({
    dist <- fit_dist()
    stats::predict(dist, nboot = 10, ci = FALSE)
  })

  transformation <- reactive({
    trans <- "log10"
    if (!input$xlog) {
      trans <- "identity"
    }
    trans
  })

  plot_model_average <- reactive({
    req(input$thresh)
    req(input$selectColour)
    req(input$selectLabel)
    req(input$selectShape)
    req(input$selectConc)
    req(input$thresh_type)
    req(input$adjustLabel)
    req(thresh_rv$percent)
    req(input$xbreaks)

    data <- names_data()
    pred <- predict_hc()
    conc <- input$selectConc %>% make.names()
    colour <- if (input$selectColour == "-none-") {
      NULL
    } else {
      input$selectColour %>% make.names()
    }
    label <- if (input$selectLabel == "-none-") {
      NULL
    } else {
      input$selectLabel %>% make.names()
    }
    shape <- if (input$selectShape == "-none-") {
      NULL
    } else {
      input$selectShape %>% make.names()
    }
    percent <- if (!input$checkHc || is.null(thresh_rv$percent)) {
      NULL
    } else {
      round(thresh_rv$percent)
    }

    shape_data <- if (is.null(shape)) {
      NULL
    } else {
      data[[shape]]
    }

    validate(need(is.null(shape_data) | is.character(shape_data) | is.factor(shape_data), message = tr("ui_hintsym", trans())))

    shift_label <- input$adjustLabel
    if (shift_label < 1) {
      shift_label <- 1
    }

    xmax <- NA
    if (!is.null(input$xMax)) {
      xmax <- input$xMax
    }

    xmin <- NA
    if (!is.null(input$xMin)) {
      xmin <- input$xMin
    }

    trans <- transformation()

    silent_plot(plot_predictions(data, pred,
      conc = conc, label = label, colour = colour,
      shape = shape, percent = percent, xbreaks = as.numeric(input$xbreaks),
      label_adjust = shift_label, xaxis = input$xaxis,
      yaxis = input$yaxis, title = input$title, xmax = xmax, xmin = xmin,
      palette = input$selectPalette, legend_colour = input$legendColour,
      legend_shape = input$legendShape, trans = trans, text_size = input$size3,
      label_size = input$sizeLabel3
    ))
  })

  # --- get confidence intervals
  table_cl <- eventReactive(input$getCl, {
    dist <- fit_dist()
    withProgress(value = 0, message = "Getting Confidence Limits...", {
      incProgress(0.4)
      nboot <- as.integer(gsub("(,|\\s)", "", input$bootSamp))
      if (input$thresh_type != "Concentration") {
        y <- ssd_hp_ave(dist, conc = thresh_rv$conc, nboot = nboot)
      } else {
        y <- ssd_hc_ave(dist, percent = thresh_rv$percent, nboot = nboot)
      }
      y
    })
  })

  estimate_time <- reactive({
    if (translation.value$lang == "English") {
      df <- data.frame(
        n = c("500", "1,000", "5,000", "10,000"),
        time = c("10 seconds", "20 seconds", "2 minutes", "5 minutes")
      )
    } else {
      df <- data.frame(
        n = c("500", "1 000", "5 000", "10 000"),
        time = c("10 secondes", "20 secondes", "2 minutes", "5 minutes")
      )
    }

    df[df$n == input$bootSamp, ]$time
  })

  ########### Outputs --------------------
  # --- datasets
  output$hot <- rhandsontable::renderRHandsontable({
    x <- hot_data()
    if (!is.null(x)) {
      rhandsontable::rhandsontable(x, width = 600, useTypes = FALSE)
    }
  })

  output$ui_viewupload <- renderUI({
    wellPanel(DT::DTOutput("viewUpload"),
      style = "overflow-x:scroll; max-height: 600px; max-width: 640px"
    )
  })

  output$viewUpload <- DT::renderDataTable({
    DT::datatable(read_data(), options = dtopt())
  })

  dtopt <- reactive({
    url <- paste0("//cdn.datatables.net/plug-ins/1.10.11/i18n/", translation.value$lang, ".json")
    list(
      language = list(url = url),
      pageLength = 10
    )
  })

  # --- render UI with choices based on file upload
  output$selectLabel <- renderUI({
    selectInput("selectLabel",
      label = tr("ui_3label", trans()),
      choices = c("-none-", column_names()),
      selected = guess_spp()
    )
  })

  output$selectColour <- renderUI({
    selectInput("selectColour",
      label = tr("ui_3colour", trans()),
      choices = c("-none-", column_names()),
      selected = "-none-"
    )
  })

  output$selectShape <- renderUI({
    selectInput("selectShape",
      label = tr("ui_3symbol", trans()),
      choices = c("-none-", column_names()),
      selected = "-none-"
    )
  })

  output$uiLegendColour <- renderUI({
    textInput("legendColour", label = tr("ui_3legend", trans()), value = input$selectColour)
  })

  output$uiLegendShape <- renderUI({
    textInput("legendShape", label = tr("ui_3shape", trans()), value = input$selectShape)
  })

  output$ui_3size <- renderUI({
    numericInput("size3", label = tr("ui_size", trans()), value = 12, min = 1, max = 100)
  })

  output$ui_3sizeLabel <- renderUI({
    numericInput("sizeLabel3", label = tr("ui_sizeLabel", trans()), value = 3, min = 1, max = 10)
  })

  output$ui_2size <- renderUI({
    numericInput("size2", label = tr("ui_size", trans()), value = 12, min = 1, max = 100)
  })

  output$ui_checkHc <- renderUI({
    checkboxInput("checkHc", label = tr("ui_checkHc", trans()), value = TRUE)
  })

  # --- render fit results ----
  output$distPlot1 <- renderPlot({
    waiter::waiter_show(id = "distPlot1", html = waiter::spin_2(), color = "white", hide_on_render = TRUE)
    plot_dist()
  })

  output$gofTable <- DT::renderDataTable({
    DT::datatable(table_gof(), options = list(dom = "t"))
  })

  output$fitFail <- renderText({
    req(fit_fail() != "")
    HTML(paste0("<font color='grey'>", paste(fit_fail(), tr("ui_hintfail", trans())), "</font>"))
  })
  # --- render predict results ----
  output$modelAveragePlot <- renderPlot({
    waiter::waiter_show(id = "modelAveragePlot", html = waiter::spin_2(), color = "white", hide_on_render = TRUE)
    plot_model_average()
  })

  output$estHc <- renderUI({
    req(input$thresh_type)
    percent <- thresh_rv$percent
    percent_pc <- 100 - as.numeric(percent)
    percent_bold <- paste0("<b>", thresh_rv$percent, "</b>")
    conc <- paste0("<b>", thresh_rv$conc, "</b>")
    if (input$thresh_type != "Concentration") {
      return(HTML(glue::glue(tr("ui_3hc2", trans()), percent = percent_bold, conc = conc)))
    }
    div(
      HTML(glue::glue("HC{percent}/PC{percent_pc}: {conc}", percent = percent, conc = conc)),
      br(),
      HTML(glue::glue(tr("ui_3hc", trans()), percent = percent_bold, conc = conc))
    )
  })

  output$clTable <- DT::renderDataTable({
    DT::datatable(table_cl(), options = list(dom = "t"))
  })

  output$describeCl <- renderText({
    desc1 <- paste(tr("ui_3cldesc1", trans()), paste0("<b>", thresh_rv$percent, "</b>"))
    if (input$thresh_type != "Concentration") {
      desc1 <- paste(tr("ui_3cldesc11", trans()), paste0("<b>", thresh_rv$conc, "</b>"))
    }
    HTML(
      desc1, tr("ui_3cldesc2", trans()),
      paste0("<b>", input$bootSamp, ".</b>"),
      tr("ui_3cldesc3", trans()),
      paste0("<b>", estimate_time(), "</b>"),
      tr("ui_3cldesc4", trans())
    )
  })

  # --- render UI ----
  shinyjs::onclick("linkFormatPredict", shinyjs::toggle("divFormatPredict", anim = TRUE, animType = "slide", time = 0.2))
  shinyjs::onclick("linkPngFormatPredict", shinyjs::toggle("divPngFormatPredict", anim = TRUE, animType = "slide", time = 0.2))
  shinyjs::onclick("linkFormatFit", shinyjs::toggle("divFormatFit", anim = TRUE, animType = "slide", time = 0.2))

  output$uiAdjustLabel <- renderUI({
    numericInput("adjustLabel",
      value = 1.05, label = tr("ui_adjustLabel", trans()),
      min = 0, max = 10, step = 0.1
    )
  })

  output$uiXmax <- renderUI({
    numericInput("xMax", label = tr("ui_xmax", trans()), min = 1, value = NULL)
  })

  output$uiXmin <- renderUI({
    numericInput("xMin", label = tr("ui_xmin", trans()), min = 1, value = NULL)
  })

  output$uiXlog <- renderUI({
    checkboxInput("xlog", tr("ui_xlog", trans()), value = TRUE)
  })

  output$uiXbreaks <- renderUI({
    req(names_data())
    req(thresh_rv$conc)
    data <- names_data()
    conc <- input$selectConc %>% make.names()

    scale <- scales::trans_breaks("log10", function(x) 10^x)
    y <- sort(signif(c(scale(data[[conc]]), thresh_rv$conc), 3))

    selectizeInput("xbreaks", tr("ui_xbreaks", trans()),
      options = list(create = TRUE, plugins = list("remove_button")),
      choices = y,
      selected = y,
      multiple = TRUE
    )
  })

  # --- download handlers ----
  output$dlFitPlot <- downloadHandler(
    filename = function() {
      "ssdtools_distFitPlot.png"
    },
    content = function(file) {
      ggplot2::ggsave(file,
        plot = plot_dist(), device = "png",
        width = get_width2(), height = get_height2(), dpi = get_dpi2()
      )
    }
  )

  output$dlPredPlot <- downloadHandler(
    filename = function() {
      "ssdtools_modelAveragePlot.png"
    },
    content = function(file) {
      ggplot2::ggsave(file,
        plot = plot_model_average(), device = "png",
        width = get_width(), height = get_height(), dpi = get_dpi()
      )
    }
  )

  output$dlFitRds <- downloadHandler(
    filename = function() {
      "ssdtools_distFitPlot.rds"
    },
    content = function(file) {
      saveRDS(plot_dist(), file = file)
    }
  )

  output$dlPredRds <- downloadHandler(
    filename = function() {
      "ssdtools_modelAveragePlot.rds"
    },
    content = function(file) {
      saveRDS(plot_model_average(), file = file)
    }
  )

  output$dlFitTable <- downloadHandler(
    filename = function() {
      "ssdtools_distGofTable.csv"
    },
    content <- function(file) {
      readr::write_csv(table_gof() %>% dplyr::as_tibble(), file)
    }
  )

  output$dlPredTable <- downloadHandler(
    filename = function() {
      "ssdtools_predictTable.csv"
    },
    content <- function(file) {
      if (!is.null(table_cl())) {
        return(readr::write_csv(table_cl() %>% dplyr::as_tibble(), file))
      } else {
        return()
      }
    }
  )

  # --- render code ----
  output$codeHead <- renderUI({
    if (upload.values$upload_state == "hot" && is.na(read_data()$Concentration[1])) {
      return()
    }
    l1 <- "install.packages('ssdtools')"
    l2 <- "library(ssdtools)"
    l3 <- "library(ggplot2)"
    l4 <- "library(dplyr)"
    if (upload.values$upload_state == "upload") {
      l5 <- "library(readr)"
    } else {
      l5 <- NULL
    }
    HTML(paste(l1, l2, l3, l4, l5, sep = "<br/>"))
  })

  output$codeData <- renderUI({
    c1 <- "# read dataset"
    c2 <- "# the file argument of read_csv() assumes the file is in your working directory. You may need to change the file path to correctly read your dataset."
    c4 <- "# this is the output of dput, which is used to create a data.frame from data entered in interactive spreadsheet"
    hot <- paste0("data <- ", utils::capture.output(dput(clean_data())) %>% glue::glue_collapse())
    upload <- paste0("data <- read_csv(file = '", input$uploadData$name, "')")
    demo <- "data <- ssddata::ccme_boron"
    c3 <- "# fix unacceptable column names"
    name <- "colnames(data) <- make.names(colnames(data))"
    if (upload.values$upload_state == "hot") {
      return(HTML(paste(c1, c4, hot, c3, name, sep = "<br/>")))
    }
    if (upload.values$upload_state == "upload") {
      return(HTML(paste(c1, c2, upload, c3, name, sep = "<br/>")))
    }
    if (upload.values$upload_state == "demo") {
      return(HTML(paste(c1, demo, c3, name, sep = "<br/>")))
    }
  })

  output$codeFit <- renderUI({
    req(check_fit() == "")
    ylab <- input$yaxis2
    xlab <- input$xaxis2
    text_size <- input$size2
    c1 <- "# fit distributions"
    fit <- paste0(
      "dist <- ssd_fit_dists(data, left = '",
      input$selectConc %>% make.names(),
      "', dists = c(",
      paste0("'", input$selectDist, "'", collapse = ", "), ")",
      ", silent = TRUE, reweight = FALSE, min_pmix = 0, nrow = 6L, computable = ",
      input$computable,
      ", at_boundary_ok = ", !input$at_boundary_ok,
      ", rescale = ", input$rescale, ")"
    )
    c2 <- "# plot distributions"
    plot <- paste0(
      "ssd_plot_cdf(dist, ylab = '", ylab, "', xlab = '", xlab, "', delta = Inf) +
                   <br/> theme_classic() + <br/> ",
      "theme(axis.text = ggplot2::element_text(color = 'black', size = ", text_size, "), <br/>
          axis.title = ggplot2::element_text(size = ", text_size, "), <br/>
          legend.text = ggplot2::element_text(size = ", text_size, "), <br/>
          legend.title = ggplot2::element_text(size = ", text_size, ")) <br/>"
    )

    c3 <- "# goodness of fit table"
    table <- "ssd_gof(dist) %>% dplyr::mutate_if(is.numeric, ~ signif(., 3))"
    HTML(paste(c1, fit, c2, plot, c3, table, sep = "<br/>"))
  })

  output$codePredPlot <- renderUI({
    req(check_fit() == "")
    req(check_pred() == "")
    req(input$selectLabel)
    xmax <- ifelse(is.null(input$xMax), "NA", input$xMax)
    xmin <- ifelse(is.null(input$xMin), "NA", input$xMin)
    legend.colour <- ifelse(is.null(input$legendColour), "NULL", paste0("'", input$legendColour, "'"))
    legend.shape <- ifelse(is.null(input$legendShape) || input$legendShape == "-none-", "NULL", paste0("'", input$legendShape, "'"))
    text_size <- input$size3
    xlab <- input$xaxis
    ylab <- input$yaxis
    title <- input$title
    trans <- transformation()
    xbreaks <- input$xbreaks
    c1 <- "# plot model average"
    c2 <- "# to add confidence intervals set ci = TRUE in predict and ssd_plot"
    c3 <- "# we recommend using nboot = 10000 in predict, although this may take several minutes to run"
    pred <- "pred <- predict(dist, nboot = 10L, ci = FALSE)"
    plot <- paste0(
      "ssd_plot(data, pred, left = '", input$selectConc %>% make.names(),
      "', label = ", code_label(),
      ", color = ", code_colour(),
      ", shape = ", code_shape(),
      ", hc = ", code_hc(),
      ", ci = FALSE, <br/>shift_x = ", input$adjustLabel,
      ", ylab = '", ylab,
      "') + <br/> ggtitle('", title,
      "') + <br/> theme_classic() + <br/>",
      "theme(axis.text = ggplot2::element_text(color = 'black', size = ", text_size, "), <br/>
          axis.title = ggplot2::element_text(size = ", text_size, "), <br/>
          legend.text = ggplot2::element_text(size = ", text_size, "), <br/>
          legend.title = ggplot2::element_text(size = ", text_size, ")) + <br/>",
      "coord_trans(x = '", trans, "') + <br/>",
      "scale_x_continuous(name = '", xlab, "', breaks = c(", paste(xbreaks, collapse = ", "), "), limits = c(", xmin, ", ", xmax, "), labels = comma_signif) + <br/>",
      "scale_color_brewer(palette = '", input$selectPalette, "', name = ", legend.colour, ") +<br/>
                     scale_shape(name = ", legend.shape, ")"
    )
    HTML(paste(c1, c2, c3, pred, plot, sep = "<br/>"))
  })

  output$codePredCl <- renderUI({
    req(input$getCl)
    req(check_fit() == "")
    req(check_pred() == "")
    form <- "ssd_hc"
    arg <- "proportion"
    thresh <- thresh_rv$percent / 100
    if (input$thresh_type != "Concentration") {
      form <- "ssd_hp"
      arg <- "conc"
      thresh <- thresh_rv$conc
    }
    c1 <- "# get confidence limits"
    c2 <- paste("# use the nboot argument in", form, "to set the number of bootstrap samples")
    conf <- paste0(
      paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE",
      ", nboot = ", input$bootSamp %>% gsub(",", "", .) %>% as.integer(), "L, min_pboot = 0.8, multi_est = TRUE, multi_ci = FALSE)"
    )
    conf2 <- paste0(
      paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE, average = FALSE",
      ", nboot = ", input$bootSamp %>% gsub(",", "", .) %>% as.integer(), "L, min_pboot = 0.8, multi_est = TRUE, multi_ci = FALSE)"
    )
    bind <- paste0("dplyr::bind_rows(", conf, ", ", conf2, ")")
    HTML(paste(c1, c2, bind, sep = "<br/>"))
  })

  output$codeSaveFit <- renderUI({
    req(check_fit() == "")
    c1 <- "# save plot"
    c2 <- "# width and height are in inches, dpi (dots per inch) sets resolution"
    save <- paste0(
      "ggsave('fit_dist_plot.png',
                    width = ", get_width2(),
      " , height = ", get_height2(),
      " , dpi = ", get_dpi2(),
      ")"
    )
    HTML(paste(c1, c2, save, sep = "<br/>"))
  })

  output$codeSavePred <- renderUI({
    req(check_fit() == "")
    req(check_pred() == "")
    req(input$selectLabel)
    c1 <- "# save plot"
    c2 <- "# width and height are in inches, dpi (dots per inch) sets resolution"
    save <- paste0(
      "ggsave('model_average_plot.png',
                    width = ", get_width(),
      " , height = ", get_height(),
      " , dpi = ", get_dpi(),
      ")"
    )
    HTML(paste(c1, c2, save, sep = "<br/>"))
  })

  ########### Observers --------------------
  # --- info
  observeEvent(input$infoCl, {
    shinyjs::toggle("clInfoText", anim = FALSE, animType = "slide", time = 0.2)
  })

  observeEvent(input$infoUpload, {
    shinyjs::toggle("infoUploadText", anim = TRUE, animType = "slide", time = 0.2)
  })

  observeEvent(input$infoDemo, {
    shinyjs::toggle("infoDemoText", anim = TRUE, animType = "slide", time = 0.2)
  })

  observeEvent(input$infoHands, {
    shinyjs::toggle("infoHandsText", anim = TRUE, animType = "slide", time = 0.2)
  })

  # --- user data
  observeEvent(input$uploadData, {
    upload.values$upload_state <- "upload"
  })

  observeEvent(input$demoData, {
    upload.values$upload_state <- "demo"
  })

  observeEvent(input$hot, {
    upload.values$upload_state <- "hot"
  })

  ###### download handlers -------
  output$ui_report_download <- renderUI({
    req(plot_model_average())
    tagList(
      textInput("toxicant", "Toxicant name"),
      shinyWidgets::dropdownButton(
        status = "primary",
        label = "Download Report",
        inline = TRUE,
        circle = FALSE,
        icon = icon("download"),
        dl_button("dl_pdf", "PDF file"),
        dl_button("dl_html", "HTML file"),
        dl_button("dl_rmd", "Rmd file")
      )
    )
  })

  output$dl_rmd <- downloadHandler(
    filename = "bcanz_report.Rmd",
    content = function(file) {
      file.copy(
        system.file(package = "shinyssdtools", "extdata/bcanz_report.Rmd"),
        file
      )
    }
  )

  params_list <- reactive({
    req(plot_model_average())
    toxicant <- input$toxicant
    data <- names_data()
    dists <- input$selectDist
    fit_plot <- plot_dist()
    fit_dist <- fit_dist()
    gof_table <- table_gof()
    model_average_plot <- plot_model_average()
    nboot <- as.integer(gsub("(,|\\s)", "", input$bootSamp))
    params <- list(
      toxicant = toxicant, data = data, dists = dists,
      fit_plot = fit_plot, fit_dist = fit_dist, gof_table = gof_table,
      model_average_plot = model_average_plot, nboot = nboot
    )
    params
  })

  output$dl_pdf <- downloadHandler(
    filename = "bcanz_report.pdf",
    content = function(file) {
      withProgress(message = "Generating report ...", value = 0.5, {
        temp_report <- file.path(tempdir(), "bcanz_report.Rmd")
        file.copy(
          system.file(package = "shinyssdtools", "extdata/bcanz_report.Rmd"),
          temp_report
        )
        params <- params_list()
        rmarkdown::render(temp_report,
          output_format = "pdf_document",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "utf-8"
        )
      })
    }
  )

  output$dl_html <- downloadHandler(
    filename = "bcanz_report.html",
    content = function(file) {
      withProgress(message = "Generating report ...", value = 0.5, {
        temp_report <- file.path(tempdir(), "bcanz_report.Rmd")
        file.copy(
          system.file(package = "shinyssdtools", "extdata/bcanz_report.Rmd"),
          file
        )
        params <- params_list()
        rmarkdown::render(temp_report,
          output_format = "html_document",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "utf-8"
        )
      })
    }
  )

  ########### Render UI Translations -------------------
  output$ui_1choose <- renderUI({
    h4(tr("ui_1choose", trans()))
  })

  output$ui_navtitle <- renderUI({
    HTML(tr("ui_navtitle", trans()))
  })

  output$ui_nav1 <- renderUI({
    HTML(tr("ui_nav1", trans()))
  })

  output$ui_nav2 <- renderUI({
    HTML(tr("ui_nav2", trans()))
  })

  output$ui_nav3 <- renderUI({
    HTML(tr("ui_nav3", trans()))
  })

  output$ui_nav4 <- renderUI({
    HTML(tr("ui_nav4", trans()))
  })

  output$ui_navabout <- renderUI({
    HTML(tr("ui_navabout", trans()))
  })

  output$ui_navguide <- renderUI({
    HTML(tr("ui_navguide", trans()))
  })

  output$ui_1data <- renderUI({
    p(tr("ui_1data", trans()), actionLink("demoData", tr("ui_1data2", trans()), icon = icon("table")))
  })

  output$ui_1datahelp <- renderUI({
    tagList(
      helpText(tr("ui_1datahelp", trans())),
      helpText("Citation:"),
      tags$a("Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian  environmental  quality guidelines, 2009, Canadian Council of  Ministers of the Environment, Winnipeg.", href = "http://ceqg-rcqe.ccme.ca/download/en/324/")
    )
  })

  output$ui_1csv <- renderUI({
    p(tr("ui_1csv", trans()))
  })

  output$ui_1csvhelp <- renderUI({
    helpText(tr("ui_1csvhelp", trans()))
  })

  output$ui_1csvupload <- renderUI({
    fileInput("uploadData",
      buttonLabel = span(tagList(icon("upload"), "csv")),
      label = "", placeholder = tr("ui_1csvlabel", trans()),
      accept = c(".csv")
    )
  })

  output$ui_1table1 <- renderUI({
    p(tr("ui_1table", trans()))
  })

  output$ui_1tablehelp <- renderUI({
    helpText(tr("ui_1tablehelp", trans()))
  })

  output$ui_1preview <- renderUI({
    h4(tr("ui_1preview", trans()))
  })

  output$ui_1note1 <- renderUI({
    helpText(tr("ui_1note", trans()))
  })

  output$ui_2select <- renderUI({
    selectizeInput("selectDist",
      label = label_mandatory(tr("ui_2dist", trans())),
      multiple = TRUE,
      choices = c(default.dists, extra.dists),
      selected = default.dists,
      options = list(
        "plugins" = list("remove_button"),
        "create" = TRUE,
        "persist" = FALSE
      )
    )
  })

  output$ui_2png <- renderUI({
    actionLink("linkFormatFit", label = tr("ui_2png", trans()))
  })

  output$ui_2width <- renderUI({
    numericInput("selectWidth2", label = tr("ui_2width", trans()), min = 1, max = 20, step = 1, value = 8)
  })

  output$ui_2height <- renderUI({
    numericInput("selectHeight2", label = tr("ui_2height", trans()), min = 1, max = 20, step = 1, value = 6)
  })

  output$ui_2dpi <- renderUI({
    numericInput("selectDpi2", label = tr("ui_2dpi", trans()), min = 50, max = 3000, step = 50, value = 300)
  })

  output$selectConc <- renderUI({
    selectInput("selectConc",
      label = label_mandatory(tr("ui_2conc", trans())),
      choices = column_names(),
      selected = guess_conc()
    )
  })

  output$ui_2rescale <- renderUI({
    checkboxInput("rescale",
      label = tr("ui_2rescale", trans()),
      value = FALSE
    )
  })

  output$ui_2at_boundary_ok <- renderUI({
    checkboxInput("at_boundary_ok",
      label = tr("ui_2at_boundary_ok", trans()),
      value = TRUE
    )
  })

  output$ui_2computable <- renderUI({
    checkboxInput("computable",
      label = tr("ui_2computable", trans()),
      value = TRUE
    )
  })

  output$ui_2xlab <- renderUI({
    textInput("xaxis2", value = "Concentration", label = tr("ui_3xlab", trans()))
  })

  output$ui_2ylab <- renderUI({
    textInput("yaxis2", value = tr("ui_2ploty", trans()), label = tr("ui_3ylab", trans()))
  })

  output$ui_2plot <- renderUI({
    h4(tr("ui_2plot", trans()))
  })

  output$ui_2table <- renderUI({
    h4(tr("ui_2table", trans()))
  })

  output$ui_2dlplot <- renderUI({
    downloadButton("dlFitPlot",
      label = tr("ui_2dlplot", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_2dlrds <- renderUI({
    downloadButton("dlFitRds",
      label = tr("ui_2dlrds", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_2dltable <- renderUI({
    downloadButton("dlFitTable",
      label = tr("ui_2dltable", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_3est <- renderUI({
    h4(tr("ui_3est", trans()))
  })

  output$ui_3bshint <- renderUI({
    hint(tr("ui_3bshint", trans()))
  })

  output$ui_thresh_type <- renderUI({
    thresh_label <- tr("ui_3threshlabel", trans())
    thresh <- tr("ui_3thresh", trans())
    radioButtons("thresh_type", thresh_label,
      choices = c("Concentration", thresh),
      selected = "Concentration", inline = TRUE
    )
  })

  output$ui_3thresh <- renderUI({
    req(input$thresh_type)
    if (input$thresh_type != "Concentration") {
      return(numericInput("conc",
        label = "by concentration",
        value = 1, min = 0,
        max = 100, step = 0.1, width = "100px"
      ))
    }
    div(
      inline(selectizeInput("thresh",
        label = "affecting % species",
        choices = c(1, 5, 10, 20),
        options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
        selected = 5, width = "100px"
      )),
      inline(selectizeInput("thresh_pc",
        label = "protecting % species",
        choices = c(99, 95, 90, 80),
        options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
        selected = 95, width = "100px"
      ))
    )
  })

  observeEvent(input$thresh, {
    thresh_pc <- 100 - as.numeric(input$thresh)
    choices <- unique(c(99, 95, 90, 80, thresh_pc))
    updateSelectizeInput(session, "thresh_pc", choices = choices, selected = isolate(thresh_pc))
  })

  observeEvent(input$thresh_pc, {
    thresh <- 100 - as.numeric(input$thresh_pc)
    choices <- c(1, 5, 10, 20, thresh)
    updateSelectizeInput(session, "thresh", choices = choices, selected = isolate(thresh))
  })

  output$ui_3samples <- renderUI({
    choices <- c("500", "1 000", "5 000", "10 000")
    if (translation.value$lang == "English") {
      choices <- c("500", "1,000", "5,000", "10,000")
    }
    selectInput("bootSamp",
      label = tr("ui_3samples", trans()),
      choices = choices,
      selected = choices[2],
      width = "150px"
    )
  })

  thresh_rv <- reactiveValues(
    percent = NULL,
    conc = NULL
  )

  observe({
    x <- fit_dist()
    req(input$thresh_type)
    if (input$thresh_type != "Concentration") {
      req(input$conc)
      conc <- input$conc
      thresh <- signif(estimate_hp(x, conc), 3)
      if (thresh < 1 | thresh > 99) {
        return()
      }
      thresh_rv$conc <- conc
      thresh_rv$percent <- thresh
    } else {
      req(input$thresh)
      thresh <- as.numeric(input$thresh)
      thresh_rv$percent <- thresh
      conc <- signif(estimate_hc(x, thresh), 3)
      thresh_rv$conc <- conc
    }
  })

  output$ui_3plotopts <- renderUI({
    actionLink("linkFormatPredict", label = tr("ui_3plotopts", trans()))
  })

  output$ui_3pal <- renderUI({
    selectInput("selectPalette", label = tr("ui_3pal", trans()), choices = pals, selected = pals[2])
  })

  output$ui_3xlab <- renderUI({
    textInput("xaxis", value = "Concentration", label = tr("ui_3xlab", trans()))
  })

  output$ui_3ylab <- renderUI({
    textInput("yaxis", value = tr("ui_2ploty", trans()), label = tr("ui_3ylab", trans()))
  })

  output$ui_3title <- renderUI({
    textInput("title", value = "", label = tr("ui_3title", trans()))
  })

  output$ui_3pngopts <- renderUI({
    actionLink("linkPngFormatPredict", label = tr("ui_3pngopts", trans()))
  })

  output$ui_3width <- renderUI({
    inline(numericInput("selectWidth", label = tr("ui_3width", trans()), min = 1, max = 20, step = 1, value = 8))
  })

  output$ui_3height <- renderUI({
    inline(numericInput("selectHeight", label = tr("ui_3height", trans()), min = 1, max = 20, step = 1, value = 6))
  })

  output$ui_3dpi <- renderUI({
    inline(numericInput("selectDpi", label = tr("ui_3dpi", trans()), min = 50, max = 3000, step = 50, value = 600))
  })

  output$ui_3model <- renderUI({
    h4(tr("ui_3model", trans()))
  })

  output$ui_3dlplot <- renderUI({
    downloadButton("dlPredPlot",
      label = tr("ui_2dlplot", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_3dlrds <- renderUI({
    downloadButton("dlPredRds",
      label = tr("ui_2dlrds", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_3dltable <- renderUI({
    downloadButton("dlPredTable",
      label = tr("ui_2dltable", trans()),
      style = "padding:4px; font-size:80%"
    )
  })

  output$ui_3cl <- renderUI({
    h4(tr("ui_3cl", trans()))
  })

  output$ui_3help <- renderUI({
    helpText(tr("ui_3help", trans()))
  })

  output$ui_3clbutton <- renderUI({
    actionButton("getCl", label = tr("ui_3clbutton", trans()))
  })

  output$ui_4help <- renderUI({
    helpText(tr("ui_4help", trans()))
  })

  output$ui_5format <- renderUI({
    radioButtons("report_format", "Report format")
  })

  output$ui_about <- renderUI({
    ver <- paste("ssdtools version:", utils::packageVersion("ssdtools"))
    sver <- paste("shinyssdtools version:", utils::packageVersion("shinyssdtools"))
    if (translation.value$lang == "English") {
      return({
        tagList(
          p(ver),
          p(sver),
          includeMarkdown(system.file("extdata/about-en.md", package = "shinyssdtools"))
        )
      })
    } else {
      return({
        tagList(
          p(ver),
          p(sver),
          includeMarkdown(system.file("extdata/about-fr.md", package = "shinyssdtools"))
        )
      })
    }
  })

  output$ui_userguide <- renderUI({
    if (translation.value$lang == "English") {
      return(includeMarkdown(system.file(package = "shinyssdtools", "extdata/user-en.md")))
    }
    includeMarkdown(system.file(package = "shinyssdtools", "extdata/user-fr.md"))
  })
}
