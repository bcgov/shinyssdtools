function(input, output, session) {
  
  ########### Render UI Translations -------------------
  output$ui_1choose <- renderUI({
    h5(tr("ui_1choose"))
  })
  
  output$ui_navtitle <- renderUI({
    HTML(tr("ui_navtitle"))
  })
  
  output$ui_nav1 <- renderUI({
    HTML(tr("ui_nav1"))
  })
  
  output$ui_nav2 <- renderUI({
    HTML(tr("ui_nav2"))
  })
  
  output$ui_nav3 <- renderUI({
    HTML(tr("ui_nav3"))
  })
  
  output$ui_nav4 <- renderUI({
    HTML(tr("ui_nav4"))
  })
  
  output$ui_navabout <- renderUI({
    HTML(tr("ui_navabout"))
  })
  
  output$ui_navguide <- renderUI({
    HTML(tr("ui_navguide"))
  })
  
  output$ui_1data <- renderUI({
    p(tr("ui_1data"), actionLink("demoData", tr("ui_1data2"), icon = icon('table')))
  })
  
  output$ui_1datahelp <- renderUI({
    helpText(tr("ui_1datahelp"))
  })
  
  output$ui_1csv <- renderUI({
    p(tr("ui_1csv"))
  })
  
  output$ui_1csvhelp <- renderUI({
        helpText(tr("ui_1csvhelp"))
  })
  
  output$ui_1csvupload <- renderUI({
    fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
              label = "", placeholder = tr("ui_1csvlabel"),
              accept = c('.csv'))
  })
  
  output$ui_1table <- renderUI({
    p(tr("ui_1table"))
  })
  
  output$ui_1tablehelp <- renderUI({
    helpText(tr("ui_1tablehelp"))
  })
  
  output$ui_1preview <- renderUI({
    h5(tr("ui_1preview"))
  })
  
  output$ui_1note <- renderUI({
    helpText(tr("ui_1note"))
  })
  
  output$ui_2select <- renderUI({
    selectizeInput('selectDist',
                   label = label_mandatory(tr("ui_2dist")),
                   multiple = TRUE,
                   choices = c(default.dists, extra.dists),
                   selected = default.dists,
                   options = list(
                     'plugins' = list('remove_button'),
                     'create' = TRUE,
                     'persist' = FALSE))
  })

  output$ui_2png <- renderUI({
    actionLink('linkFormatFit', label = tr("ui_2png"))
  })

  output$ui_2width <- renderUI({
    numericInput('selectWidth2', label = tr("ui_2width"), min = 1, max = 20, step = 1, value = 8)
  })

  output$ui_2height <- renderUI({
    numericInput('selectHeight2', label = tr("ui_2height"), min = 1, max = 20, step = 1, value = 6)
    })

  output$ui_2dpi <- renderUI({
    numericInput('selectDpi2', label = tr("ui_2dpi"), min = 50, max = 3000, step = 50, value = 300)
    })

  output$selectConc = renderUI({
    selectInput("selectConc",
                label = label_mandatory(tr("ui_2conc")),
                choices = column_names(),
                selected = guess_conc())
  })

  output$ui_2plot <- renderUI({
    h5(tr("ui_2plot"))
  })

  output$ui_2table <- renderUI({
    h5(tr("ui_2table"))
  })

  output$ui_2dlplot <- renderUI({
      downloadButton("dlFitPlot", label = tr("ui_2dlplot"),
                     style = 'padding:4px; font-size:80%')
  })

  output$ui_2dltable <- renderUI({
      downloadButton("dlFitTable", label = tr("ui_2dltable"),
                     style = 'padding:4px; font-size:80%')
  })

  output$ui_3est <- renderUI({
    h5(tr("ui_3est"))
  })

  output$ui_3bshint <- renderUI({
    hint(tr("ui_3bshint"))
  })

  output$ui_3thresh <- renderUI({
    numericInput("selectHc", label = tr("ui_3thresh"), value = 5, min = 0,
                 max = 99, step = 5, width = "100px")
  })

  output$ui_3samples <- renderUI({
    selectInput('bootSamp', label = tr("ui_3samples"),
                choices = c("500", "1,000", "5,000", "10,000"),
                selected = "10,000",
                width = "130px")
  })

  output$ui_3plotopts <- renderUI({
    actionLink('linkFormatPredict', label = tr("ui_3plotopts"))
  })

  output$ui_3pal <- renderUI({
    selectInput('selectPalette', label = tr("ui_3pal"), choices = pals, selected = pals[2])
  })

  output$ui_3xlab <- renderUI({
    textInput('xaxis', value = "Concentration", label = tr("ui_3xlab"))
  })

  output$ui_3ylab <- renderUI({
    textInput('yaxis', value = "Percent of Species Affected", label = tr("ui_3ylab"))
  })

  output$ui_3title <- renderUI({
    textInput('title', value = "", label = tr("ui_3title"))
  })

  output$ui_3pngopts <- renderUI({
    actionLink('linkPngFormatPredict', label = tr("ui_3pngopts"))
  })

  output$ui_3width <- renderUI({
    inline(numericInput('selectWidth', label = tr("ui_3width"), min = 1, max = 20, step = 1, value = 8))
  })

  output$ui_3height <- renderUI({
    inline(numericInput('selectHeight', label = tr("ui_3height"), min = 1, max = 20, step = 1, value = 6))
  })

  output$ui_3dpi <- renderUI({
    inline(numericInput('selectDpi', label = tr("ui_3dpi"), min = 50, max = 3000, step = 50, value = 600))
  })

  output$ui_3model <- renderUI({
    h5(tr("ui_3model"))
  })

  output$ui_3dlplot <- renderUI({
    downloadButton("dlPredPlot", label = tr("ui_2dlplot"),
                   style = 'padding:4px; font-size:80%')
  })

  output$ui_3dltable <- renderUI({
    downloadButton("dlPredTable", label = tr("ui_2dltable"),
                   style = 'padding:4px; font-size:80%')
  })

  output$ui_3cl <- renderUI({
    h5(tr("ui_3cl"))
  })

  output$ui_3help <- renderUI({
    helpText(tr("ui_3help"))
  })

  output$ui_3clbutton <- renderUI({
    actionButton('getCl', label = tr("ui_3clbutton"))
  })

  output$ui_4help <- renderUI({
    helpText(tr("ui_4help"))
  })
  
  output$ui_about <- renderUI({
    HTML(tr("ui_draft"), tr("ui_about"))
  })
  
  output$ui_userguide <- renderUI({
    if(translation.value$lang == "English"){
      return(includeHTML("user-guide/user-guide.html"))
    }
    includeHTML("user-guide/user-guide-french.html")
  })

  ########### Reactives --------------------
  # --- upload data
  translation.value <- reactiveValues(
    lang = "English"
  )
  observeEvent(input$fr, {
    translation.value$lang <- 'French'
  })
  observeEvent(input$en, {
    translation.value$lang <- 'English'
  })
  
  tr <- function(id, lang = translation.value$lang){
    if(lang == "English"){
      return(translations$english[translations$id == id])
    }
    return(translations$french[translations$id == id])
  }
  
  # --- upload data
  upload.values <- reactiveValues(
    upload_state = NULL
  )
  
  #  read/create handson table 
  hot.values = reactiveValues()
  hot_data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      DF <- mutate_if(DF, is.factor, as.character)
    } else {
      if (is.null(hot.values[["DF"]]))
        DF = data.frame(Concentration = rep(NA_real_, 10), 
                        Species = rep(NA_character_, 10),
                        Group = rep(NA_character_, 10))
      else
        DF = hot.values[["DF"]]
    }
    hot.values[["DF"]] = DF
    DF
  })
  
  # read whichever dataset method user chooses
  read_data <- reactive({
    req(upload.values$upload_state)
    if (upload.values$upload_state == 'upload') {
      data <- input$uploadData
      if(!grepl(".csv", data$name, fixed = TRUE)) {
        Sys.sleep(1)
        return(create_error("We're not sure what to do with that file type. Please upload a csv."))
      }
      return(readr::read_csv(data$datapath))
    } else if (upload.values$upload_state == 'demo') {
      return(readr::read_csv("demo-data/boron-data.csv"))
    } else if (upload.values$upload_state == 'hot') {
      return(hot_data())
    } })
  
  clean_data <- reactive({
    data <- read_data()
    if(length(data)) {
      # remove any column names like X1, X2 (blank headers from excel/numbers)
      data[,colnames(data) %in% paste0("X", 1:200)] <- NULL
      # remove any rows with all NA
      data <- data[!(rowSums(is.na(data) | data == "") == ncol(data)),]
    }
      data
    })
  
  # deal with unacceptable coumn names
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
    
    if(length(data[[conc]]) == 0L)
      return(tr("ui_hintdata"))
    if(!is.numeric(data[[conc]]))
      return(tr("ui_hintnum"))
    if(any(is.na(data[[conc]])))
      return(tr("ui_hintmiss"))
    if(any(data[[conc]] <= 0))
      return(tr("ui_hintpos"))
    if(any(is.infinite(data[[conc]])))
      return(tr("ui_hintfin"))
    if(zero_range(data[[conc]]))
      return(tr("ui_hintident"))
    if(length(data[[conc]]) < 6)
      return(tr("ui_hint6"))
    if(is.null(dist))
      return(tr("ui_hintdist"))
    ""
  })
  
  output$checkfit <- reactive({
    check_fit() != ""
  })
  outputOptions(output, "checkfit", suspendWhenHidden = FALSE)
  
  check_pred <- reactive({
    data <- clean_data()
    if("Concentration" %in% names(data) && length(data[["Concentration"]]) == 0L)
      return(tr("ui_hintdata"))
    if(is.null(input$selectConc))
      return(tr("ui_hintpred"))
    if(check_fit() != "")
      return(tr("ui_hintfit"))
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
    name[grepl("conc", name %>% tolower)][1]
  })
  
  guess_spp <- reactive({
    name <- column_names()
    name[grepl( "sp", name %>% tolower)][1]
  })
  
  code_label <- reactive({
    if(input$selectLabel == "-none-")
      return('NULL')
    paste0("'", input$selectLabel %>% make.names(), "'")
  })
  
  code_colour <- reactive({
    if(input$selectColour == "-none-")
      return('NULL')
    paste0("'", input$selectColour %>% make.names(), "'")
  })
  
  code_shape <- reactive({
    if(input$selectShape == "-none-")
      return('NULL')
    paste0("'", input$selectShape %>% make.names(), "'")
  })
  
  code_hc <- reactive({
    if(!input$checkHc)
      return('NULL') 
    paste0(input$selectHc, "L")
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
    if(input$selectDpi > 3000)
      return(3000)
    if(input$selectDpi == 0)
      return(1)
    input$selectDpi
  })
  
  get_dpi2 <- reactive({
    if(input$selectDpi2 > 3000)
      return(3000)
    if(input$selectDpi2 == 0)
      return(1)
    input$selectDpi2
  })
  
  estimate_hc <- reactive({
    if(input$selectHc == 0 | input$selectHc > 99)
      return()
    pred <- predict_hc()
    pred[pred$percent == input$selectHc, "est"] %>% round(2)
  })
  
  # --- fit distributions
  fit_dist <- reactive({
    req(input$selectConc)
    req(input$selectDist)
    req(check_fit() == "")
    data <- names_data()
    conc <- input$selectConc %>% make.names()
    dist <-  ssdca::ssd_fit_dists(data, left = conc,
                                          dists = input$selectDist, silent = TRUE)
  })
  
  plot_dist <- reactive({
    dist <- fit_dist()
    # withProgress(message = "This won't take long...", value = 0,{
      # incProgress(0.6)
     ggplot2::autoplot(dist, ylab = tr("ui_2ploty"))
    # })
  })
  
  table_gof <- reactive({
    dist <- fit_dist()
    gof <- ssdca::ssd_gof(dist) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
    names(gof) <- gsub("weight", tr("ui_2weight"), names(gof))
    gof
  })
  
  fit_fail <- reactive({
    req(input$selectDist)
    dist <- fit_dist()
    paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    dist <- fit_dist()
    stats::predict(dist, nboot = 10)
  })
  
  plot_model_average <- reactive({
    req(input$selectColour)
    req(input$selectLabel)
    req(input$selectShape)
    req(input$selectConc)
    
    if(input$selectHc == 0 | input$selectHc > 99)
      return()

    data <- names_data()
    pred <- predict_hc()
    conc <- input$selectConc %>% make.names()
    colour <- if(input$selectColour == "-none-") {NULL} else {input$selectColour %>% make.names()}
    label <- if(input$selectLabel == "-none-") {NULL} else {input$selectLabel %>% make.names()}
    shape <- if(input$selectShape == "-none-") {NULL} else {input$selectShape %>% make.names()}
    hc <- if(!input$checkHc) {NULL} else {input$selectHc}
    
    shape_data <- if(is.null(shape)) {NULL} else {data[[shape]]}

    validate(need(is.null(shape_data) | shape_data %>% is.character(), message = "Symbol variable cannot be numeric."))
    validate(need(shape_data %>% unique %>% length < 7, message = "Symbol variable cannot have more than 6 distinct values."))

    ssdca::ssd_plot(data, pred, left = conc, label = label, 
                    color = colour, shape = shape, hc = hc, ci = FALSE, 
                    shift_x = input$adjustLabel %>% as.numeric(), 
                    xlab = input$xaxis, ylab = input$yaxis) +
      ggtitle(input$title) +
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = NA, colour='black'),
            axis.text = element_text(color = "black"),
            legend.key = element_rect(fill = NA, colour = NA)) +
      expand_limits(x = input$xMax) + 
      scale_color_brewer(palette = input$selectPalette, name = input$legendColour) + 
      scale_shape(name = input$legendShape)
  })
  
  # --- get confidence intervals
  table_cl <- eventReactive(input$getCl, {
    dist <- fit_dist()
    withProgress(value = 0, message = "Getting Confidence Limits...", {
      incProgress(0.4)
      ssdca::ssd_hc(dist, hc = input$selectHc, nboot = input$bootSamp %>% 
                      gsub(",", "", .) %>% as.integer) %>%
        mutate_at(c("est", "se", "ucl", "lcl"), ~ round(., 2))
    })
  })

  estimate_time <- reactive({
    if(translation.value$lang == "English"){
      df <- data.frame(n = c("500", "1,000", "5,000", "10,000"), 
                       time = c("10 seconds", "15 seconds", "1.5 minutes", "3 minutes"))
    } else {
      df <- data.frame(n = c("500", "1,000", "5,000", "10,000"), 
                       time = c("10 secondes", "15 secondes", "1.5 minutes", "3 minutes"))
    }
    
    df[df$n == input$bootSamp,]$time
  })
  
  # --- create feedback
  ssdca_shiny_feedback <- reactive({
    data.frame(Name = input$name,
               Email = input$email,
               Comment = input$comment)
  })
  
  ########### Outputs --------------------
  # --- datasets
  output$hot <- renderRHandsontable({
    DF = hot_data()
    if (!is.null(DF))
      rhandsontable(DF, width = 600, useTypes = FALSE) 
  })
  
  output$viewUpload <- DT::renderDataTable({
    datatable(read_data(), options = dtopt())
    })
  
  dtopt <- reactive({
    url <- paste0("//cdn.datatables.net/plug-ins/1.10.11/i18n/", translation.value$lang, ".json")
    list(
      language = list(url = url),
      pageLength = 10
    )
  })

  
  # --- render UI with choices based on file upload
  output$selectLabel = renderUI({
    selectInput("selectLabel", 
                label = tr("ui_3label"), 
                choices = c("-none-", column_names()),
                selected = guess_spp())
  })
  
  output$selectColour = renderUI({
    selectInput("selectColour", 
                label = tr("ui_3colour"), 
                choices = c("-none-", column_names()),
                selected = "-none-")
  })
  
  output$selectShape = renderUI({
    selectInput("selectShape", 
                label = tr("ui_3symbol"), 
                choices = c("-none-", column_names()),
                selected = "-none-")
  })
  
  output$uiLegendColour <- renderUI({
    textInput('legendColour', label = tr("ui_3legend"), value = input$selectColour)
  })
  
  output$uiLegendShape <- renderUI({
    textInput('legendShape', label = tr("ui_3shape"), value = input$selectShape)
  })
  
  # --- render fit results
  output$distPlot <- renderPlot({
    plot_dist()
  })
  
  output$gofTable <- renderDataTable({ 
    datatable(table_gof(), options = list(dom = "t"))
    })
  
  output$fitFail <- renderText({
    req(fit_fail() != "")
    HTML(paste0("<font color='grey'>", paste(fit_fail(), tr("ui_hintfail")), "</font>"))
  })
  # --- render predict results
  output$modelAveragePlot <- renderPlot({
    plot_model_average()
  })
  
  output$estHc <- renderUI({
    HTML(tr("ui_3hc"), paste0("<b>", input$selectHc,"</b>"), 
         tr("ui_3hc2"), paste0("<b>", estimate_hc(), "</b>"))})
  
  output$clTable <- renderDataTable({
    datatable(table_cl(), options = list(dom = "t"))
  })
  
  output$describeCl <- renderText({
    HTML(tr("ui_3cldesc1"), paste0("<b>", input$selectHc, "</b>"), 
         tr("ui_3cldesc2"), 
         paste0("<b>", input$bootSamp, ".</b>"), 
         tr("ui_3cldesc3"),
         paste0("<b>", estimate_time(), "</b>"),
         tr("ui_3cldesc4"))
  })
  
  # --- render UI
  onclick('linkFormatPredict', toggle('divFormatPredict', anim = TRUE, animType = "slide", time = 0.2))
  onclick('linkPngFormatPredict', toggle('divPngFormatPredict', anim = TRUE, animType = "slide", time = 0.2))
  onclick('linkFormatFit', toggle('divFormatFit', anim = TRUE, animType = "slide", time = 0.2))
  
  output$uiXmax <- renderUI({
    req(input$selectConc)
    conc <- input$selectConc
    data <- clean_data()
    numericInput('xMax', label = 'X-axis max', min = 1, value = max(data[[conc]], na.rm = TRUE))
  })
  
  output$uiXmin <- renderUI({
    req(input$selectConc)
    conc <- input$selectConc
    data <- clean_data()
    numericInput('xMin', label = 'X-axis min', min = 1, value = min(data[[conc]], na.rm = TRUE))
  })
  
  # --- download handlers
  output$dlFitPlot <- downloadHandler(
    filename = function() {"ssdtools_distFitPlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_dist(), device = "png",
                      width = get_width2(), height = get_height2(), dpi = get_dpi2())
    }
  )
  
  output$dlPredPlot <- downloadHandler(
    filename = function() {"ssdtools_modelAveragePlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_model_average(), device = "png", 
                      width = get_width(), height = get_height(), dpi = get_dpi())
    }
  )
  
  output$dlFitTable <- downloadHandler(
    filename = function() {"ssdtools_distGofTable.csv"},
    content <- function(file) {
      readr::write_csv(table_gof() %>% as_tibble(), file)
    }
  )
  
  output$dlPredTable <- downloadHandler(
    filename = function() {"ssdtools_predictTable.csv"},
    content <- function(file) {
      if(!is.null(table_cl())) {
        return(readr::write_csv(table_cl() %>% as_tibble(), file))
      } else {
        return()
      }
    }
  )  
  
  # --- render code
  output$codeHead <- renderUI({
    if(upload.values$upload_state == "hot" && is.na(read_data()$Concentration[1]))
      return()
    l1 <- "install.packages('ssdtools')"
    l2 <- "library(ssdtools)"
    l3 <- "library(ggplot2)"
    if(upload.values$upload_state == "upload") {l4 <- "library(readr)"} else {l4 <- NULL}
    HTML(paste(l1, l2, l3, l4, sep = "<br/>"))
  })
  
  output$codeData <- renderUI({
    c1 <- "# read dataset"
    c2 <- "# the file argument of read_csv() assumes the file is in your working directory. You may need to change the file path to correctly read your dataset."
    c4 <- "# this is the output of dput, which is used to create a data.frame from data entered in interactive spreadsheet"
    hot <- paste0("data <- ", capture.output(dput(clean_data())) %>% glue::collapse())
    upload <- paste0("data <- read_csv(file = '", input$uploadData$name, "')")
    demo <- "data <- ssdtools::boron_data"
    c3 <- "# fix unacceptable column names"
    name <- "colnames(data) <- make.names(colnames(data))"
    if(upload.values$upload_state == "hot")
      return(HTML(paste(c1, c4, hot, c3, name, sep = "<br/>")))
    if(upload.values$upload_state == "upload")
      return(HTML(paste(c1, c2, upload, c3, name, sep = "<br/>")))
    if(upload.values$upload_state == "demo")
      return(HTML(paste(c1, demo, c3, name, sep = "<br/>")))
      })

  output$codeFit <- renderUI({
    req(check_fit() == "")
    c1 <- "# fit distributions"
    fit <- paste0("dist <- ssd_fit_dists(data, left = '", input$selectConc %>% make.names, 
         "', dists = c(", paste0("'", input$selectDist, "'", collapse = ', '), "))")
    c2 <- "# plot distributions"
    plot <- "autoplot(dist)"
    c3 <- "# goodness of fit table"
    table <- "ssd_gof(dist)"
    HTML(paste(c1, fit, c2, plot, c3, table, sep = "<br/>"))
  })
  
  output$codePredPlot <- renderUI({
    req(check_fit() == "")
    req(check_pred() == "")
    req(input$selectLabel)
    xmax <- ifelse(is.null(input$xMax), "NULL", input$xMax)
    legend.colour <- ifelse(is.null(input$legendColour), "NULL", paste0("'", input$legendColour, "'"))
    legend.shape <- ifelse(is.null(input$legendShape) || input$legendShape == "-none-", "NULL", paste0("'", input$legendShape, "'"))
    
    c1 <- "# plot model average"
    c2 <- "# set the nboot argument and set ci = TRUE in ssd_plot to add confidence intervals to plot."
    c3 <- "# we reccommend using nboot = 10000, although this may take half a day to run"
    pred <- "pred <- predict(dist, nboot = 10L)"
    plot <-  paste0("ssd_plot(data, pred, left = '", input$selectConc %>% make.names, 
                    "', label = ", code_label(),
                    ", color = ", code_colour(),
                    ", shape = ", code_shape(),
                    ", hc = ", code_hc(), 
                    ", ci = FALSE, <br/>shift_x = ", input$adjustLabel,
                    ", xlab = '", input$xaxis,
                    "', ylab = '", input$yaxis,
                    "') + <br/> ggtitle('", input$title, 
                    "') +<br/> theme(panel.border = element_blank(),<br/> 
                    panel.grid.major = element_blank(),<br/>  
                    panel.grid.minor = element_blank(),<br/> 
                    panel.background = element_rect(fill = NA, colour='black'),<br/>
                    axis.text = element_text(color = 'black'),<br/>
                    legend.key = element_rect(fill = NA, colour = NA)) +<br/>
                    expand_limits(x = ", xmax, ") +<br/>
                    scale_color_brewer(palette = '", input$selectPalette, "', name = ", legend.colour, ") +<br/> 
                    scale_shape(name = ", legend.shape, ")")
    HTML(paste(c1, c2, c3, pred, plot, sep = "<br/>"))
  })
  
  output$codePredCl<- renderUI({
    req(input$getCl)
    req(check_fit() == "")
    req(check_pred() == "")
    c1 <- "# get confidence limits"
    c2 <- "# use the nboot argument in ssd_hc to set the number of bootstrap samples"
    conf <- paste0("ssd_hc(dist, hc = ", input$selectHc, "L",
    ", nboot = ", input$bootSamp %>% gsub(',', '', .) %>% as.integer, "L)") 
    HTML(paste(c1, c2, conf, sep = "<br/>"))
  })
  
  output$codeSaveFit <- renderUI({
    req(check_fit() == "")
    c1 <- "# save plot"
    c2 <- "# width and height are in inches, dpi (dots per inch) sets resolution"
    save <- paste0("ggsave('fit_dist_plot.png', 
                   width = ", get_width2(), 
                   " , height = ", get_height2(), 
                   " , dpi = ", get_dpi2(),
                   ")")
    HTML(paste(c1, c2, save, sep = "<br/>"))
  })
  
  output$codeSavePred <- renderUI({
    req(check_fit() == "")
    req(check_pred() == "")
    req(input$selectLabel)
    c1 <- "# save plot"
    c2 <- "# width and height are in inches, dpi (dots per inch) sets resolution"
    save <- paste0("ggsave('model_average_plot.png', 
                   width = ", get_width(), 
                   " , height = ", get_height(), 
                   " , dpi = ", get_dpi(),
                   ")")
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
    upload.values$upload_state <- 'upload'
  })
  
  observeEvent(input$demoData, {
    upload.values$upload_state <- 'demo'
  })
  
  observeEvent(input$hot, {
    upload.values$upload_state <- 'hot'
  })
  
  # --- feedback
  observeEvent(input$contactUs, {
    shinyjs::toggle("feedbackForm", anim = TRUE, animType = "slide", time = 0.2)
  })
  
  create_feedback <- reactive({
    data.frame(App = "ssdtools",
               Name = input$name,
               Email = input$email,
               Comment = input$comment)
  })
  
  observe({
    shinyjs::toggleState("submit_feedback", condition = input$comment != "")
  })
  
  observeEvent(input$submit_feedback, {   
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Sending message...", value = 0.5)  
    create_feedback() %>% slackr::slackr()
    showModal(modalDialog(
      footer = modalButton("OK"),
      title = "",
      "Thanks! Your message was successfully submitted."
    ))
    shinyjs::reset('feedbackForm')
    shinyjs::hide('feedbackForm')})
}
  







