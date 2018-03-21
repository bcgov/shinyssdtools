# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  ########### Reactives --------------------
  # --- upload data
  upload.values <- reactiveValues(
    upload_state = NULL
  )
  
  #  read/create handson table 
  hot.values = reactiveValues()
  hot_data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
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
      return("You have not added a dataset.")
    if(!is.numeric(data[[conc]]))
      return("Concentration column must contain numbers.")
    if(any(is.na(data[[conc]])))
      return("Concentration values must not be missing.")
    if(any(data[[conc]] <= 0))
      return("Concentration values must be positive.")
    if(any(is.infinite(data[[conc]])))
      return("Concentration values must be finite.")
    if(zero_range(data[[conc]]))
      return("Concentration values must not all be identical.")
    if(length(data[[conc]]) < 6)
      return("There must be at least 6 concentration values.")
    if(is.null(dist))
      return("At least one distribution must be selected.")
    ""
  })
  
  output$checkfit <- reactive({
    check_fit() != ""
  })
  outputOptions(output, "checkfit", suspendWhenHidden = FALSE)
  
  check_pred <- reactive({
    data <- clean_data()
    if("Concentration" %in% names(data) && length(data[["Concentration"]]) == 0L)
      return("You have not added a dataset.")
    if(is.null(input$selectConc))
      return("You must select the 'Fit' tab before using the 'Predict' tab.")
    if(check_fit() != "")
      return("You have not successfully fit any distributions yet. Run the 'Fit' tab first.")
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
  
  get_expandX <- reactive({
    conc <- input$selectConc
    data <- clean_data()
    ifelse(is.na(input$expandX), max(data[[conc]], na.rm = TRUE), input$expandX)
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
      ggplot2::autoplot(dist)
    # })
  })
  
  table_gof <- reactive({
    dist <- fit_dist()
    gof <- ssdca::ssd_gof(dist) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
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
    if(input$selectHc == 0 | input$selectHc > 99)
      return()
    req(input$selectColour)
    req(input$selectLabel)
    req(input$selectShape)

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
      expand_limits(x = get_expandX())
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
    df <- data.frame(n = c("500", "1,000", "5,000", "10,000"), time = c("10 seconds.", "15 seconds.", "1 and a half minutes.", "3 minutes."))
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
  
  output$viewUpload <- renderDataTable({
    read_data()
  })
  
  # --- render UI with choices based on file upload
  output$selectConc = renderUI({
    selectInput("selectConc", 
                label = label_mandatory("Select column with concentration values"), 
                choices = column_names(),
                selected = guess_conc())
  })
  
  output$selectLabel = renderUI({
    selectInput("selectLabel", 
                label = "Label by", 
                choices = c("-none-", column_names()),
                selected = guess_spp())
  })
  
  output$selectColour = renderUI({
    selectInput("selectColour", 
                label = "Colour by", 
                choices = c("-none-", column_names()),
                selected = "-none-")
  })
  
  output$selectShape = renderUI({
    selectInput("selectShape", 
                label = "Symbol by", 
                choices = c("-none-", column_names()),
                selected = "-none-")
  })
  
  output$expandX <- renderUI({
    numericInput('expandX', label = 'Expand X-axis', min = 1, value = )
  })
  
  output$expandX <- renderUI({
    req(input$selectConc)
    conc <- input$selectConc
    data <- clean_data()
    numericInput('expandX', label = 'Expand X-axis', min = 1, value = max(data[[conc]], na.rm = TRUE))
  })
  
  # --- render fit results
  output$distPlot <- renderPlot({
    plot_dist()
  })
  
  output$gofTable <- renderDataTable({ 
    datatable(table_gof(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))})
  
  output$fitFail <- renderText({
    req(fit_fail() != "")
    HTML(paste0("<font color='grey'>", paste(fit_fail(), "distribution(s) failed to fit. Run R code to get more information."), "</font>"))
  })
  # --- render predict results
  output$modelAveragePlot <- renderPlot({
    plot_model_average()
  })
  
  output$estHc <- renderUI({
    HTML("The model averaged estimate of the concentration that affects",
                                 paste0("<b>", input$selectHc,"</b>"), 
                                 "% of species is",
                                 paste0("<b>", estimate_hc(), "</b>"))})
  
  output$clTable <- renderDataTable({
    datatable(table_cl(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))
  })
  
  output$sampTime <- renderText({
    HTML("It will take around", paste0("<b>", estimate_time(), "</b>"), "to get CL from",
         paste0("<b>", input$bootSamp, "</b>"), "bootstrap samples.")
  })
  
  output$describeCl <- renderText({
    HTML("You have selected", paste0("<b>", input$selectHc, "</b>"), 
         "% threshold to estimate hazard concentration and", 
         paste0("<b>", input$bootSamp, "</b>"), 
         "bootstrap samples to generate confidence limits. This will take around",
         paste0("<b>", estimate_time(), "</b>"))
  })
  
  # --- download handlers
  output$dlFitPlot <- downloadHandler(
    filename = function() {"ssdca_distFitPlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_dist(), device = "png",
                      width = get_width2(), height = get_height2(), dpi = get_dpi2())
    }
  )
  
  output$dlPredPlot <- downloadHandler(
    filename = function() {"ssdca_modelAveragePlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_model_average(), device = "png", 
                      width = get_width(), height = get_height(), dpi = get_dpi())
    }
  )
  
  output$dlFitTable <- downloadHandler(
    filename = function() {"ssdca_distGofTable.csv"},
    content <- function(file) {
      readr::write_csv(table_gof() %>% as_tibble(), file)
    }
  )
  
  output$dlPredTable <- downloadHandler(
    filename = function() {"ssdca_predictTable.csv"},
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
    l1 <- "library(ssdca)"
    l2 <- "library(ggplot2)"
    if(upload.values$upload_state == "upload") {l3 <- "library(readr)"} else {l3 <- NULL}
    HTML(paste(l1, l2, l3, sep = "<br/>"))
  })
  
  output$codeData <- renderUI({
    c1 <- "# read dataset"
    c2 <- "# the file argument of read_csv() assumes the file is in your working directory. You may need to change the file path to correctly read your dataset."
    c4 <- "# this is the output of dput, which is used to create a data.frame from data entered in interactive spreadsheet"
    hot <- paste0("data <- ", capture.output(dput(clean_data())) %>% glue::collapse())
    upload <- paste0("data <- read_csv(file = '", input$uploadData$name, "')")
    demo <- "data <- ssdca::boron_data"
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
                    expand_limits(x = ", get_expandX(), ")")
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
  observeEvent(input$submit_feedback,
               {               req(input$comment)
                 withProgress(value = 0.2, "Sending...", {
                 slackr::slackr(ssdca_shiny_feedback())
                 })
                 showModal(modalDialog(
                   footer = modalButton("OK"),
                   title = "",
                   "Thanks! your message has been sent to the administrator."
                 ))})
}




