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
    if(length(unique(data[[conc]])) < 8)
      return("There must be at least 8 distinct concentration values.")
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
  
  code_spp <- reactive({
    if(input$selectSpp == "-none-")
      return('NULL')
    paste0("'", input$selectSpp %>% make.names(), "'")
  })
  
  code_group <- reactive({
    if(input$selectGroup == "-none-")
      return('NULL')
    paste0("'", input$selectGroup %>% make.names(), "'")
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
    print(fit_fail())
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
    req(input$selectSpp)
    req(input$selectGroup)
    data <- names_data()
    pred <- predict_hc()
    conc <- input$selectConc %>% make.names()
    group <- if(input$selectGroup == "-none-") {NULL} else {input$selectGroup %>% make.names()}
    spp <- if(input$selectSpp == "-none-") {NULL} else {input$selectSpp %>% make.names()}
    ssdca::ssd_plot(data, pred, left = conc, label = spp, 
                    color = group, hc = input$selectHc, ci = FALSE, 
                    shift_x = input$adjustLabel %>% as.numeric(), 
                    xlab = input$xaxis, ylab = input$yaxis) +
      ggtitle(input$title)
  })
  
  estimate_hc <- reactive({
    if(input$selectHc == 0 | input$selectHc > 99)
      return()
    pred <- predict_hc()
    pred[pred$percent == input$selectHc, "est"] %>% round(2)
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
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Label by", 
                choices = c("-none-", column_names()),
                selected = guess_spp())
  })
  
  output$selectGroup = renderUI({
    selectInput("selectGroup", 
                label = "Colour by", 
                choices = c("-none-", column_names()),
                selected = "-none-")
  })
  
  # --- render fit results
  output$distPlot <- renderPlot({
    plot_dist()
  })
  
  output$gofTable <- renderDataTable({ 
    datatable(table_gof(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))})
  
  output$fitFail <- renderText({
    req(fit_fail() != "")
    HTML(paste0("<font color='grey'>", paste(fit_fail(), "distribution(s) failed to fit."), "</font>"))
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
    # req(check_pred() == "")
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
      ggplot2::ggsave(file, plot = plot_dist(), device = "png")
    }
  )
  
  output$dlPredPlot <- downloadHandler(
    filename = function() {"ssdca_modelAveragePlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_model_average(), device = "png")
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
    if(upload.values$upload_state == "hot" && is.na(read_data()$Concentration[1]))
      return()
    c1 <- "# read dataset"
    c2 <- "# the file argument of read_csv() assumes the file is in your working directory. You may need to change the file path to correctly read your dataset."
    hot <- paste0("data <- ", capture.output(dput(clean_data())) %>% glue::collapse())
    upload <- paste0("data <- read_csv(file = '", input$uploadData$name, "')")
    demo <- "data <- ssdca::boron_data"
    c3 <- "# fix unacceptable column names"
    name <- "colnames(data) <- make.names(colnames(data))"
    if(upload.values$upload_state == "hot")
      return(HTML(paste(c1, hot, c3, name, sep = "<br/>")))
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
    req(input$selectSpp)
    
    c1 <- "# plot model average"
    c2 <- "# set the nboot argument and set ci = TRUE in ssd_plot to add confidence intervals to plot."
    c3 <- "# we reccommend using nboot = 10000, although this may take half a day to run"
    pred <- "pred <- predict(dist, nboot = 10L)"
    plot <-  paste0("ssd_plot(data, pred, left = '", input$selectConc %>% make.names, 
                    "', label = ", code_spp(),
                    ", color = ", code_group(),
                    ", hc = ", input$selectHc, "L",
                    ", ci = FALSE, shift_x = ", input$adjustLabel,
                    ", xlab = '", input$xaxis,
                    "', ylab = '", input$yaxis,
                    "') + ggtitle('", input$title, "')")
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
  observeEvent(input$feedback,
               {showModal(modalDialog(title = "You can use this to report a bug, request a feature, or simply to provide some feedback. The message is sent to the administrator.", 
                                      size = "m", easyClose = TRUE,
                                      footer = modalButton("Never mind"),
                                      textInput("name", "Name (optional):", width = "30%"),
                                      textInput("email", "Email (optional):", width = "30%"),
                                      textInput("comment", label_mandatory("Comment:"), width = "100%"),
                                      actionButton("submit_feedback", "Submit")))})
  
  observeEvent(input$submit_feedback,
               {withProgress(value = 0.2, "Sending...", {
                 slackr::slackr(ssdca_shiny_feedback())
                 removeModal()})
                 })
  
  # --- information
  observeEvent(input$information,
               {showModal(modalDialog(tech.info,
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it")))})
}




