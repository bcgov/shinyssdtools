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
    print(data)
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
    if(length(data[["Conc"]]) == 0L)
      return("You have not added a dataset.")
    if(!is.null(input$selectConc) & length(data[[input$selectConc]]) == 0L)
      return("You have not added a dataset.")
    if(is.null(fit_dist()))
      return("You have not fit distributions yet. Select the 'Fit' tab.")
    ""
  })
  
  output$checkpred <- reactive({
    check_pred() != ""
  })
  outputOptions(output, "checkpred", suspendWhenHidden = FALSE)

  output$hintFi <- renderText(red_hint(check_fit()))
  output$hintPr <- renderText(red_hint(check_pred()))

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
  
  # --- fit distributions
  fit_dist <- reactive({
    if(is.null(input$selectConc))
      return(NULL)
    data <- names_data()
    conc <- input$selectConc %>% make.names()
    dist <-  ssdca::ssd_fit_dists(data, left = conc,
                                          dists = input$selectDist, silent = TRUE)
  })
  
  plot_dist <- reactive({
    withProgress(message = "Calculating...", value = 0,{
      incProgress(0.6)
      ggplot2::autoplot(fit_dist())
    })
  })
  
  table_gof <- reactive({
    gof <- ssdca::ssd_gof(fit_dist()) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    stats::predict(fit_dist(), nboot = 10)
  })
  
  plot_model_average <- reactive({
    req(input$selectConc)
    req(input$selectDist)
    req(input$selectHc)
    req(input$selectSpp)
    req(input$selectGroup)
    if(input$selectHc == 0 | input$selectHc > 99)
      return(NULL)
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
    req(input$selectConc)
    req(input$selectDist)
    req(input$selectHc)
    req(input$selectSpp)
    req(input$selectGroup)
    if(input$selectHc == 0 | input$selectHc > 99)
      return(NULL)
    pred <- predict_hc()
    pred[pred$percent == input$selectHc, "est"] %>% round(2)
  })
  
  # --- get confidence intervals
  table_cl <- eventReactive(input$getCl, {
    withProgress(value = 0, message = "Generating Confidence Limits...", {
      incProgress(0.4)
      ssdca::ssd_hc(fit_dist(), hc = input$selectHc, nboot = input$bootSamp %>% as.integer) %>%
        mutate_at(c("est", "se", "ucl", "lcl"), ~ round(., 2))
    })
  })

  estimate_time <- reactive({
    df <- data.frame(n = c("500", "1,000", "5,000", "10,000"), time = c("10 seconds.", "15 seconds.", "1 and a half minutes.", "3 minutes."))
    df[df$n == input$bootSamp,]$time
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
  
  # --- results
  output$distPlot <- renderPlot({
    if(check_fit() != "")
      return(NULL)
    plot_dist()
  })
  
  output$gofTable <- renderDataTable({ 
    if(check_fit() != "")
      return(NULL)
    datatable(table_gof(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))})
  
  # --- predict
  output$modelAveragePlot <- renderPlot({
    if(check_pred() != "")
      return(NULL)
    plot_model_average()
  })
  
  output$estHc <- renderUI({
    if(check_pred() != "")
      return(NULL)
    HTML("The model averaged estimate of the concentration that affects",
                                 paste0("<b>", input$selectHc,"</b>"), 
                                 "% of species is",
                                 paste0("<b>", estimate_hc(), "</b>"))})
  
  output$clTable <- renderDataTable({
    if(check_pred() != "")
      return(NULL)
    datatable(table_cl(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))
  })
  
  output$sampTime <- renderText({
    HTML("It will take around", paste0("<b>", estimate_time(), "</b>"), "to get CL from",
         paste0("<b>", input$bootSamp, "</b>"), "bootstrap samples.")
  })
  
  output$describeCi <- renderText({
    HTML("You have selected", paste0("<b>", input$selectHc, "</b>"), 
         "% threshold to estimate hazard concentration and", 
         paste0("<b>", input$bootSamp, "</b>"), 
         "bootstrap samples to generate confidence limits. This will take around",
         paste0("<b>", estimate_time(), "</b>"))
  })
  
  # --- download handlers
  output$dlDistPlot <- downloadHandler(
    filename = function() {"ssdca_distFitPlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_dist(), device = "png")
    }
  )
  
  output$dlModelPlot <- downloadHandler(
    filename = function() {"ssdca_modelAveragePlot.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_model_average(), device = "png")
    }
  )
  
  output$dlGofTable <- downloadHandler(
    filename = function() {"ssdca_distGofTable.csv"},
    content <- function(file) {
      readr::write_csv(table_gof() %>% as_tibble(), file)
    }
  )
  
  output$dlPredTable <- downloadHandler(
    filename = function() {"ssdca_predictTable.csv"},
    content <- function(file) {
      readr::write_csv(table_cl() %>% as_tibble(), file)
    }
  )  
  ########### Observers --------------------
  # --- data upload
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
  
  observeEvent(input$uploadData, {
    upload.values$upload_state <- 'upload'
  })
  
  observeEvent(input$demoData, {
    upload.values$upload_state <- 'demo'
  })
  
  observeEvent(input$hot, {
    upload.values$upload_state <- 'hot'
  })
  
  # --- describe results
  
  # --- feedback
  
  # --- information
  
}





