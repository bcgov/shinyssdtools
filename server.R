
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  ########### Reactives --------------------
  upload.values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$uploadData, {
    upload.values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$demoData, {
    upload.values$upload_state <- 'demo'
  })
  
  read_data <- reactive({
    req(upload.values$upload_state)
    if (upload.values$upload_state == 'uploaded') {
      data <- input$uploadData
      if(!grepl(".csv", data$name, fixed = TRUE)) {
        Sys.sleep(1)
        return(create_error("We're not sure what to do with that file type. Please upload a csv."))
      }
      return(readr::read_csv(data$datapath))
    } else if (upload.values$upload_state == 'demo') {
      return(readr::read_csv("demo-data/boron-data.csv"))
    }})
  
  # clean common problems to avoid errors
  clean_data <- reactive({
    data <- read_data()
    if(length(data)) {
      # remove any column names like X1, X2 (blank headers from excel/numbers)
      data[,colnames(data) %in% paste0("X", 1:200)] <- NULL
      # remove any rows with all NA
      data <- data[!(rowSums(is.na(data)) == ncol(data)),]
    }
    data
  })
  
  check_data <- reactive({

    req(input$go)
    conc <- isolate(input$selectConc)
    spp <- isolate(input$selectSpp)
    dist <- isolate(input$selectDist)
    
    data <- isolate(clean_data())

    if(!is.numeric(data[[conc]]))
      return(create_error("Concentration column must contain numbers."))
    if(any(is.na(data[[conc]])))
      return(create_error("Concentration values must not be missing."))
    if(any(data[[conc]] <= 0))
      return(create_error("Concentration values must be positive."))
    if(any(is.infinite(data[[conc]])))
      return(create_error("Concentration values must be finite."))
    if(length(unique(data[[conc]])) < 5)
      return(create_error("There must be at least 5 distinct concentration values."))
    if(any(is.na(data[[spp]])))
      return(create_error("Species names must not be missing."))
    if(anyDuplicated(data[[spp]]))
      return(create_error("Some species are duplicated. This app only handles one chemical at a time. Each species should only have one concentration value."))
    if(is.null(dist))
      return(NULL)
    data 
  })
  
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
    data <- check_data()
    dist <-  isolate(ssdca::ssd_fit_dists(data, left = input$selectConc,
                                          dists = input$selectDist, silent = TRUE))
  })
  
  plot_dist <- reactive({
    req(check_data())
    withProgress(message = "Calculating...", value = 0,{
      incProgress(0.6)
      ggplot2::autoplot(fit_dist())
    })
  })
  
  table_gof <- reactive({
    req(check_data())
    gof <- ssdca::ssd_gof(fit_dist()) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    withProgress(value = 0, message = "Calculating...", {
      incProgress(0.3)
      dist <- fit_dist()
      incProgress(amount = 0.6)
      pred <- stats::predict(dist, nboot = 10) 
      pred
    })
  })
  
  plot_model_average <- reactive({
    req(check_data())
    data <- check_data()
    pred <- predict_hc()
    ssdca::ssd_plot(data, pred, left = isolate(input$selectConc), label = isolate(input$selectSpp), hc = input$selectHc, ci = FALSE)
  })
  
  describe_hc <- reactive({
    req(check_data())
    req(input$selectHc)
    pred <- predict_hc()
    est <- pred[pred$percent == input$selectHc, "est"] %>% round(2)
    est
  })
  
  # --- create feedback
  ssdca_shiny_feedback <- reactive({
    data.frame(Name = input$name,
               Email = input$email,
               Comment = input$comment)
  })
  
  ########### Outputs --------------------
  # --- render UI with choices based on file upload
  output$selectConc = renderUI({
    selectInput("selectConc", 
                label = label_mandatory("Select concentration column:"), 
                choices = column_names(),
                selected = guess_conc())
  })
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Select species column (optional):", 
                choices = column_names(),
                selected = guess_spp())
  })
  
  output$selectDist <- renderUI({
    selectizeInput('selectDist', 
                   label = label_mandatory("Select distributions to fit:"),
                   multiple = TRUE, 
                   choices = c(default.dists, extra.dists),
                   selected = default.dists,
                   options = list(
                     'plugins' = list('remove_button'),
                     'create' = TRUE,
                     'persist' = FALSE))
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
      readr::write_csv(tibble(a = 1), file)
    }
  )
  
  output$dlPredTable <- downloadHandler(
    filename = function() {"ssdca_predictTable.csv"},
    content <- function(file) {
      pred <- predict_hc()
      pred <- pred[, c("percent", "est")]
      readr::write_csv(pred, file)
    }
  )
  
  ########### Observers --------------------
  # update button
  observeEvent(input$go, {
    data <- check_data()
    # --- fit dist
    output$distPlot <- renderPlot({
      plot_dist()
    })
    
    output$gofTable <- renderDataTable({ 
      datatable(table_gof(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))})
    
    # --- describe results
    output$estHc <- renderUI({HTML(paste0("<b>", describe_hc()$est, "<b>"))})
    output$text1 <- renderUI({HTML("The model average estimate of the concentration that affects")})
    output$selectHc <- renderUI({numericInput("selectHc", label = NULL, value = 5, min = 0, 
                                              max = 99, step = 5, width = "70px")})
    output$text2 <- renderUI({HTML("% of species is")})
    
    # --- predict
    output$modelAveragePlot <- renderPlot({
      plot_model_average()
    })
  })
  
  # --- feedback
  observeEvent(input$feedback,
               {showModal(modalDialog(title = "Message sent to administrator.", 
                                      size = "m", easyClose = TRUE,
                                      footer = modalButton("Never mind"),
                                      textInput("name", "Name (optional):", width = "30%"),
                                      textInput("email", "Email (optional):", width = "30%"),
                                      textInput("comment", label_mandatory("Comment:"), width = "100%"),
                                      actionButton("submit_feedback", "Submit")))})
  
  observeEvent(input$submit_feedback,
               {slackr::slackr(ssdca_shiny_feedback())
                 removeModal()})
  
  # --- information
  observeEvent(input$information,
               {showModal(modalDialog(tech.info,
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it")))})
}
  