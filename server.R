
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  # Reactives --------------------

  # --- read, clean and check data
  read_data <- reactive({
    req(input$uploadData)
    data <- input$uploadData
    if(is.null(data)) {return(NULL)}
    if(!grepl(".csv", data$name, fixed = TRUE)) {return(NULL)}
    isolate(readr::read_csv(data$datapath))
  })
  
  # demo_data
  demo_data <- reactive({
    data <- read_csv("test/data/boron-data.csv")
  })
  
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
    # dependency on go/update button
    input$go
    
    data <- clean_data()
    # data <- clean_data()
    # conc <- data[[,input$selectConc]]
    # if(!is.numeric(conc)) return(create_error("Concentration column must contain numbers."))

    # data 
  })

  
  column_names <- reactive({
    names(clean_data()) 
  })
  
  guess_conc <- reactive({
    name <- column_names()
    conc <- name[stringr::str_detect(name %>% tolower, "conc")][1]
  })
  
  guess_spp <- reactive({
    name <- column_names()
    name[stringr::str_detect(name %>% tolower, "sp")][1]
  })
  
  # --- fit distributions
  # fit_dist <- reactive({
  #   withProgress(message = "Fitting distribution", value = 0, {
  #     data <- check_data()
  #     incProgress(20)
  #     dist <-  isolate(ssdca::ssd_fit_dists(data, left = input$selectConc, 
  #                                           dists = input$selectDist, silent = TRUE))
  #     incProgress(80)
  #   })
  #   dist
  #     
  # })
  
  fit_dist <- reactive({
      data <- check_data()
      dist <-  isolate(ssdca::ssd_fit_dists(data, left = input$selectConc, 
                                            dists = input$selectDist, silent = TRUE))
  })
  
  plot_dist <- reactive({
      autoplot(fit_dist())
  })
    
  table_gof <- reactive({
    req(input$go)
    gof <- ssdca::ssd_gof(fit_dist()) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    dist <- fit_dist()
    pred <- predict(dist)
  })
  
  plot_model_average <- reactive({
    withProgress(message = "Getting predictions...",
                 detail = "This may take a minute or so...",
                 value = 0, {
                   data <- clean_data()
                   incProgress(10)
                   pred <- predict_hc()
                   incProgress(75)})
                   ssdca::ssd_plot(data, pred, label = input$selectSpp, hc = input$selectHc/100)
                                    
  })
  
  describe_hazard_conc <- reactive({
    pred <- predict_hc()
    est <- pred[round(pred$prop, 2) == (input$selectHc/100), "est"] %>% round(2)
    lower <- pred[round(pred$prop, 2) == (input$selectHc/100), "lcl"] %>% round(2)
    upper <- pred[round(pred$prop, 2) == (input$selectHc/100), "ucl"] %>% round(2)
    
    out <- list(est = est, lower = lower, upper = upper)
    out
  })
  
  # Outputs --------------------
  # output$selectedDist <- renderPrint({input$selectDist%>% paste("\n") %>% glue::collapse()})
  
  # --- render UI with choices based on file upload
  output$selectConc = renderUI({
    selectInput("selectConc", 
                label = label_mandatory("Select concentration column:"), 
                choices = column_names(),
                selected = guess_conc())
  })
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Select species column:", 
                choices = column_names(),
                selected = guess_spp())
  })
  
  output$selectDist <- renderUI({
    req(input$uploadData)
    selectizeInput('selectDist', 
                   label = label_mandatory("Select distributions to fit:"),
                   multiple = TRUE, 
                   choices = list(Recommended = default.dists, Additional = extra.dists),
                   selected = default.dists,
                   options = list(
                     'plugins' = list('remove_button'),
                     'create' = TRUE,
                     'persist' = FALSE))
  })
  
  # --- fit dist
  output$distPlot <- renderPlot({
    req(input$go)
    withProgress(message = "Fitting distributions...", value = 0, {
      incProgress(20)
      plot_dist()

  })})
    
  output$gofTable <- renderDataTable({
    req(input$go)
    table_gof()})
  
  # --- predict
  output$modelAveragePlot <- renderPlot({
    req(input$go)
   plot_model_average()
  })
  
  # --- describe results
  output$estHc <- renderUI({HTML(paste0("<b>", describe_hazard_conc()$est, "<b>"))})
  output$lowerHc <- renderUI({HTML(paste0("<b>", describe_hazard_conc()$lower, "<b>"))})
  output$upperHc <- renderUI({HTML(paste0("<b>", describe_hazard_conc()$upper, "<b>"))})
  output$text1 <- renderUI({HTML("The model average estimate of the concentration that affects")})
  output$selectHc <- renderUI({numericInput("selectHc", label = NULL, value = 5, min = 0, 
                                            max = 99, step = 5, width = "70px")})
  output$text2 <- renderUI({HTML("% of species is")})
  output$text3 <- renderUI({HTML("but it could be as low as")})
  output$text4 <- renderUI({HTML("or as high as")})
  
  # --- check data
  # output$hint <- renderText(check_data())
    
  # Observers --------------------
  # --- extras
  observeEvent(input$feedback,
               {showModal(modalDialog(title = "", 
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it"),
                                      textInput("name", "Name (optional):", width = "30%"),
                                      textInput("email", "Email (optional):", width = "30%"),
                                      textInput("comment", labelMandatory("Comment:"), width = "100%"),
                                      actionButton("submit_feedback", "Submit")))})
  
  # --- information
  observeEvent(input$information,
               {showModal(modalDialog("Here is where we put technical details about how the models are fit, etc.",
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it")))})
  
 
}
  
#   # --- go/update
#   observeEvent(input$go, 
#                {
#  
#     
# 
# 
#     })
# 
# }


  
  
  
  
  
  