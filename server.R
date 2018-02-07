
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
    data <- clean_data()
  })
  
  column_names <- reactive({
    names(clean_data()) 
  })
  
  # --- fit distributions
  fit_dist <- reactive({
    data <- clean_data()
    dist <-  ssdca::ssd_fit_dists(data, left = input$selectConc, dists = input$selectDist, silent = TRUE)
  })
  
  plot_dist <- reactive({
    autoplot(fit_dist())
})
    
  table_gof <- reactive({
    dist <- fit_dist()
    gof <- ssdca::ssd_gof(dist) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    dist <- fit_dist()
    pred <- predict(dist)
  })
  
  plot_model_average <- reactive({
    data <- clean_data()
    pred <- predict_hc()
    ssd_plot(data, pred, label = input$selectSpp, hc = input$selectHc)
  })
  
  describe_hazard_conc <- reactive({
    pred <- predict_hc()
    est <- pred[pred$prop == input$selectHc, "est"]
    lower <- pred[pred$prop == input$selectHc, "lcl"]
    upper <- pred[pred$prop == input$selectHc, "ucl"]
    
    output <- paste("The model average estimate of the concentartion that affects", 
                    input$selectHc, 
                    "of the species is", est,
                    "but it could be as low as", lower, 
                    "or as high as", upper)
  })
  
  
  # Outputs --------------------
  # output$selectedDist <- renderPrint({input$selectDist%>% paste("\n") %>% glue::collapse()})
  
  # --- render UI with choices based on file upload
  output$selectConc = renderUI({
    selectInput("selectConc", 
                label = label_mandatory("Select concentration column:"), 
                choices = column_names())
  })
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Select species column:", 
                choices = column_names())
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
  
  output$selectHc <- renderUI({
    req(input$uploadData)
    numericInput('selectHc', label = "Select hazard concentration", value = 0.05, 
                 min = 0.01, max = 0.99, step = 0.05)
  })
  
  # --- fit dist
  output$distPlot <- renderPlot(plot_dist())
  output$gofTable <- renderDataTable(table_gof())
  
  # --- predict
  output$modelAveragePlot <- renderPlot(plot_model_average())
  output$hazardConc <- renderPrint(describe_hazard_conc())
  
    
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
  
  # information
  observeEvent(input$information,
               {showModal(modalDialog("Here is where we put technical details about how the models are fit, etc.",
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it")))
               })
 
  }
  
  
  
  
  
  
  