
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  # Reactives --------------------

  # --- read file upload into tibble 
  get_data <- reactive({
    data <- input$uploadData
    if(is.null(data)) {return(NULL)}
    if(!grepl(".csv", data$name, fixed = TRUE)) {return(NULL)}
    isolate(readr::read_csv(data$datapath))
  })
  
  # --- clean common problems to avoid errors
  clean_data <- reactive({
    data <- get_data()
    if(length(data)) {
      # remove any column names like X1, X2 (blank headers from excel/numbers)
      data[,colnames(data) %in% paste0("X", 1:200)] <- NULL
      # remove any rows with all NA
      data <- data[!(rowSums(is.na(data)) == ncol(data)),] 
    }
    data
  })
  
  # --- check that data conforms to requirements of ssd_fit_dist
  check_data <- reactive({
    data <- clean_data()
  })
  
  column_names <- reactive({
    names(clean_data()) %>% print
  })
  
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
  
  # Outputs --------------------
  # output$selectedDist <- renderPrint({input$selectDist%>% paste("\n") %>% glue::collapse()})
  output$distPlot <- renderPlot(plot_dist())
  output$gofTable <- renderDataTable(table_gof())
    
  # Observers --------------------
  observeEvent(input$fit, {
    plot_dist()
  })
  
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
  
  
  
  
  
  
  