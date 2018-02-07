
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
                label = "Select column with concentrations:", 
                choices = column_names())
  })
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Select column with species names:", 
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
  
  
 
  }
  
  
  
  
  
  
  