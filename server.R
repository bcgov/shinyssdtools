
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  # Reactives --------------------

  # --- read, clean and check data
  read_data <- reactive({
    req(input$uploadData)
    data <- input$uploadData
    if(is.null(data)) {return(NULL)}
    if(!grepl(".csv", data$name, fixed = TRUE))  
      return(create_error("We're not sure what to do with that file type. Please upload a csv."))
    if(!grepl(".csv", data$name, fixed = TRUE)) 
      return(NULL)
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
    req(input$go)
    
    conc <- isolate(input$selectConc)
    spp <- isolate(input$selectSpp)
    dist <- isolate(input$selectDist)
    
    data <- clean_data()
    
    if(!is.numeric(data[[conc]]))
      return(create_error("Concentration column must contain numbers."))
    if(any(data[[conc]] < 0))
      return(create_error("Concentration values must be positive."))
    if(any(duplicated(data[[spp]])))
      return(create_error("Some species are duplicated. This app only handles one chemical at a time and each species should only have one concentration value."))
    if(any(is.na(conc)))
      return(create_error("No missing concentation values allowed."))
    data 
  })

  
  column_names <- reactive({
    names(clean_data()) 
  })
  
  guess_conc <- reactive({
    name <- column_names()
    name[stringr::str_detect(name %>% tolower, "conc")][1]
  })
  
  guess_spp <- reactive({
    name <- column_names()
    name[stringr::str_detect(name %>% tolower, "sp")][1]
  })
  
  # --- fit distributions
  fit_dist <- reactive({
      data <- check_data()
      dist <-  isolate(ssdca::ssd_fit_dists(data, left = input$selectConc,
                                            dists = input$selectDist, silent = TRUE))
  })
  
  plot_dist <- reactive({
    if(is.null(check_data()))
      return(NULL)
      autoplot(fit_dist())
  })
    
  table_gof <- reactive({
    gof <- ssdca::ssd_gof(fit_dist()) %>% dplyr::mutate_if(is.numeric, ~ round(., 2))
  })
  
  # --- predict and model average
  predict_hc <- reactive({
    withProgress(value = 0, message = "This'll take a minute...", {
      incProgress(0.3)
                   dist <- fit_dist()
                   incProgress(amount = 0.6)
                   pred <- predict(dist) 
                 })
  })
  
  plot_model_average <- reactive({
    if(is.null(check_data())) 
      return(NULL)
    data <- check_data()
    pred <- predict_hc()
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
  
  # --- create feedback
  ssdca_shiny_feedback <- reactive({
    c(Name = input$name,
           Email = input$email,
           Comment = input$comment)
  })
  
  # Outputs --------------------
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
      plot_dist()
  })
    
  output$gofTable <- renderDataTable({ 
    req(input$go)
    datatable(table_gof(), options = list(paging = FALSE, sDom  = '<"top">lrt<"bottom">ip'))})
  
  # --- predict
  output$modelAveragePlot <- renderPlot({
    req(input$go)
   plot_model_average()
  })
  
  # --- describe results
  output$estHc <- renderUI({
    req(input$go)
    HTML(paste0("<b>", describe_hazard_conc()$est, "<b>"))})
  output$lowerHc <- renderUI({
    req(input$go)
    HTML(paste0("<b>", describe_hazard_conc()$lower, "<b>"))})
  output$upperHc <- renderUI({
    req(input$go)
    HTML(paste0("<b>", describe_hazard_conc()$upper, "<b>"))})
  output$text1 <- renderUI({
    req(input$go)
    HTML("The model average estimate of the concentration that affects")})
  output$selectHc <- renderUI({
    req(input$go)
    numericInput("selectHc", label = NULL, value = 5, min = 0, 
                                            max = 99, step = 5, width = "70px")})
  output$text2 <- renderUI({
    req(input$go)
    HTML("% of species is")})
  output$text3 <- renderUI({
    req(input$go)
    HTML("but it could be as low as")})
  output$text4 <- renderUI({
    req(input$go)
    HTML("or as high as")})
  
  # --- download handlers
  output$dlDistPlot <- downloadHandler(
    filename = function() { "ssdca_distFitPlot.png"},
    content = function(file) {
      ggsave(file, plot = plot_dist(), device = "png")
    }
  )
  
  output$dlModelPlot <- downloadHandler(
    filename = function() { "ssdca_modelAveragePlot.png"},
    content = function(file) {
      ggsave(file, plot = plot_model_average(), device = "png")
    }
  )
  
  output$dlGofTable <- downloadHandler(
    filename = function() {"ssdca_distGofTable.csv"},
    content <- function(file) {
      readr::write_csv(tibble(a = 1), file)
    }
  )
  
  output$dlPredTable <- downloadHandler(
    filename = function() {"ssdca_distGofTable.csv"},
    content <- function(file) {
      readr::write_csv(predict_hc(), file)
    }
  )
    
  # Observers --------------------
  # --- feedback
  observeEvent(input$feedback,
               {showModal(modalDialog(title = "", 
                                      size = "m", easyClose = T,
                                      footer = modalButton("Never mind"),
                                      textInput("name", "Name (optional):", width = "30%"),
                                      textInput("email", "Email (optional):", width = "30%"),
                                      textInput("comment", label_mandatory("Comment:"), width = "100%"),
                                      actionButton("submit_feedback", "Submit")))})
  
  observeEvent(input$submit_feedback,
               {slackr(ssdca_shiny_feedback())
                 removeModal()})
  
  # --- information
  observeEvent(input$information,
               {showModal(modalDialog("Here is where we put technical details about how the models are fit, etc.",
                                      size = "m", easyClose = T,
                                      footer = modalButton("Got it")))})
}
  


  
  
  
  
  
  