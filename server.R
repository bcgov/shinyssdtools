# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  ########### Reactives --------------------
  
  # --- upload data
  upload.values <- reactiveValues(
    upload_state = NULL
  )
  
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
  
  clean_data <- reactive({
    data <- read_data()
    if(length(data)) {
      # remove any column names like X1, X2 (blank headers from excel/numbers)
      data[,colnames(data) %in% paste0("X", 1:200)] <- NULL
      # remove any rows with all NA
      data <- data[!(rowSums(is.na(data) | data == "") == ncol(data)),]
      # remove any cols with all NA
      # data <- data[,colSums(is.na(data) | data == "") < nrow(data)]
    }
    print(data)
      data
      
    })
  
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
      return("At least one distribution mut be selected.")
    ""
  })
  
  output$hintFi <- renderText(check_fit())
  output$hintPr <- renderText(check_fit())
  output$hintFi <- renderText(check_fit())
  
  
  names_data <- reactive({
    data <- clean_data()
    names(data) %<>% make.names
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
    print(clean_data())
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
  
  # --- create feedback
  
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
                label = label_mandatory("Select column with concentration values:"), 
                choices = column_names(),
                selected = guess_conc())
  })
  
  output$selectSpp = renderUI({
    selectInput("selectSpp", 
                label = "Label by:", 
                choices = c("-none-", column_names()),
                selected = guess_spp())
  })
  
  output$selectGroup = renderUI({
    selectInput("selectGroup", 
                label = "Colour by:", 
                choices = c("-none-", column_names()),
                selected = "-none-")
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
  
  output$estHc <- renderUI({HTML( paste0("<b>", describe_hc()$est, "</b>"))})
  
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
    plot_model_average()
  })
  
  # --- download handlers
  
  ########### Observers --------------------
  # --- data upload
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





