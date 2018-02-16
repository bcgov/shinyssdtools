# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  ########### Reactives --------------------
  
  # --- upload data
  upload.values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$uploadData, {
    upload.values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$demoData, {
    upload.values$upload_state <- 'demo'
  })
  
  observeEvent(input$useHot, {
    upload.values$upload_state <- 'hot'
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
    } else if (upload.values$upload_state == 'hot') {
      return()
    } })
  
  hot.values = reactiveValues()
  
  hot_data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(hot.values[["DF"]]))
        DF = data.frame(Concentration = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
                        Species = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_))
      else
        DF = hot.values[["DF"]]
    }
    
    hot.values[["DF"]] = DF
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = hot_data()
    if (!is.null(DF))
      rhandsontable(DF, width = 600, )
  })
  # --- render column choices

  # --- fit distributions
  
  # --- predict and model average

  # --- create feedback
  
  ########### Outputs --------------------
  # --- render UI with choices based on file upload
  
  # --- download handlers
  
  ########### Observers --------------------
  
  # --- describe results
  
  # --- feedback
  
  # --- information
 
}


