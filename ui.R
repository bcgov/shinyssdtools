# Define UI
fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  tags$head(
    # Include custom CSS
    includeCSS("style.css")
  ),
  # App title ----
  titlePanel("Fit and Plot Species Sensitivity Distributions"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # upload csv with data
      fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
                label = "", placeholder = "Upload your data...",
                accept = c('.csv')),
      
      # select which column is concentration
      uiOutput('selectConc'),
      uiOutput('selectSpp'),
      uiOutput('selectDist'),
      br(),
      actionLink("information", label = "Technical info", icon = icon('info-circle')),
      br(),
      actionLink("feedback", label = "Feedback?", icon = icon("comment"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = span(tagList(icon("columns"), "Fit")),
                           br(),
                           inline(tags$body("Hint: here is where we might put helpful hints if plot isn't loading")),
                           # inline(textOutput("hint")),
                           plotOutput("distPlot"),
                           br(),
                           DT::dataTableOutput("gofTable")),
                  tabPanel(title = span(tagList(icon("calculator"), "Predict")), 
                           br(),
                           plotOutput("modelAveragePlot"),
                           br(),
                           inline(tags$body("The model average estimate of the concentration that affects")),
                           inline(numericInput("selectHc", label = NULL, value = 5, min = 0, 
                                               max = 99, step = 5, width = "70px")),
                           inline(tags$body("% of the species is ")),
                           inline(htmlOutput("estHc")),
                           inline(tags$body("but it could be as low as")),
                           inline(htmlOutput("lowerHc")),
                           inline(tags$body("or as high as")),
                           inline(htmlOutput("upperHc"))),
                  tabPanel(title = span(tagList(icon("code"), "Rcode")), 
                           verbatimTextOutput("code"))
                  
                  
      )
    )
  )
)

  
# shinyWidgets::pickerInput(
#   inputId = "selectDist", 
#   label = "Select distributions to fit", 
#   choices = full.dists,
#   selected = default.dists,
#   options = list(
#     `actions-box` = TRUE, 
#     size = 10,
#     `selected-text-format` = "count > 3"
#   ), 
#   multiple = TRUE
# ),
# verbatimTextOutput("selectedDist",  placeholder = TRUE)

