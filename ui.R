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
      actionLink("demoData", label = "Use demo data", icon = icon('table')),
      # upload csv with data
      fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
                label = "", placeholder = "Upload your data...",
                accept = c('.csv')),
      
      # select which column is concentration
      uiOutput('selectConc'),
      uiOutput('selectSpp'),
      uiOutput('selectGroup'),
      uiOutput('selectDist'),
      actionButton("go", "Update"),
      br(), br(),
      actionLink("information", label = "Technical info", icon = icon('info-circle')),
      br(),
      actionLink("feedback", label = "Feedback?", icon = icon("comment"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), "Fit")),
                           br(),
                           inline(downloadButton("dlDistPlot", label = "plot", style = 'padding:4px; font-size:80%')),
                           inline(downloadButton("dlGofTable", label = "table", style = 'padding:4px; font-size:80%')),
                           br(), br(),
                           plotOutput("distPlot"),
                           br(),
                           dataTableOutput("gofTable")),
                  tabPanel(title = span(tagList(icon("calculator"), "Predict")), 
                           br(),
                           inline(downloadButton("dlModelPlot", label = "plot", style = 'padding:4px; font-size:80%')),
                           inline(downloadButton("dlPredTable", label = "table", style = 'padding:4px; font-size:80%')),
                           br(), br(),
                           plotOutput("modelAveragePlot"),
                           br(),
                           inline(htmlOutput("text1")),
                           inline(uiOutput("selectHc")),
                           inline(htmlOutput("text2")),
                           inline(htmlOutput("estHc")))
      )
    )
  )
)
