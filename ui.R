# Define UI
fluidPage(
  shinyjs::useShinyjs(),
  shinythemes::themeSelector(),
  # shinyjs::inlineCSS(appCSS),
  # tags$head(
  #   # Include custom CSS
  #   includeCSS("style.css")
  # ),
  # App title ----
  titlePanel("Fit and Plot Species Sensitivity Distributions"),
  
  tabsetPanel(type = "tabs",
              tabPanel(title = span(tagList(icon("table"), "1. Data")),
                       fluidRow(
                         column(5,
                                h5("Choose one of the following options:"),
                                hr(),
                                inline(p("1. Use ")),
                                inline(actionLink("demoData", label = "boron dataset", icon = icon('table'))),
                                helpText("This can be used to demo the app or view a dataset in the correct format."),
                                # upload csv with data
                                hr(),
                                p("2. Upload a csv file"),
                                helpText("If you have an xls/xlsx file, try exporting a worksheet as csv from excel."),
                                
                                fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
                                          label = "", placeholder = "Upload your data...",
                                          accept = c('.csv')),
                                # input data in DataTable
                                hr(),
                                p("3. Fill out Spreadsheet"),
                                helpText("Right-click on the table to delete/insert rows or columns.", "Column names cannot be changed.", "Species and Group are optional."),
                                rHandsontableOutput("hot"),
                                hr()),
                         column(7,
                                h5("Preview chosen dataset:"),
                                hr(),
                                wellPanel(dataTableOutput('viewUpload', width = 500), 
                                          style = "overflow-x:scroll; max-height: 600px; max-width: 540px")))),
              tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), "2. Fit")),
                       fluidRow(
                         column(4,
                                br(),
                                helpText("Hint:"),
                                wellPanel(
                                  textOutput('hintFi')
                                ),
                                wellPanel(
                                  uiOutput('selectConc'),
                                  uiOutput('selectDist'))
                                ),
                                
                         column(8,
                                br(),
                                inline(downloadButton("dlDistPlot", label = "plot", style = 'padding:4px; font-size:80%')),
                                inline(downloadButton("dlGofTable", label = "table", style = 'padding:4px; font-size:80%')),
                                plotOutput("distPlot"),
                                dataTableOutput("gofTable")))),
              tabPanel(title = span(tagList(icon("calculator"), "3. Predict")), 
                       fluidRow(
                         column(4,
                                br(),
                                helpText("Having trouble?"),
                                wellPanel(
                                  htmlOutput('hintPr')
                                ),
                                wellPanel(
                                  uiOutput('selectSpp'),
                                  uiOutput('selectGroup'),
                                  numericInput("selectHc", label = "Hazard Concentration:", value = 5, min = 0, 
                                               max = 99, step = 5, width = "70px"))),
                       column(8,
                              br(),
                              inline(downloadButton("dlModelPlot", label = "plot", style = 'padding:4px; font-size:80%')),
                              inline(downloadButton("dlPredTable", label = "table", style = 'padding:4px; font-size:80%')),
                              plotOutput("modelAveragePlot"),
                              br(),
                              inline(htmlOutput("estHc"))))),
              tabPanel(title = span(tagList(icon("calculator"), "4. Confidence Interval")), 
                       br(),
                       helpText("Having trouble?"),
                       wellPanel(
                         htmlOutput('hintCi')
                       )),
              tabPanel(title = span(tagList(icon("code"), "Rcode")), 
                       br())
  )
)
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       actionLink("demoData", label = "Use demo data", icon = icon('table')),
#       # upload csv with data
#       fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
#                 label = "", placeholder = "Upload your data...",
#                 accept = c('.csv')),
#       
#       # select which column is concentration
#       uiOutput('selectConc'),
#       uiOutput('selectSpp'),
#       uiOutput('selectGroup'),
#       uiOutput('selectDist'),
#       actionButton("go", "Update"),
#       br(), br(),
#       actionLink("information", label = "Technical info", icon = icon('info-circle')),
#       br(),
#       actionLink("feedback", label = "Feedback?", icon = icon("comment"))
#       
#     ),
#     )
#   )
# )
# 
# 
