# Define UI
fluidPage(
  shinyjs::useShinyjs(),
  shinythemes::themeSelector(),
  shinyjs::inlineCSS(appCSS),
  tags$head(
    # Include custom CSS
    includeCSS("style.css")
  ),
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
                                inline(actionLink('infoDemo', icon = icon('info-circle'), label = NULL)),
                                shinyjs::hidden(div(id = "infoDemoText", 
                                                    helpText("This can be used to demo the app or view a dataset in the correct format."))),
                                # upload csv with data
                                hr(),
                                inline(p("2. Upload a csv file")),
                                inline(actionLink("infoUpload", icon = icon("info-circle"), label = NULL)),
                                shinyjs::hidden(div(id = "infoUploadText", 
                                                    helpText("Upload an external dataset here. The dataset must include a column with with at least 8 distinct, positive, non-missing, numeric concentration values.", 
                                                             "Other useful but optional variables include species and group, which may be used to label and color plot output.",
                                                             "If you have an xls/xlsx file, try exporting a worksheet as csv from excel."))),
                                fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
                                          label = "", placeholder = "Upload your data...",
                                          accept = c('.csv')),
                                
                                # input data in DataTable
                                hr(),
                                inline(p("3. Fill out table below")),
                                inline(actionLink("infoHands", icon = icon('info-circle'), label = NULL)),
                                shinyjs::hidden(div(id = "infoHandsText", 
                                                    helpText("The table below is interactive and is similar to an excel spreadsheet.",
                                                             "Click on a cell to begin data input.",
                                                             "Right-click on the table to delete/insert rows or columns.", 
                                                             "Column names cannot be changed.", 
                                                             "The Concentration column must be filled out, 
                                                             with at least 8 distinct, positive, non-missing, numeric values.",
                                                             "Species and Group are optional and may be used to format plot outputs."))),
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
                                wellPanel(
                                  uiOutput('selectConc'),
                                  selectizeInput('selectDist', 
                                                 label = label_mandatory("Select distributions to fit:"),
                                                 multiple = TRUE, 
                                                 choices = c(default.dists, extra.dists),
                                                 selected = default.dists,
                                                 options = list(
                                                   'plugins' = list('remove_button'),
                                                   'create' = TRUE,
                                                   'persist' = FALSE)))),
                         column(8,
                                br(),
                                inline(downloadButton("dlDistPlot", label = "plot .png", style = 'padding:4px; font-size:80%')),
                                inline(downloadButton("dlGofTable", label = "table .csv", style = 'padding:4px; font-size:80%')),
                                br(), br(),
                                conditionalPanel(
                                  condition = "output.checkfit",
                                  htmlOutput('hintFi')
                                ),
                                plotOutput("distPlot"),
                                dataTableOutput("gofTable")))),
              tabPanel(title = span(tagList(icon("calculator"), "3. Predict")), 
                       fluidRow(
                         column(4,
                                br(),
                                wellPanel(
                                  h5("Estimate hazard concentration"),
                                  inline(numericInput("selectHc", label = "Threshold (%)", value = 5, min = 0, 
                                               max = 99, step = 5, width = "100px")),
                                  inline(selectInput('bootSamp', label = "Bootstrap samples", 
                                                     choices = c("500", "1,000", "5,000", "10,000"),
                                                     width = "130px")),
                                  hr(),
                                  h5("Format plot"),
                                  uiOutput('selectSpp'),
                                  uiOutput('selectGroup'),
                                  textInput('xaxis', value = "Concentration", label = "x-axis label"),
                                  textInput('yaxis', value = "Percent of Species Affected", label = "y-axis label"),
                                  textInput('title', value = "", label = "Plot title"),
                                  numericInput('adjustLabel', value = 1.3, label = "Adjust label position",
                                               min = 1, max = 10, step = 0.1))),
                       column(8,
                              br(),
                              h5("Plot model average and estimate hazard concentration"),
                              inline(downloadButton("dlModelPlot", label = "plot .png", style = 'padding:4px; font-size:80%')),
                              inline(downloadButton("dlPredTable", label = "table .csv", style = 'padding:4px; font-size:80%')),
                              br(), br(),
                              conditionalPanel(
                                condition = "output.checkpred",
                                htmlOutput('hintPr')
                              ),
                              plotOutput("modelAveragePlot"),
                              br(),
                              htmlOutput("estHc"),
                              hr(),
                              inline(h5("Get confidence limits")),
                              inline(actionLink("infoCl", icon = icon('info-circle'), label = NULL)),
                              shinyjs::hidden(div(id = "clInfoText", helpText("Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimated hazard concentration and selected % threshold.",
                                                        "To calculate CL for a different % threshold or number of bootstrap samples, simply select new values in the sidebar and click 'Get CL' again."))),
                              
                              htmlOutput('describeCi'),
                              br(),
                              inline(actionButton('getCl', label = "Get CL")),
                              dataTableOutput('clTable')
                              ))),
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
