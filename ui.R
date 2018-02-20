# Define UI
fluidPage(
  shinyjs::useShinyjs(),
  # shinythemes::themeSelector(),
  shinyjs::inlineCSS(appCSS),
  tags$head(
    # Include custom CSS
    includeCSS("style.css")
  ),                                                                                                                                                                                                                                                                                                             
  # App title ----
  titlePanel("Fit and Plot Species Sensitivity Distributions"),
  actionLink("information", label = "Technical info", icon = icon('info-circle')),
  actionLink("feedback", label = "Feedback", icon = icon("comment")),
  br(), br(),
  tabsetPanel(type = "tabs",
              tabPanel(title = span(tagList(icon("table"), "1. Data")),
                       fluidRow(helpText("   Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value."),

                         column(5,
                                h5("Choose one of the following options:"),
                                hr(),
                                inline(p("1. Use ")),
                                inline(actionLink("demoData", label = "boron dataset", icon = icon('table'))),
                                inline(actionLink('infoDemo', icon = icon('info-circle'), label = NULL)),
                                shinyjs::hidden(div(id = "infoDemoText", 
                                                    helpText("This can be used to demo the app or view a dataset that 'works'."))),
                                # upload csv with data
                                hr(),
                                inline(p("2. Upload a csv file")),
                                inline(actionLink("infoUpload", icon = icon("info-circle"), label = NULL)),
                                shinyjs::hidden(div(id = "infoUploadText", 
                                                    helpText("Upload a csv file containing your dataset. The dataset must include a column with with at least 8 distinct, positive, non-missing, numeric concentration values.", 
                                                             "Other useful but optional variables include species and group, which may be used to label and color plot output, respectively.",
                                                             "If you have an xls/xlsx file, try exporting a worksheet to csv using excel."))),
                                fileInput('uploadData', buttonLabel = span(tagList(icon("upload"), "csv")),
                                          label = "", placeholder = "Upload your data...",
                                          accept = c('.csv')),
                                
                                # input data in DataTable
                                hr(),
                                inline(p("3. Fill out table below")),
                                inline(actionLink("infoHands", icon = icon('info-circle'), label = NULL)),
                                shinyjs::hidden(div(id = "infoHandsText", 
                                                    helpText("The table below is interactive and acts like a simple excel spreadsheet.",
                                                             "Click on a cell to begin data input.",
                                                             "Right-click on the table to delete/insert rows or columns.", 
                                                             "Column names cannot be changed.", 
                                                             "The Concentration column must be filled out, 
                                                             with at least 8 distinct, positive, non-missing, numeric values.",
                                                             "Species and Group are optional and may be used to format plot outputs.",
                                                             "If the table is behaving unexpectedly, please reload the website."))),
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
                                                 label = label_mandatory("Select distributions to fit"),
                                                 multiple = TRUE, 
                                                 choices = c(default.dists, extra.dists),
                                                 selected = default.dists,
                                                 options = list(
                                                   'plugins' = list('remove_button'),
                                                   'create' = TRUE,
                                                   'persist' = FALSE)))),
                         column(8,
                                br(),
                                conditionalPanel(
                                  condition = "output.checkfit",
                                  htmlOutput('hintFi')
                                ),
                                conditionalPanel(
                                  condition = "output.distPlot",
                                  h5("Plot fitted distributions")
                                ),
                                
                                inline(conditionalPanel(
                                  condition = "output.distPlot",
                                  downloadButton("dlFitPlot", label = "plot .png", 
                                                 style = 'padding:4px; font-size:80%'))),
                                inline(conditionalPanel(
                                  condition = "output.distPlot",
                                  downloadButton("dlFitTable", label = "table .csv", 
                                                 style = 'padding:4px; font-size:80%'))),
                         br(), br(),
                         conditionalPanel(
                           condition = "output.distPlot",
                           htmlOutput('fitFail')
                         ),
                                plotOutput("distPlot"),
                         br(),
                         conditionalPanel(
                           condition = "output.gofTable",
                           h5("Goodness of fit table")
                         ),
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
                              conditionalPanel(
                                condition = "output.checkpred",
                                htmlOutput('hintPr')
                              ),
                              conditionalPanel(
                                condition = "output.modelAveragePlot",
                                h5("Plot model average and estimate hazard concentration")
                              ),
                              inline(conditionalPanel(
                                condition = "output.modelAveragePlot",
                                downloadButton("dlPredPlot", label = "plot .png", 
                                               style = 'padding:4px; font-size:80%'))),
                              br(), 
                              plotOutput("modelAveragePlot"),
                              br(),
                              conditionalPanel(
                                condition = "output.modelAveragePlot",
                                htmlOutput("estHc")
                              ),
                              hr(),
                              inline(conditionalPanel(
                                condition = "output.modelAveragePlot",
                                h5("Get confidence limits")
                              )),
                              inline(conditionalPanel(
                                condition = "output.modelAveragePlot",
                                actionLink("infoCl", icon = icon('info-circle'), label = NULL)
                                )),
                              shinyjs::hidden(div(id = "clInfoText", helpText("Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimated hazard concentration and selected % threshold.",
                                                                              "To calculate CL for a different % threshold or number of bootstrap samples, simply select new values in the sidebar and click 'Get CL' again."))),
                              conditionalPanel(
                                condition = "output.modelAveragePlot",
                                htmlOutput('describeCl')
                              ),
                              inline(conditionalPanel(
                                condition = "output.modelAveragePlot",
                                actionButton('getCl', label = "Get CL")
                              )),
                              conditionalPanel(
                                condition = "output.modelAveragePlot",
                                dataTableOutput('clTable')
                              )))),
              tabPanel(title = span(tagList(icon("code"), "R code")), 
                       br(),
                       helpText("Copy and paste code below to reproduce results. Code is added as functions are executed within the app.",
                                "(e.g., code for generating confidence limits will appear after 'Get CL' is clicked.)"),
                       wellPanel(
                         uiOutput('codeHead'),
                         br(),
                         uiOutput('codeData'),
                         br(),
                         uiOutput('codeFit'),
                         br(),
                         uiOutput('codePredPlot'),
                         br(),
                         uiOutput('codePredCl'))
                       )
                      
))

