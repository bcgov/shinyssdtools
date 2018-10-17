ui = tagList(shinyjs::useShinyjs(),  
             tags$head(includeCSS("style.css")),
             navbarPage( title = "Fit and Plot Species Sensitivity Distributions",
                         tabPanel(title = span(tagList(icon("table"), "1. Data")),
                                  fluidRow(br(),
                                           column(4,
                                                  h5("Choose one of the following options:"),
                                                  inline(p("1. Use ")),
                                                  inline(actionLink("demoData", label = "boron dataset", icon = icon('table'))),
                                                  inline(actionLink('infoDemo', icon = icon('info-circle'), label = NULL)),
                                                  shinyjs::hidden(div(id = "infoDemoText", 
                                                                      helpText("This can be used to demo the app or view a dataset that 'works'."))),
                                                  # upload csv with data
                                                  br(),
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
                                                  rHandsontableOutput("hot")),
                                           column(8,
                                                  h5("Preview chosen dataset:"),
                                                  wellPanel(dataTableOutput('viewUpload', width = 600), 
                                                            style = "overflow-x:scroll; max-height: 600px; max-width: 640px"))),
                                  div(id = 'note', helpText("Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value."))),
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
                                                              'persist' = FALSE)),
                                             hr(),
                                             h5("Format png"),
                                             inline(numericInput('selectWidth2', label = 'Width', min = 1, max = 20, step = 1, value = 8)),
                                             inline(numericInput('selectHeight2', label = 'Height', min = 1, max = 20, step = 1, value = 6)),
                                             inline(numericInput('selectDpi2', label = 'Dpi', min = 50, max = 3000, step = 50, value = 300)))),
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
                                             hint("10,000 bootstrap samples recommended"),
                                             br(), br(),
                                             inline(numericInput("selectHc", label = "Threshold (%)", value = 5, min = 0, 
                                                                 max = 99, step = 5, width = "100px")),
                                             inline(selectInput('bootSamp', label = "Bootstrap samples", 
                                                                choices = c("500", "1,000", "5,000", "10,000"),
                                                                selected = "10,000",
                                                                width = "130px")),
                                             hr(),
                                             h5("Format plot"),
                                             uiOutput('selectLabel'),
                                             uiOutput('selectColour'),
                                             uiOutput('selectShape'),
                                             textInput('xaxis', value = "Concentration", label = "x-axis label"),
                                             textInput('yaxis', value = "Percent of Species Affected", label = "y-axis label"),
                                             textInput('title', value = "", label = "Plot title"),
                                             inline(numericInput('adjustLabel', value = 1.3, label = "Adjust label",
                                                                 min = 1, max = 10, step = 0.1)),
                                             inline(checkboxInput('checkHc', label = "HC Estimate", value = TRUE)),
                                             hr(),
                                             h5("Format png"),
                                             inline(numericInput('selectWidth', label = 'Width', min = 1, max = 20, step = 1, value = 8)),
                                             inline(numericInput('selectHeight', label = 'Height', min = 1, max = 20, step = 1, value = 6)),
                                             inline(numericInput('selectDpi', label = 'Dpi', min = 50, max = 3000, step = 50, value = 600)),
                                             uiOutput('expandX'))),
                                    column(8,
                                           br(),
                                           conditionalPanel(
                                             condition = "output.checkpred",
                                             htmlOutput('hintPr')
                                           ),
                                           conditionalPanel(
                                             condition = "input.expandX",
                                             htmlOutput('hintEx')
                                           ),
                                           conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             h5("Plot model average and estimate hazard concentration")
                                           ),
                                           inline(conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             downloadButton("dlPredPlot", label = "plot .png", 
                                                            style = 'padding:4px; font-size:80%'))),
                                           inline(conditionalPanel(
                                             condition = "output.clTable",
                                             downloadButton("dlPredTable", label = "table .csv", 
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
                                  div(id = 'codes',
                                      wellPanel(
                                        uiOutput('codeHead'),
                                        br(),
                                        uiOutput('codeData'),
                                        br(),
                                        uiOutput('codeFit'),
                                        br(),
                                        uiOutput('codeSaveFit'),
                                        br(),
                                        uiOutput('codePredPlot'),
                                        br(),
                                        uiOutput('codeSavePred'),
                                        br(),
                                        uiOutput('codePredCl'))
                                  )),
                         tabPanel(title = "About",
                                  br(),
                                  wellPanel(
                                    HTML("This is a draft and may change at some point in the future.<br><br>",
                                         tech.info)),
                                    br(),
                                  p(HTML("If you have any questions or concerns you can"), actionLink('contactUs', label = 'message us.')),
                                  shinyjs::hidden(div(id = 'feedbackForm',
                                        br(),
                                        wellPanel(textInput("name", "Name (optional):", width = "50%"),
                                                  textInput("email", "Email (optional):", width = "50%"),
                                                  textAreaInput("comment", label_mandatory("Comment:"), width = "100%", height = '100px'),
                                                  actionButton("submit_feedback", "Submit"))))),
                         tabPanel(title = "User Guide",
                                  includeHTML('user-guide/user-guide.html'))
             ))




