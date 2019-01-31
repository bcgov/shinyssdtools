ui = tagList(shinyjs::useShinyjs(),  
             tags$head(includeCSS("style.css")),
             div(class = "div-link", 
                 HTML(paste0(div("Select language: ", class = 'msw-label'),
                         actionButton("en", "EN", class = 'msw-button'),
                         "/",
                         actionButton("fr", "FR", class = 'msw-button')
             ))),
             navbarPage( title = uiOutput("ui_navtitle"),
                         tabPanel(title = span(tagList(icon("table"), uiOutput("nav_1"))),
                                  fluidRow(br(),
                                           column(4,
                                                  uiOutput("ui_1choose"),
                                                  inline(uiOutput("ui_1data")),
                                                  inline(actionLink('infoDemo', icon = icon('info-circle'), label = NULL)),
                                                  shinyjs::hidden(div(id = "infoDemoText", 
                                                                      uiOutput("ui_1datahelp"))),
                                                  # upload csv with data
                                                  br(),
                                                  inline(uiOutput("ui_1csv")),
                                                  inline(actionLink("infoUpload", icon = icon("info-circle"), label = NULL)),
                                                  shinyjs::hidden(div(id = "infoUploadText", 
                                                    uiOutput("ui_1csvhelp"))),
                                                  uiOutput("ui_1csvupload"),
                                                  
                                                  # input data in DataTable
                                                  inline(uiOutput("ui_1table")),
                                                  inline(actionLink("infoHands", icon = icon('info-circle'), label = NULL)),
                                                  shinyjs::hidden(div(id = "infoHandsText", 
                                                                      uiOutput("ui_1tablehelp"))),
                                                  rHandsontableOutput("hot")),
                                           column(8,
                                                  uiOutput("ui_1preview"),
                                                  wellPanel(dataTableOutput('viewUpload', width = 600), 
                                                            style = "overflow-x:scroll; max-height: 600px; max-width: 640px"))),
                                  div(id = 'note', 
                                      uiOutput("ui_1note"))),
                         tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), uiOutput("nav_2"))),
                                  fluidRow(
                                    column(4,
                                           br(),
                                           wellPanel(
                                             uiOutput('selectConc'),
                                             uiOutput("ui_2select"),
                                             br(),
                                             uiOutput("ui_2png"),
                                             shinyjs::hidden(div(id = 'divFormatFit',
                                                                 br(),
                                                                 inline(uiOutput("ui_2width")),
                                                                 inline(uiOutput("ui_2height")),
                                                                 inline(uiOutput("ui_2dpi")))))),
                                    column(8,
                                           br(),
                                           conditionalPanel(
                                             condition = "output.checkfit",
                                             htmlOutput('hintFi')
                                           ),
                                           conditionalPanel(
                                             condition = "output.distPlot",
                                             uiOutput("ui_2plot")
                                           ),
                                           inline(conditionalPanel(
                                             condition = "output.distPlot",
                                           uiOutput("ui_2dlplot"))),
                                           inline(conditionalPanel(
                                             condition = "output.distPlot",
                                           uiOutput("ui_2dltable"))),
                                           br(), br(),
                                           conditionalPanel(
                                             condition = "output.distPlot",
                                             htmlOutput('fitFail')
                                           ),
                                           withSpinner(plotOutput("distPlot")),
                                           br(),
                                           conditionalPanel(
                                             condition = "output.gofTable",
                                             uiOutput("ui_2table")
                                           ),
                                           dataTableOutput("gofTable")))),
                         tabPanel(title = span(tagList(icon("calculator"), uiOutput("nav_3"))), 
                                  fluidRow(
                                    column(4,
                                           br(),
                                           wellPanel(
                                             uiOutput("ui_3est"),
                                             uiOutput("ui_3bshint"),
                                             br(), br(),
                                             inline(uiOutput("ui_3thresh")),
                                             inline(uiOutput("ui_3samples")),
                                             br(),
                                             uiOutput('selectLabel'),
                                             uiOutput('selectColour'),
                                             uiOutput('selectShape'),
                                             uiOutput("ui_plotopts"),
                                             shinyjs::hidden(div(id = 'divFormatPredict',
                                                                 br(),
                                                                
                                                                 selectInput('selectPalette', label = 'Colour palette', choices = pals, selected = pals[2]),
                                                                 textInput('xaxis', value = "Concentration", label = "x-axis label"),
                                                                 textInput('yaxis', value = "Percent of Species Affected", label = "y-axis label"),
                                                                 textInput('title', value = "", label = "Plot title"),
                                                                 uiOutput('uiLegendColour'),
                                                                 uiOutput('uiLegendShape'),
                                                                 splitLayout(
                                                                   uiOutput('uiXmax'),
                                                                   numericInput('adjustLabel', value = 1.3, label = "Adjust label",
                                                                                min = 1, max = 10, step = 0.1)
                                                                 ),
                                                                   inline(checkboxInput('checkHc', label = "HC Estimate", value = TRUE)) 
                                                                 )),
                                             br(), br(),
                                             actionLink('linkPngFormatPredict', label = "Png file formatting options"),
                                             shinyjs::hidden(div(id = "divPngFormatPredict",
                                                                 br(),
                                                                 inline(numericInput('selectWidth', label = 'Width', min = 1, max = 20, step = 1, value = 8)),
                                                                 inline(numericInput('selectHeight', label = 'Height', min = 1, max = 20, step = 1, value = 6)),
                                                                 inline(numericInput('selectDpi', label = 'Dpi (resolution)', min = 50, max = 3000, step = 50, value = 600)),
                                                                 uiOutput('expandX'))), class = "wellpanel")),
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
                                           withSpinner(plotOutput("modelAveragePlot")),
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
                         tabPanel(title = span(tagList(icon("code"), uiOutput("nav_code"))), 
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
                         tabPanel(title = uiOutput("nav_about"),
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
                         tabPanel(title = uiOutput("nav_guide"),
                                  br(),
                                  includeHTML("user-guide/user-guide.html"))
             ))




