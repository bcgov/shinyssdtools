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
                                             uiOutput("ui_3plotopts"),
                                             shinyjs::hidden(div(id = 'divFormatPredict',
                                                                 br(),
                                                                uiOutput("ui_3pal"),
                                                                uiOutput("ui_3xlab"),
                                                                uiOutput("ui_3ylab"),
                                                                uiOutput("ui_3title"),
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
                                             uiOutput("ui_3pngopts"),
                                             shinyjs::hidden(div(id = "divPngFormatPredict",
                                                                 br(),
                                                                 uiOutput("ui_3width"),
                                                                 uiOutput("ui_3height"),
                                                                 uiOutput("ui_3dpi"))), class = "wellpanel")),
                                    column(8,
                                           br(),
                                           conditionalPanel(
                                             condition = "output.checkpred",
                                             htmlOutput('hintPr')
                                           ),
                                           conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             uiOutput("ui_3model")
                                           ),
                                           inline(conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             uiOutput("ui_3dlplot"))),
                                           inline(conditionalPanel(
                                             condition = "output.clTable",
                                             uiOutput("ui_3dltable"))),
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
                                             uiOutput("ui_3cl")
                                           )),
                                           inline(conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             actionLink("infoCl", icon = icon('info-circle'), label = NULL)
                                           )),
                                           shinyjs::hidden(div(id = "clInfoText",
                                                               uiOutput("ui_3help"))),
                                           conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             htmlOutput('describeCl')
                                           ),
                                           inline(conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             uiOutput("ui_3clbutton")
                                           )),
                                           conditionalPanel(
                                             condition = "output.modelAveragePlot",
                                             dataTableOutput('clTable')
                                           )
                                           ))),
                         tabPanel(title = span(tagList(icon("code"), uiOutput("nav_code"))), 
                                  br(),
                                  uiOutput("ui_4help"),
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
                                    uiOutput("ui_about"))
                                  ),
                         tabPanel(title = uiOutput("nav_guide"),
                                  br(),
                                  includeHTML("user-guide/user-guide.html"))
             ))




