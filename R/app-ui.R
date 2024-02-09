# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

app_ui <- function() {
  tagList(
    tags$head(tags$title("shinyssdtools")),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    add_external_resources(),
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    div(class = "div-link", HTML(paste0(
      actionButton("en", "English",
        class = "msw-button"
      ), "/",
      actionButton("fr", "French", class = "msw-button")
    ))),
    navbarPage(
      title = uiOutput("ui_navtitle"), windowTitle = "shinyssdtools",

      # data tab ----------------------------------------------------------------
      tabPanel(
        title = span(tagList(icon("table"), inline(uiOutput("ui_nav1")))),
        fluidRow(
          br(),
          column(
            4,
            uiOutput("ui_1choose"),
            inline(uiOutput("ui_1data")),
            inline(actionLink("infoDemo", icon = icon("info-circle"), label = NULL)),
            shinyjs::hidden(div(
              id = "infoDemoText",
              uiOutput("ui_1datahelp")
            )),
            # upload csv with data
            br(),
            inline(uiOutput("ui_1csv")),
            inline(actionLink("infoUpload", icon = icon("info-circle"), label = NULL)),
            shinyjs::hidden(div(
              id = "infoUploadText",
              uiOutput("ui_1csvhelp")
            )),
            uiOutput("ui_1csvupload"),

            # input data in DataTable
            inline(uiOutput("ui_1table1")),
            inline(actionLink("infoHands", icon = icon("info-circle"), label = NULL)),
            shinyjs::hidden(div(
              id = "infoHandsText",
              uiOutput("ui_1tablehelp")
            )),
            rhandsontable::rHandsontableOutput("hot")
          ),
          column(
            8,
            uiOutput("ui_1preview"),
            uiOutput("ui_viewupload")
          )
        ),
        div(
          id = "note",
          uiOutput("ui_1note1")
        )
      ),
      # fit tab -----------------------------------------------------------------
      tabPanel(
        title = span(tagList(icon("stats", lib = "glyphicon"), inline(uiOutput("ui_nav2")))),
        fluidRow(
          column(
            4,
            br(),
            wellPanel(
              uiOutput("selectConc"),
              uiOutput("ui_2select"),
              uiOutput("ui_2rescale"),
              uiOutput("ui_2at_boundary_ok"),
              uiOutput("ui_2computable"),
              uiOutput("ui_2xlab"),
              uiOutput("ui_2ylab"),
              uiOutput("ui_2size"),
              uiOutput("ui_2png"),
              div(
                id = "divFormatFit",
                br(),
                inline(uiOutput("ui_2width")),
                inline(uiOutput("ui_2height")),
                inline(uiOutput("ui_2dpi"))
              )
            )
          ),
          column(
            8,
            br(),
            conditionalPanel(
              condition = "output.checkfit",
              htmlOutput("hintFi")
            ),
            conditionalPanel(
              condition = "output.distPlot1",
              uiOutput("ui_2plot")
            ),
            inline(conditionalPanel(
              condition = "output.distPlot1",
              uiOutput("ui_2dlplot")
            )),
            inline(conditionalPanel(
              condition = "output.distPlot1",
              uiOutput("ui_2dlrds")
            )),
            inline(conditionalPanel(
              condition = "output.distPlot1",
              uiOutput("ui_2dltable")
            )),
            br(), br(),
            htmlOutput("fitFail"),
            uiOutput("ui_distplot"),
            br(),
            conditionalPanel(
              condition = "output.gofTable",
              uiOutput("ui_2table")
            ),
            DT::dataTableOutput("gofTable")
          )
        )
      ),
      # predict tab -------------------------------------------------------------
      tabPanel(
        title = span(tagList(icon("calculator"), inline(uiOutput("ui_nav3")))),
        fluidRow(
          column(
            4,
            br(),
            wellPanel(
              uiOutput("ui_3est"),
              uiOutput("ui_3bshint"),
              br(),
              uiOutput("ui_thresh_type"),
              uiOutput("ui_3thresh"),
              uiOutput("ui_3samples"),
              br(),
              uiOutput("selectLabel"),
              uiOutput("selectColour"),
              uiOutput("selectShape"),
              uiOutput("ui_3plotopts"),
              div(
                id = "divFormatPredict",
                br(),
                uiOutput("ui_3pal"),
                uiOutput("ui_3xlab"),
                uiOutput("ui_3ylab"),
                uiOutput("ui_3title"),
                uiOutput("uiLegendColour"),
                uiOutput("uiLegendShape"),
                splitLayout(
                  uiOutput("ui_3size"),
                  uiOutput("ui_3sizeLabel")
                ),
                uiOutput("ui_checkHc"),
                splitLayout(
                  uiOutput("uiAdjustLabel"),
                  uiOutput("uiXmin"),
                  uiOutput("uiXmax")
                ),
                uiOutput("uiXlog"),
                uiOutput("uiXbreaks"),
              ),
              uiOutput("ui_3pngopts"),
              div(
                id = "divPngFormatPredict",
                br(),
                inline(uiOutput("ui_3width")),
                inline(uiOutput("ui_3height")),
                inline(uiOutput("ui_3dpi"))
              ),
              class = "wellpanel"
            )
          ),
          column(
            8,
            br(),
            conditionalPanel(
              condition = "output.checkpred",
              htmlOutput("hintPr")
            ),
            conditionalPanel(
              condition = "output.modelAveragePlot",
              uiOutput("ui_3model")
            ),
            inline(conditionalPanel(
              condition = "output.modelAveragePlot",
              uiOutput("ui_3dlplot")
            )),
            inline(conditionalPanel(
              condition = "output.modelAveragePlot",
              uiOutput("ui_3dlrds")
            )),
            inline(conditionalPanel(
              condition = "output.clTable",
              uiOutput("ui_3dltable")
            )),
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
              uiOutput("ui_3cl")
            )),
            inline(conditionalPanel(
              condition = "output.modelAveragePlot",
              actionLink("infoCl", icon = icon("info-circle"), label = NULL)
            )),
            shinyjs::hidden(div(
              id = "clInfoText",
              uiOutput("ui_3help")
            )),
            conditionalPanel(
              condition = "output.modelAveragePlot",
              htmlOutput("describeCl")
            ),
            inline(conditionalPanel(
              condition = "output.modelAveragePlot",
              uiOutput("ui_3clbutton")
            )),
            conditionalPanel(
              condition = "output.modelAveragePlot",
              DT::dataTableOutput("clTable")
            )
          )
        )
      ),
      tabPanel(
        title = "4. BCANZ Report",
        br(),
        uiOutput("ui_report_download")
      ),
      # R code tab ------------------------------------------------------------------
      tabPanel(
        title = span(tagList(icon("code"), inline(uiOutput("ui_nav4")))),
        br(),
        uiOutput("ui_4help"),
        div(
          id = "codes",
          wellPanel(
            uiOutput("codeHead"),
            br(),
            uiOutput("codeData"),
            br(),
            uiOutput("codeFit"),
            br(),
            uiOutput("codeSaveFit"),
            br(),
            uiOutput("codePredPlot"),
            br(),
            uiOutput("codeSavePred"),
            br(),
            uiOutput("codePredCl")
          )
        )
      ),
      tabPanel(
        title = inline(uiOutput("ui_navabout")),
        br(),
        wellPanel(
          uiOutput("ui_about")
        )
      ),
      tabPanel(
        title = inline(uiOutput("ui_navguide")),
        br(),
        wellPanel(uiOutput("ui_userguide"))
      )
    )
  )
}

add_external_resources <- function() {
  addResourcePath("www", system.file("app/www", package = "shinyssdtools"))
  tagList(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"))
}
