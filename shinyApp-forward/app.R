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

library(shiny)

ui <- fluidPage(

    fluidRow(align = "center",
             br(),
    h3("ssdtools doesn't live here anymore!"),
    h5("Please go to"),
    tags$a(href = "https://poissonconsulting.shinyapps.io/ssdtools/", 
           "https://poissonconsulting.shinyapps.io/ssdtools/"),
    h5("and update your bookmarks."))
)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
