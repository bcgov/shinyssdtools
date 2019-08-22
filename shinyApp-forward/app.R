
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
