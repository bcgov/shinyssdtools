#' Run Shiny Application
#'
#' @export
run_ssdtools_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "shinyssdtools"), 
                     options = c("launch.browser" = TRUE))
}
