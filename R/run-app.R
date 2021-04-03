#' Run Shiny ssdtools Application
#'
#' @export
run_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "shinyssdtools"))
}

#' @describeIn run_app
#'
#' @export
run_ssdtools_app <- function() {
  run_app()
}
