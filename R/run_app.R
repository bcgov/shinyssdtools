#' Run Shiny Application
#'
#' @export
run_ssdtools_app <- function() {
  lifecycle::deprecate_soft("0.0.4", "run_ssdtools_app()", "run_app()")
  run_app()
}

#' Run ssdtools Shiny Application
#'
#' @export
run_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "shinyssdtools"))
}
