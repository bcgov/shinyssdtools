silent_plot <- function(x){
  suppressMessages(suppressWarnings(print(x)))
}

dl_button <- function(..., icon = "download", class = "small-dl") {
  downloadButton(..., icon = icon(icon), class = class)
}