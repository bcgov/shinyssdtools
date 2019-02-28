# functions
label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

inline = function (x) {
  tags$div(style = "display:inline-block;", x)
}

hint <- function(x) HTML(paste0("<font color='grey'>", x, "</font>"))

zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}







