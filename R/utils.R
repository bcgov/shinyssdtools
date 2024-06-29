silent_plot <- function(x) {
  suppressMessages(suppressWarnings(print(x)))
}

dl_button <- function(..., icon = "download", class = "small-dl") {
  downloadButton(..., icon = icon(icon), class = class)
}

units <- function() {
  c(
    "",
    "mg/L",
    "\u00b5g/L",
    "ng/L",
    "%",
    "mg/kg",
    "\u00b5g/kg",
    "mol/L",
    "mmol/L",
    "\u00b5mol/L"
  )
}

append_unit <- function(x, unit) {
  if (unit == "")
    return(x)
  paste0(x, " (", unit, ")")
}
