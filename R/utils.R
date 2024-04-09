silent_plot <- function(x) {
  suppressMessages(suppressWarnings(print(x)))
}

dl_button <- function(..., icon = "download", class = "small-dl") {
  downloadButton(..., icon = icon(icon), class = class)
}

units <- function(){
  c("",
    "mg/L",
    "µg/L", 
    "ng/L",
    "%",
    "mg/kg",
    "µg/kg",
    "mol/L",
    "mmol/L",
    "µmol/L")
}

append_unit <- function(x, unit){
  if(unit == "")
    return(x)
  paste0(x, " (", unit, ")")
}
