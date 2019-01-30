translations <- read_csv("input/translations.csv")
translations$id <- paste0("ui_", translations$id)


