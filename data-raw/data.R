translations <- readxl::read_xlsx("extdata/translations.xlsx", sheet = 1)
translations$id <- paste0("ui_", translations$id)

# ad translation for hc
translations <- translations %>%
  dplyr::filter(!id %in% c("ui_3hc", "ui_3hc2"))

translations <- dplyr::bind_rows(
  translations,
  dplyr::tibble(
    id = "ui_3conc",
    english = "Concentration",
    french = "Concentration"
  ),
  dplyr::tibble(
    id = "ui_thresh_type",
    english = "Get estimate by",
    french = "Obtenir l’estimation par"
  ),
  dplyr::tibble(
    id = "ui_2dlrds",
    english = "Plot .rds",
    french = "Graphique .rds"
  ),
  dplyr::tibble(
    id = "ui_checkHc",
    english = "Plot Threshold/Concentration",
    french = "Graphique seuil/concentration"
  ),
  dplyr::tibble(
    id = "ui_3hc",
    english = "The model averaged estimate of the concentration that affects {percent} % of species is {conc}",
    french = "Selon le modèle agrégé, la concentration affectant {percent} % des espèces est de {conc}"
  ),
  dplyr::tibble(
    id = "ui_3hc2",
    english = "The model averaged estimate of the threshold for a concentration of {conc} is {percent} % of species",
    french = "L'estimation par inférence multimodèle du seuil pour une concentration de {conc} est de {percent} % des espèces."
  ),
  dplyr::tibble(
    id = "ui_1table1",
    english = "hi",
    french = "nope"
  )
)

checkr::check_key(translations, "id")

boron.data <- readr::read_csv("extdata/boron-data.csv")

pal <- RColorBrewer::brewer.pal.info
pals <- pal[which(pal$category == "qual"), ] %>% row.names()

extra.dists <- c("gompertz", "lgumbel", "weibull")
default.dists <- c("llogis", "gamma", "lnorm")

usethis::use_data(boron.data, translations, pals, default.dists, extra.dists, internal = TRUE, overwrite = TRUE)
