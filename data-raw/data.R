translations <- readxl::read_xlsx(system.file(package = "shinyssdtools", "extdata/translations.xlsx"), sheet = 1)
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
    english = "Table",
    french = "Table"
  ),
  dplyr::tibble(
    id = "ui_2rescale",
    english = "Rescale data prior to fitting",
    french = "Rescale data prior to fitting"
  ),
  dplyr::tibble(
    id = "ui_2at_boundary_ok",
    english = "Exclude distributions with a parameter value at a boundary",
    french = "Exclude distributions with a parameter value at a boundary"
  ),
  dplyr::tibble(
    id = "ui_2computable",
    english = "Exclude distributions without computable standard errors",
    french = "Exclude distributions without computable standard errors"
  )
)

checkr::check_key(translations, "id")

boron.data <- readr::read_csv(system.file(package = "shinyssdtools", "extdata/boron-data.csv"))

pal <- RColorBrewer::brewer.pal.info
pals <- pal[which(pal$category == "qual"), ] %>% row.names()

default.dists <- ssdtools::ssd_dists_bcanz()
extra.dists <- setdiff(ssdtools::ssd_dists_all(), default.dists)

usethis::use_data(boron.data, translations, pals, default.dists, extra.dists, internal = TRUE, overwrite = TRUE)
