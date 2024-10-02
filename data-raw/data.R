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
    english = "The model averaged estimate of the fraction affected by a concentration of {conc} is {percent} % of species",
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
    french = "Redimensionner les données avant l'ajustement"
  ),
  dplyr::tibble(
    id = "ui_xmax",
    english = "X-axis maximum",
    french = "Maximum de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_xmin",
    english = "X-axis minimum",
    french = "Minimum de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_xlog",
    english = "Log x-axis",
    french = "Log de l'axe X"
  ),
  dplyr::tibble(
    id = "ui_sizeLabel",
    english = "Label size",
    french = "Taille de l'étiquette"
  ),
  dplyr::tibble(
    id = "ui_size",
    english = "Text size",
    french = "Taille du texte"
  ),
  dplyr::tibble(
    id = "ui_adjustLabel",
    english = "Shift label",
    french = "Étiquette de changement"
  ),
  dplyr::tibble(
    id = "ui_xbreaks",
    english = "X-axis ticks",
    french = "Repères de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_2unit",
    english = "Select units",
    french = "Sélectionner les unités"
  ),
  dplyr::tibble(
    id = "ui_3byconc",
    english = "by concentration",
    french = "par concentration"
  ),
  dplyr::tibble(
    id = "ui_3affecting",
    english = "affecting % species",
    french = "affectant % des espèces"
  ),
  dplyr::tibble(
    id = "ui_3protecting",
    english = "protecting % species",
    french = "protégeant % des espèces"
  ),
  dplyr::tibble(
    id = "ui_4toxname",
    english = "Toxicant name",
    french = "Nom de la substance"
  ),
  dplyr::tibble(
    id = "ui_4download",
    english = "Download Report",
    french = "Télécharger le Rapport"
  ),
  dplyr::tibble(
    id = "ui_4pdf",
    english = "PDF file",
    french = "Fichier PDF"
  ),
  dplyr::tibble(
    id = "ui_4html",
    english = "HTML file",
    french = "Fichier HTML"
  ),
  dplyr::tibble(
    id = "ui_4rmd",
    english = "RMD file",
    french = "Fichier RMD"
  ),
  dplyr::tibble(
    id = "ui_4gentitle",
    english = "Generating report ...",
    french = "Génération du rapport en cours ... "
  ),
  dplyr::tibble(
    id = "ui_4genbody",
    english = "This may take a minute, depending on the number of bootstrap samples selected in the Predict tab.",
    french = "Cela peut prendre une minute, en fonction du nombre d'échantillons bootstrap sélectionné dans l'onglet « Predict »."
  ),
  dplyr::tibble(
    id = "ui_1htconc",
    english = "Conc",
    french = "Conc"
  ),
  dplyr::tibble(
    id = "ui_1htspp",
    english = "Species",
    french = "Espèce"
  ),
  dplyr::tibble(
    id = "ui_1htgrp",
    english = "Group",
    french = "Groupe"
  ),
  dplyr::tibble(
    id = "ui_1htchm",
    english = "Chemical",
    french = "Produit Chimique"
  ),
  dplyr::tibble(
    id = "ui_1htunt",
    english = "Units",
    french = "Unités"
  ),
  dplyr::tibble(
    id = "ui_bcanz_file",
    english = "bcanz_report.Rmd",
    french = "bcanz_report_fr.Rmd"
  ),
  dplyr::tibble(
    id = "ui_bcanz_filename",
    english = "bcanz_report",
    french = "rapport_bcanz"
  )
)

chk::check_key(translations, "id")

boron.data <- ssddata::ccme_boron

pal <- RColorBrewer::brewer.pal.info
pals <- pal[which(pal$category == "qual"), ] %>% row.names()

default.dists <- ssdtools::ssd_dists_bcanz()
extra.dists <- setdiff(ssdtools::ssd_dists_all(), default.dists)

usethis::use_data(boron.data, translations, pals, default.dists, extra.dists, internal = TRUE, overwrite = TRUE)
