# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

#' Create a waiter loading screen
#' @param id Character string output ID to attach waiter to
#' @param ns Shiny namespace function
#' @return A Waiter object configured for the namespaced output
#' @keywords internal
ui_waiter <- function(id, ns) {
  waiter::Waiter$new(id = ns(id), html = waiter::spin_2(), color = "white")
}

#' Create static label with dynamic input pattern
#' @param ns_id Character string namespaced input ID for label's 'for' attribute
#' @param translate_key Character string translation key for data-translate attribute
#' @param default_text Character string default text to display in label
#' @param ui_output_id Character string ID for the uiOutput element
#' @return tagList with label and uiOutput elements
#' @keywords internal
static_label_input <- function(
  ns_id,
  translate_key,
  default_text,
  ui_output_id
) {
  tagList(
    tags$label(
      `for` = ns_id,
      class = "control-label",
      span(`data-translate` = translate_key, default_text)
    ),
    uiOutput(ui_output_id)
  )
}

#' Generate DataTables options with language support
#' @param lang Character string language code: "english", "french", or "spanish" (default: "english")
#' @return List of DataTables options including pageLength, language translations, and column definitions
#' @keywords internal
dt_options <- function(lang = "english") {
  # Language-specific translations
  lang_options <- switch(
    lang,
    "french" = list(
      search = "Rechercher :",
      lengthMenu = "Afficher _MENU_ entr\u00e9es",
      info = "Affichage de _START_ \u00e0 _END_ sur _TOTAL_ entr\u00e9es",
      infoEmpty = "Aucune donn\u00e9e disponible",
      infoFiltered = "(filtr\u00e9 \u00e0 partir de _MAX_ entr\u00e9es au total)",
      zeroRecords = "Aucun enregistrement correspondant trouv\u00e9",
      paginate = list(
        first = "Premier",
        last = "Dernier",
        `next` = "Suivant",
        previous = "Pr\u00e9c\u00e9dent"
      ),
      processing = "Traitement en cours...",
      loadingRecords = "Chargement des enregistrements...",
      emptyTable = "Aucune donn\u00e9e disponible dans le tableau"
    ),
    "spanish" = list(
      search = "Buscar datos:",
      lengthMenu = "Mostrar _MENU_ entradas",
      info = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
      infoEmpty = "No hay datos disponibles",
      infoFiltered = "(filtrado de _MAX_ entradas totales)",
      zeroRecords = "No se encontraron registros coincidentes",
      paginate = list(
        first = "Primero",
        last = "\u00daltimo",
        `next` = "Siguiente",
        previous = "Anterior"
      ),
      processing = "Procesando...",
      loadingRecords = "Cargando registros...",
      emptyTable = "No hay datos disponibles en la tabla"
    ),
    # Default English
    list(
      search = "Search data:",
      lengthMenu = "Show _MENU_ entries",
      info = "Showing _START_ to _END_ of _TOTAL_ entries",
      infoEmpty = "No data available",
      infoFiltered = "(filtered from _MAX_ total entries)",
      zeroRecords = "No matching records found",
      paginate = list(
        first = "First",
        last = "Last",
        `next` = "Next",
        previous = "Previous"
      ),
      processing = "Processing...",
      loadingRecords = "Loading records...",
      emptyTable = "No data available in table"
    )
  )

  list(
    # Pagination and display
    pageLength = 15,
    lengthMenu = c(10, 15, 25, 50, 100),
    # Search and filtering
    searchHighlight = TRUE,
    search = list(regex = TRUE, caseInsensitive = TRUE),

    # Column features
    columnDefs = list(
      list(className = 'dt-center', targets = '_all'),
      list(searchable = TRUE, targets = '_all')
    ),

    # Language support
    language = lang_options
  )
}

#' Create a dashed border info box
#' @param x UI content to display inside the box
#' @return div element with dashed border styling and centered muted text
#' @keywords internal
ui_dashbox <- function(x) {
  div(
    class = "text-muted text-center p-5",
    style = "border: 2px dashed #dee2e6; border-radius: 8px; margin: 2rem;",
    x
  )
}

#' Create table download popover button
#' @param tab Character string module prefix for button IDs (default: "fit")
#' @param ns Shiny namespace function
#' @return bslib popover element with download button and format options
#' @keywords internal
ui_download_popover_table <- function(tab = "fit", ns) {
  bslib::popover(
    actionButton(
      ns(paste0(tab, "DownloadBtnTbl")),
      label = tagList(
        bsicons::bs_icon("download"),
        span(`data-translate` = "ui_2download", "Download")
      ),
      style = "padding:4px; font-size:80%; width: 100px"
    ),
    card(
      class = card_shadow,
      style = "width: 250px; margin-top: 10px;",
      card_body(
        div(
          style = "display: grid; gap: 8px;",
          downloadButton(
            ns(paste0(tab, "DlXlsx")),
            label = span(`data-translate` = "ui_2dlxlsx", "XLSX file"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          ),
          downloadButton(
            ns(paste0(tab, "DlCsv")),
            label = span(`data-translate` = "ui_2dlcsv", "CSV File"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          )
        )
      )
    ),
    placement = "bottom"
  )
}

#' Create report download popover button
#' @param tab Character string module prefix for button IDs (default: "report")
#' @param ns Shiny namespace function
#' @return bslib popover element with download button and format options
#' @keywords internal
ui_download_report <- function(tab = "report", ns) {
  bslib::popover(
    actionButton(
      ns(paste0(tab, "DownloadBtnReport")),
      label = tagList(
        bsicons::bs_icon("download"),
        span(`data-translate` = "ui_2download", "Download")
      ),
      style = "padding:4px; font-size:80%; width: 100px"
    ),
    card(
      class = card_shadow,
      style = "width: 250px; margin-top: 10px;",
      card_body(
        div(
          style = "display: grid; gap: 8px;",
          downloadButton(
            ns(paste0(tab, "DlPdf")),
            label = span(`data-translate` = "ui_2dlpdf", "PDF file"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          ),
          downloadButton(
            ns(paste0(tab, "DlHtml")),
            label = span(`data-translate` = "ui_2dlhtml", "HTML File"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          )
        )
      )
    ),
    placement = "bottom"
  )
}

#' Create plot download popover button with settings
#' @param tab Character string module prefix for button IDs (default: "fit")
#' @param ns Shiny namespace function
#' @return bslib popover element with download button, format options, and PNG settings
#' @keywords internal
ui_download_popover <- function(tab = "fit", ns) {
  bslib::popover(
    actionButton(
      ns(paste0(tab, "DownloadBtn")),
      label = tagList(
        bsicons::bs_icon("download"),
        span(`data-translate` = "ui_2download", "Download")
      ),
      style = "padding:4px; font-size:80%; width: 100px;"
    ),
    card(
      class = card_shadow,
      style = "width: 250px; margin-top: 10px;",
      card_body(
        div(
          style = "display: grid; gap: 8px;",
          downloadButton(
            ns(paste0(tab, "DlPlot")),
            label = span(`data-translate` = "ui_2dlplot", "PNG file"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          ),
          downloadButton(
            ns(paste0(tab, "DlRds")),
            label = span(`data-translate` = "ui_2dlrds", "RDS File"),
            style = "width: 100%; padding: 6px; font-size: 12px;",
            class = "btn-primary btn-sm"
          )
        ),
        div(
          h6(
            span(`data-translate` = "ui_2png", "PNG Format Settings"),
            style = "margin-bottom: 10px;"
          ),
          div(
            style = "display: flex; gap: 5px; justify-content: space-between;",
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(
                ns("width"),
                label = span(`data-translate` = "ui_2width", "Width"),
                value = 6,
                min = 1,
                max = 50,
                step = 1
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(
                ns("height"),
                label = span(`data-translate` = "ui_2height", "Height"),
                value = 4,
                min = 1,
                max = 50,
                step = 1
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(
                ns("dpi"),
                label = span(`data-translate` = "ui_2dpi", "DPI"),
                value = 300,
                min = 50,
                max = 2000,
                step = 50
              )
            )
          )
        )
      )
    ),
    placement = "bottom"
  )
}

#' Build GOF-style header tooltips named vector
#' @param trans Translations reactive value
#' @param lang Character string language code ("english", "french", "spanish")
#' @return Named character vector mapping column names to tooltip descriptions
#' @keywords internal
gof_header_tooltips <- function(trans, lang = "english") {
  # Descriptions sourced from inst/extdata/about-{lang}.md
  tooltips <- switch(lang,
    "french" = c(
      dist = "Distribution",
      npars = "Nombre de param\u00e8tres",
      nobs = "Nombre d'observations",
      log_lik = "Log-vraisemblance",
      aic = "Crit\u00e8re d'information Akaike",
      aicc = "Crit\u00e8re d'information Akaike corrig\u00e9 pour la taille de l'\u00e9chantillon",
      delta = "Diff\u00e9rence entre AICc",
      wt = "Pond\u00e9ration des crit\u00e8res d'information AICc",
      weight = "Pond\u00e9ration des crit\u00e8res d'information AICc",
      bic = "Crit\u00e8re d'information Bay\u00e9sien",
      ad = "Statistique d'Anderson-Darling",
      ks = "Statistique de Kolmogorov-Smirnov",
      cvm = "Statistique de Cramer-von Mises",
      est = "Concentration ou pourcentage estim\u00e9",
      se = "Erreur type",
      lcl = "Limite de confiance inf\u00e9rieure",
      ucl = "Limite de confiance sup\u00e9rieure",
      nboot = "Nombre d'\u00e9chantillons bootstrap",
      pboot = "Proportion de bootstraps r\u00e9ussis",
      samples = "Nombre d'\u00e9chantillons",
      proportion = "Proportion d'esp\u00e8ces affect\u00e9es",
      percent = "Pourcentage d'esp\u00e8ces affect\u00e9es"
    ),
    # Default: English
    c(
      dist = "Distribution",
      npars = "Number of parameters",
      nobs = "Number of observations",
      log_lik = "Log-likelihood",
      aic = "Akaike's Information Criterion",
      aicc = "Akaike's Information Criterion corrected for sample size",
      delta = "AICc difference",
      wt = "AICc based Akaike weight",
      weight = "AICc based Akaike weight",
      bic = "Bayesian Information Criterion",
      ad = "Anderson-Darling statistic",
      ks = "Kolmogorov-Smirnov statistic",
      cvm = "Cramer-von Mises statistic",
      est = "Estimated concentration or percent",
      se = "Standard error",
      lcl = "Lower confidence limit",
      ucl = "Upper confidence limit",
      nboot = "Number of bootstrap samples",
      pboot = "Proportion of successful bootstraps",
      samples = "Number of samples",
      proportion = "Proportion of species affected",
      percent = "Percent of species affected"
    )
  )
  wt_translated <- tr("ui_2weight", trans)
  if (!wt_translated %in% names(tooltips)) {
    tooltips[[wt_translated]] <- tooltips[["wt"]]
  }
  tooltips
}

#' Create DT headerCallback JS for column tooltips
#' @param tooltips Named character vector mapping column names to tooltip text
#' @return DT::JS object with headerCallback function
#' @keywords internal
dt_header_tooltip_callback <- function(tooltips) {
  json <- as.character(jsonlite::toJSON(as.list(tooltips), auto_unbox = TRUE))
  DT::JS(sprintf(
    "function(thead, data, start, end, display) {
      var tooltips = %s;
      $(thead).find('th').each(function() {
        var text = $(this).text().trim();
        if (tooltips[text]) {
          $(this).attr('title', tooltips[text]);
          $(this).css('cursor', 'help');
        }
      });
    }",
    json
  ))
}

#' Add weight column color bar to a DT datatable
#' @param dt A DT datatable object
#' @param data The data frame used to create the datatable
#' @param trans Translations reactive value
#' @return The DT datatable with color bar formatting on the weight column
#' @keywords internal
dt_weight_color_bar <- function(dt, data, trans) {
  wt_col <- grep(
    paste0("^wt$|^weight$|^", tr("ui_2weight", trans), "$"),
    names(data),
    value = TRUE
  )[1]
  if (!is.na(wt_col) && length(data[[wt_col]]) > 0 && any(!is.na(data[[wt_col]]))) {
    wt_range <- range(data[[wt_col]], na.rm = TRUE)
    dt <- DT::formatStyle(
      dt,
      wt_col,
      background = DT::styleColorBar(
        c(0, max(wt_range[2], 0.01)),
        color = "#d4edda"
      ),
      backgroundSize = "98% 80%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
  }
  dt
}
