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
app_server <- function(input, output, session) {
  # --- Translations
  current_lang <- reactive({
    # Determine which language was clicked most recently
    clicks <- c(
      english = input$english %||% 0,
      french = input$french %||% 0,
      spanish = input$spanish %||% 0
    )

    # Return the language with the highest click count
    # Default to english if all are 0
    if (all(clicks == 0)) {
      return("english")
    }
    names(which.max(clicks))
  })

  # Set up shinyhelper with language-specific help files
  observe({
    lang_dir <- switch(
      current_lang(),
      "english" = "en",
      "french" = "fr",
      "spanish" = "es"
    )
    shinyhelper::observe_helpers(
      help_dir = system.file(
        paste("helpfiles", lang_dir, sep = "_"),
        package = "shinyssdtools"
      )
    )
  })

  # shinyhelper::observe_helpers(
  #   help_dir = system.file("helpfiles", package = "shinyssdtools")
  # )

  trans <- reactive({
    translations$trans <- translations[[current_lang()]]
    translations
  }) %>%
    bindEvent(current_lang())

  client_translations <- reactive({
    id <- unique(translations$id)
    trans_data <- trans()

    translations <- sapply(id, function(x) {
      tr(x, trans_data)
    })
    names(translations) <- id
    as.list(translations)
  })

  # Send to client
  observe({
    session$sendCustomMessage(
      "updateTranslations",
      list(translations = client_translations(), language = current_lang())
    )
  }) %>%
    bindEvent(client_translations())

  # --- Number formatting
  big_mark <- reactive({
    switch(
      current_lang(),
      "french" = " ",
      "spanish" = ".",
      ","  # Default for English
    )
  }) %>%
    bindEvent(current_lang())

  decimal_mark <- reactive({
    switch(
      current_lang(),
      "french" = ",",
      "spanish" = ",",
      "."  # Default for English
    )
  }) %>%
    bindEvent(current_lang())

  # Module Server Calls -----------------------------------------------------

  # Shared toxicant name across modules
  shared_toxicant_name <- reactiveVal("")

  # Call module servers with shared values
  data_mod <- mod_data_server("data_mod", trans, current_lang, shared_toxicant_name)
  fit_mod <- mod_fit_server(
    "fit_mod",
    trans,
    current_lang,
    data_mod,
    big_mark,
    decimal_mark,
    main_nav = reactive({
      input$main_nav
    })
  )
  predict_mod <- mod_predict_server(
    "predict_mod",
    trans,
    current_lang,
    data_mod,
    fit_mod,
    big_mark,
    decimal_mark,
    main_nav = reactive({
      input$main_nav
    })
  )
  report_mod <- mod_report_server(
    "report_mod",
    trans,
    current_lang,
    data_mod,
    fit_mod,
    predict_mod,
    shared_toxicant_name
  )
  rcode_mod <- mod_rcode_server(
    "rcode_mod",
    trans,
    data_mod,
    fit_mod,
    predict_mod
  )

  output$ui_5format <- renderUI({
    radioButtons("report_format", "Report format")
  })

  output$ui_about <- renderUI({
    lang <- current_lang()
    ver <- paste("ssdtools version:", utils::packageVersion("ssdtools"))
    sver <- paste(
      "shinyssdtools version:",
      utils::packageVersion("shinyssdtools")
    )

    file_suffix <- switch(
      lang,
      "english" = "en",
      "french" = "fr",
      "spanish" = "es",
      "en"  # Default to English
    )

    file_path <- system.file(
      package = "shinyssdtools",
      paste0("extdata/about-", file_suffix, ".html")
    )

    # Fall back to English if translation doesn't exist
    if (!file.exists(file_path) || file_path == "") {
      file_path <- system.file(package = "shinyssdtools", "extdata/about-en.html")
    }

    tagList(
      p(ver),
      p(sver),
      includeHTML(file_path)
    )
  }) %>%
    bindEvent(current_lang())

  output$ui_userguide <- renderUI({
    lang <- current_lang()
    file_suffix <- switch(
      lang,
      "english" = "en",
      "french" = "fr",
      "spanish" = "es",
      "en"  # Default to English
    )
    file_path <- system.file(
      package = "shinyssdtools",
      paste0("extdata/user-", file_suffix, ".html")
    )

    # Fall back to English if translation doesn't exist
    if (!file.exists(file_path) || file_path == "") {
      file_path <- system.file(package = "shinyssdtools", "extdata/user-en.html")
    }

    includeHTML(file_path)
  }) %>%
    bindEvent(current_lang())
}
