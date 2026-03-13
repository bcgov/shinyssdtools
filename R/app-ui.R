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

app_ui <- function() {
  tagList(
    # Dependencies
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    rclipboard::rclipboardSetup(),

    # Include custom JavaScript for translations
    tags$script(src = "translation.js"),

    # Include custom JavaScript for popover dismiss behavior
    tags$script(src = "popover-dismiss.js"),

    # Add custom CSS handler for notifications
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('addCustomCSS', function(message) {
        var style = document.createElement('style');
        style.type = 'text/css';
        style.innerHTML = message.css;
        document.getElementsByTagName('head')[0].appendChild(style);
      });
    "
    )),

    # Hide shiny errors
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      "/* Remove focus outline from help icons */",
      ".bi-question-circle:focus { outline: none !important; border: none !important; box-shadow: none !important; }",
      ".bi-question-circle { cursor: pointer; }",
      ".initially-hidden {
        display: none;
      }"
    ),

    # Language dropdown menu styling with theme colors
    tags$style(
      type = "text/css",
      glue::glue("
        .dropdown-menu {{
          background-color: {color_secondary} !important;
        }}
        .dropdown-menu .bslib-nav-item a,
        .dropdown-menu .action-button {{
          color: #FFFFFF !important;
          display: block;
          padding: 0.5rem 1rem;
        }}
        .dropdown-menu .bslib-nav-item a:hover,
        .dropdown-menu .action-button:hover {{
          background-color: rgba(255, 255, 255, 0.1) !important;
          color: #FFFFFF !important;
          text-decoration: none;
        }}
      ")
    ),
    tags$script(HTML(
      "
      $(document).on('shiny:connected', function() {
        $('.initially-hidden').removeClass('initially-hidden');
      });
    "
    )),

    page_navbar(
      title = "shinyssdtools",
      theme = bs_theme(
        primary = color_primary,
        secondary = color_secondary,
        success = color_primary,
        info = color_primary
      ),
      navbar_options = navbar_options(bg = color_secondary, underline = TRUE),
      nav_panel(
        title = span(`data-translate` = "ui_navanalyse", "Analyse"),
        page_fillable(
          layout_sidebar(
            padding = 0,
            gap = 0,

            # nav ---------------------------------------------------------------------
            sidebar = sidebar(
              width = 180,
              bg = color_sidebar,
              navset_underline(
                id = "main_nav",
                nav_panel(
                  title = span(
                    bsicons::bs_icon("table"),
                    span(
                      `data-translate` = "ui_nav1",
                      style = "margin-left: 0.5rem;",
                      "1. Data"
                    )
                  ),
                  value = "data"
                ),
                nav_panel(
                  title = span(
                    bsicons::bs_icon("graph-up"),
                    span(
                      `data-translate` = "ui_nav2",
                      style = "margin-left: 0.5rem;",
                      "2. Fit"
                    )
                  ),
                  value = "fit"
                ),
                nav_panel(
                  title = span(
                    bsicons::bs_icon("calculator"),
                    span(
                      `data-translate` = "ui_nav3",
                      style = "margin-left: 0.5rem;",
                      "3. Predict"
                    )
                  ),
                  value = "predict"
                ),
                nav_panel(
                  title = span(
                    bsicons::bs_icon("file-bar-graph"),
                    span(
                      `data-translate` = "ui_nav4",
                      style = "margin-left: 0.5rem;",
                      "4. Report"
                    )
                  ),
                  value = "report"
                ),
                nav_panel(
                  title = span(
                    bsicons::bs_icon("code-slash"),
                    span(
                      `data-translate` = "ui_nav5",
                      style = "margin-left: 0.5rem;",
                      "R Code"
                    )
                  ),
                  value = "rcode"
                )
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'data'",
                mod_data_ui("data_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'fit'",
                mod_fit_ui("fit_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'predict'",
                mod_predict_ui("predict_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'report'",
                mod_report_ui("report_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'rcode'",
                mod_rcode_ui("rcode_mod")
              )
            )
          )
        )
      ),
      nav_panel(
        title = span(`data-translate` = "ui_navabout", "About"),
        card(class = card_shadow, card_body(uiOutput("ui_about")))
      ),
      nav_panel(
        title = span(`data-translate` = "ui_navguide", "User Guide"),
        card(class = card_shadow, uiOutput("ui_userguide"))
      ),
      nav_spacer(),
      nav_menu(
        title = span(`data-translate` = "ui_navlang", "Language"),
        align = "right",
        nav_item(
          actionLink(inputId = "english", label = "English")
        ),
        nav_item(
          actionLink(inputId = "french", label = "Fran\u00e7ais")
        )
        # Spanish disabled
        # nav_item(
        #   actionLink(inputId = "spanish", label = "Espa\u00f1ol")
        # )
      )
    )
  )
}
