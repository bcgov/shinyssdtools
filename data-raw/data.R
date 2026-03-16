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

library(dplyr)

# All translations unified from CSV (previously split between Excel and manual tibbles)
translations <- dplyr::bind_rows(
  dplyr::tibble(
    id = "ui_navtitle",
    english = "Fit and Plot Species Sensitivity Distributions",
    french = "Ajustement et tracer des distributions de sensibilit\u00e9 des esp\u00e8ces",
    spanish = "Ajuste y Gr\u00e1fico de Distribuciones de Sensibilidad de Especies"
  ),
  dplyr::tibble(
    id = "ui_nav1",
    english = "1. Data",
    french = "1. Donn\u00e9es",
    spanish = "1. Datos"
  ),
  dplyr::tibble(
    id = "ui_nav2",
    english = "2. Fit",
    french = "2. Ajustement",
    spanish = "2. Ajuste"
  ),
  dplyr::tibble(
    id = "ui_nav3",
    english = "3. Predict",
    french = "3. Estimation",
    spanish = "3. Predicci\u00f3n"
  ),
  dplyr::tibble(
    id = "ui_nav4",
    english = "4. Report",
    french = "4. Rapport",
    spanish = "4. Informe"
  ),
  dplyr::tibble(
    id = "ui_nav5",
    english = "R Code",
    french = "Code R",
    spanish = "C\u00f3digo R"
  ),
  dplyr::tibble(
    id = "ui_navanalyse",
    english = "Analyse",
    french = "Analyse",
    spanish = "Analizar"
  ),
  dplyr::tibble(
    id = "ui_navabout",
    english = "About",
    french = "\u00c0 propos",
    spanish = "Acerca de"
  ),
  dplyr::tibble(
    id = "ui_navguide",
    english = "User Guide",
    french = "Guide de l'utilisateur",
    spanish = "Gu\u00eda del Usuario"
  ),
  dplyr::tibble(
    id = "ui_navlang",
    english = "Language",
    french = "Langue",
    spanish = "Idioma"
  ),
  dplyr::tibble(
    id = "ui_tabdata",
    english = "Provide data",
    french = "Fournir les donn\u00e9es",
    spanish = "Proporcionar datos"
  ),
  dplyr::tibble(
    id = "ui_tabfit",
    english = "Fit distributions",
    french = "Ajustement des distributions",
    spanish = "Ajustar distribuciones"
  ),
  dplyr::tibble(
    id = "ui_tabpredict",
    english = "Estimate hazard concentration",
    french = "Estimation de la concentration pr\u00e9sentant un risque",
    spanish = "Estimar concentraci\u00f3n peligrosa"
  ),
  dplyr::tibble(
    id = "ui_tabreport",
    english = "Get BCANZ report",
    french = "Obtenir le rapport BCANZ",
    spanish = "Obtener informe BCANZ"
  ),
  dplyr::tibble(
    id = "ui_tabcode",
    english = "Get R code",
    french = "Obtenir le code R",
    spanish = "Obtener c\u00f3digo R"
  ),
  dplyr::tibble(
    id = "ui_1choose",
    english = "Choose one of the following options:",
    french = "Choisir l\u2019une des options suivantes :",
    spanish = "Elija una de las siguientes opciones:"
  ),
  dplyr::tibble(
    id = "ui_1data",
    english = "1. Use",
    french = "1. Utiliser",
    spanish = "1. Usar"
  ),
  dplyr::tibble(
    id = "ui_1data2",
    english = "boron dataset",
    french = "l\u2019ensemble de donn\u00e9es pour le bore",
    spanish = "conjunto de datos de boro"
  ),
  dplyr::tibble(
    id = "ui_1title",
    english = "1. Data",
    french = "1. Donn\u00e9es",
    spanish = "1. Datos"
  ),
  dplyr::tibble(
    id = "ui_1datahelp",
    english = "1. This can be used to demo the app or view a dataset that 'works'.",
    french = "1. Ces donn\u00e9es peuvent \u00eatre t\u00e9l\u00e9charg\u00e9es pour visualiser un ensemble 'fonctionnel' de donn\u00e9es.",
    spanish = "1. Esto se puede usar para demostrar la aplicaci\u00f3n o ver un conjunto de datos que 'funciona'."
  ),
  dplyr::tibble(
    id = "ui_1csv",
    english = "2. Upload CSV file",
    french = "2. T\u00e9l\u00e9verser un fichier CSV",
    spanish = "2. Cargar archivo CSV"
  ),
  dplyr::tibble(
    id = "ui_1csvlabel",
    english = "Upload your data",
    french = "T\u00e9l\u00e9charger les donn\u00e9es",
    spanish = "Cargar sus datos"
  ),
  dplyr::tibble(
    id = "ui_1table",
    english = "3. Fill out table below:",
    french = "3. Remplir le tableau ci-dessous:",
    spanish = "3. Completar la tabla a continuaci\u00f3n:"
  ),

  dplyr::tibble(
    id = "ui_1note",
    english = "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value.",
    french = "Note : L\u2019application est faite pour travailler un contaminant \u00e0 la fois. Il n\u2019est pas possible d\u2019entrer plus d\u2019une concentration par esp\u00e8ce.",
    spanish = "Nota: la aplicaci\u00f3n est\u00e1 dise\u00f1ada para manejar un qu\u00edmico a la vez. Cada especie no debe tener m\u00e1s de un valor de concentraci\u00f3n."
  ),
  dplyr::tibble(
    id = "ui_1preview",
    english = "Preview chosen dataset",
    french = "Aper\u00e7u de l'ensemble de donn\u00e9es s\u00e9lectionn\u00e9",
    spanish = "Vista previa del conjunto de datos elegido"
  ),
  dplyr::tibble(
    id = "ui_2title",
    english = "2. Fit",
    french = "2. Ajustement",
    spanish = "2. Ajuste"
  ),
  dplyr::tibble(
    id = "ui_2conc",
    english = "Select column with concentration values",
    french = "S\u00e9lectionner la colonne avec les valeurs de concentration",
    spanish = "Seleccionar columna con valores de concentraci\u00f3n"
  ),
  dplyr::tibble(
    id = "ui_2dist",
    english = "Select distributions to fit",
    french = "S\u00e9lectionner les distributions pour l\u2019ajustement",
    spanish = "Seleccionar distribuciones para ajustar"
  ),
  dplyr::tibble(
    id = "ui_2png",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG",
    spanish = "Opciones de formato de archivo PNG"
  ),
  dplyr::tibble(
    id = "ui_2plot",
    english = "Plot fitted distributions",
    french = "Repr\u00e9sentation des courbes de distribution ajust\u00e9es",
    spanish = "Gr\u00e1fico de distribuciones ajustadas"
  ),
  dplyr::tibble(
    id = "ui_2table",
    english = "Goodness of Fit table",
    french = "Tableau de l'\u00e9valuation de la qualit\u00e9 de l\u2019ajustement des courbes de distribution",
    spanish = "Tabla de Bondad de Ajuste"
  ),
  dplyr::tibble(
    id = "ui_2weight",
    english = "weight",
    french = "coefficient de pond\u00e9ration",
    spanish = "peso"
  ),
  dplyr::tibble(
    id = "ui_2ploty",
    english = "Species affected (%)",
    french = "Pourcentage d\u2019esp\u00e8ces affect\u00e9es",
    spanish = "Especies afectadas (%)"
  ),
  dplyr::tibble(
    id = "ui_2download",
    english = "Download",
    french = "T\u00e9l\u00e9charger",
    spanish = "Descargar"
  ),
  dplyr::tibble(
    id = "ui_2height",
    english = "Height",
    french = "Hauteur",
    spanish = "Altura"
  ),
  dplyr::tibble(
    id = "ui_2width",
    english = "Width",
    french = "Largeur",
    spanish = "Ancho"
  ),
  dplyr::tibble(
    id = "ui_2dpi",
    english = "Dpi",
    french = "Dpi",
    spanish = "Dpi"
  ),
  dplyr::tibble(
    id = "ui_2dlpdf",
    english = "PDF File",
    french = "Fichier PDF",
    spanish = "Archivo PDF"
  ),
  dplyr::tibble(
    id = "ui_2dlhtml",
    english = "HTML File",
    french = "Fichier HTML",
    spanish = "Archivo HTML"
  ),
  dplyr::tibble(
    id = "ui_2dlrds",
    english = "RDS File",
    french = "Fichier RDS",
    spanish = "Archivo RDS"
  ),
  dplyr::tibble(
    id = "ui_2dlplot",
    english = "PNG File",
    french = "Fichier PNG",
    spanish = "Archivo PNG"
  ),
  dplyr::tibble(
    id = "ui_2dlcsv",
    english = "CSV File",
    french = "Fichier CSV",
    spanish = "Archivo CSV"
  ),
  dplyr::tibble(
    id = "ui_2dlxlsx",
    english = "XLSX File",
    french = "Fichier XLSX",
    spanish = "Archivo XLSX"
  ),
  dplyr::tibble(
    id = "ui_3tabtitle",
    english = "3. Predict",
    french = "3. Estimation",
    spanish = "3. Predicci\u00f3n"
  ),
  dplyr::tibble(
    id = "ui_3est",
    english = "Estimate hazard concentration",
    french = "Estimation de la concentration pr\u00e9sentant un risque",
    spanish = "Estimar concentraci\u00f3n peligrosa"
  ),
  dplyr::tibble(
    id = "ui_3bshint",
    english = "10,000 bootstrap samples recommended",
    french = "10 000 simulations bootstrap recommand\u00e9es",
    spanish = "Se recomiendan 10.000 muestras bootstrap"
  ),
  dplyr::tibble(
    id = "ui_3thresh",
    english = "Fraction affected",
    french = "Fraction affect\u00e9e",
    spanish = "Fracci\u00f3n afectada"
  ),
  dplyr::tibble(
    id = "ui_3threshlabel",
    english = "Required estimate",
    french = "Estimation n\u00e9cessaire",
    spanish = "Estimaci\u00f3n requerida"
  ),
  dplyr::tibble(
    id = "ui_3samples",
    english = "Bootstrap samples",
    french = "Simulations bootstrap",
    spanish = "Muestras bootstrap"
  ),
  dplyr::tibble(
    id = "ui_3label",
    english = "Label by",
    french = "Organiser les \u00e9tiquettes par",
    spanish = "Etiquetar por"
  ),
  dplyr::tibble(
    id = "ui_3colour",
    english = "Colour by",
    french = "Organiser les couleurs par",
    spanish = "Colorear por"
  ),
  dplyr::tibble(
    id = "ui_3symbol",
    english = "Symbol by",
    french = "Organiser les symboles par",
    spanish = "S\u00edmbolo por"
  ),
  dplyr::tibble(
    id = "ui_3plotopts",
    english = "Plot formatting options",
    french = "Options de formatage du graphique",
    spanish = "Opciones de formato de gr\u00e1fico"
  ),
  dplyr::tibble(
    id = "ui_3pngopts",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG",
    spanish = "Opciones de formato de archivo PNG"
  ),
  dplyr::tibble(
    id = "ui_3model",
    english = "Plot model average and estimate hazard concentration",
    french = "Repr\u00e9sentation de l'inf\u00e9rence multimod\u00e8le et estimation de la concentration pr\u00e9sentant un risque",
    spanish = "Gr\u00e1fico de promedio de modelo y estimaci\u00f3n de concentraci\u00f3n peligrosa"
  ),
  dplyr::tibble(
    id = "ui_3help",
    english = "Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimate.",
    french = "Cliquez sur 'Obtenir bornes' pour obtenir les bornes inf\u00e9rieures et sup\u00e9rieures de l'intervalle de confiance pour l\u2019estimation.",
    spanish = "Haga clic en 'Obtener LC' para calcular los l\u00edmites de confianza superior e inferior (LC) para la estimaci\u00f3n."
  ),
  dplyr::tibble(
    id = "ui_3hc",
    english = "The model averaged estimate of the concentration that affects",
    french = "L'estimation par inf\u00e9rence multimod\u00e8le de la concentration affectant",
    spanish = "La estimaci\u00f3n promediada del modelo de la concentraci\u00f3n que afecta"
  ),
  dplyr::tibble(
    id = "ui_3hc2",
    english = "% of species is",
    french = "% des esp\u00e8ces est de",
    spanish = "% de las especies es"
  ),
  dplyr::tibble(
    id = "ui_3perc",
    english = "The model averaged estimate of the fraction affected by a concentration of",
    french = "L'estimation par inf\u00e9rence multimod\u00e8le du seuil pour une concentration de",
    spanish = "La estimaci\u00f3n promediada del modelo de la fracci\u00f3n afectada por una concentraci\u00f3n de"
  ),
  dplyr::tibble(
    id = "ui_3perc2",
    english = "is",
    french = "est de",
    spanish = "es"
  ),
  dplyr::tibble(
    id = "ui_3perc3",
    english = "% of species",
    french = "% des esp\u00e8ces",
    spanish = "% de las especies"
  ),
  dplyr::tibble(
    id = "ui_3cl",
    english = "Get confidence limits",
    french = "Obtenir les bornes de l'intervalle de confiance",
    spanish = "Obtener l\u00edmites de confianza"
  ),
  dplyr::tibble(
    id = "ui_3cl2",
    english = "Confidence limits",
    french = "Bornes de l'intervalle de confiance",
    spanish = "L\u00edmites de confianza"
  ),
  dplyr::tibble(
    id = "ui_3includeci",
    english = "Include on model average plot",
    french = "Inclure sur le graphique de l'inf\u00e9rence multimod\u00e8le",
    spanish = "Incluir en gr\u00e1fico de promedio del modelo"
  ),
  dplyr::tibble(
    id = "ui_3ribbonstyle",
    english = "Model averaged SSD and CL style",
    french = "Style de la DSE moyenn\u00e9e et bornes",
    spanish = "Estilo de estimaci\u00f3n promediada del modelo y LC"
  ),
  dplyr::tibble(
    id = "ui_3ribbonblack",
    english = "Ribbon",
    french = "Ruban",
    spanish = "Cinta"
  ),
  dplyr::tibble(
    id = "ui_3ribbonlines",
    english = "Lines",
    french = "Lignes",
    spanish = "L\u00edneas"
  ),
  dplyr::tibble(
    id = "ui_3pal",
    english = "Colour palette",
    french = "\u00c9ventail de couleur",
    spanish = "Paleta de colores"
  ),
  dplyr::tibble(
    id = "ui_3xlab",
    english = "X-axis label",
    french = "Titre de l\u2019axe des X",
    spanish = "Etiqueta del eje X"
  ),
  dplyr::tibble(
    id = "ui_3ylab",
    english = "Y-axis label",
    french = "Titre de l\u2019axe des Y",
    spanish = "Etiqueta del eje Y"
  ),
  dplyr::tibble(
    id = "ui_3title",
    english = "Plot title",
    french = "Titre du graphique",
    spanish = "T\u00edtulo del gr\u00e1fico"
  ),
  dplyr::tibble(
    id = "ui_3legend",
    english = "Colour legend title",
    french = "Titre de la l\u00e9gende des couleurs",
    spanish = "T\u00edtulo de leyenda de color"
  ),
  dplyr::tibble(
    id = "ui_3shape",
    english = "Shape legend title",
    french = "Titre de la l\u00e9gende des symboles",
    spanish = "T\u00edtulo de leyenda de forma"
  ),
  dplyr::tibble(
    id = "ui_3width",
    english = "Width",
    french = "Largeur",
    spanish = "Ancho"
  ),
  dplyr::tibble(
    id = "ui_3height",
    english = "Height",
    french = "Hauteur",
    spanish = "Altura"
  ),
  dplyr::tibble(
    id = "ui_3dpi",
    english = "Dpi (resolution)",
    french = "Dpi (r\u00e9solution)",
    spanish = "Dpi (resoluci\u00f3n)"
  ),
  dplyr::tibble(
    id = "ui_3clbutton",
    english = "Get CL",
    french = "Obtenir bornes",
    spanish = "Obtener LC"
  ),
  dplyr::tibble(
    id = "ui_3cldesc1",
    english = "The selected % threshold to estimate hazard concentration is",
    french = "Le seuil (%) s\u00e9lectionn\u00e9 pour estimer la concentration pr\u00e9sentant un risque est de",
    spanish = "El umbral de % seleccionado para estimar la concentraci\u00f3n peligrosa es"
  ),
  dplyr::tibble(
    id = "ui_3cldesc11",
    english = "The selected hazard concentration to estimate % threshold is",
    french = "La concentration pr\u00e9sentant un risque s\u00e9lectionn\u00e9e pour estimer le seuil (%) est de",
    spanish = "La concentraci\u00f3n peligrosa seleccionada para estimar el umbral de % es"
  ),
  dplyr::tibble(
    id = "ui_3cldesc2",
    english = "and the number of bootstrap samples is",
    french = "et le nombre de simulations bootstrap est de",
    spanish = "y el n\u00famero de muestras bootstrap es"
  ),
  dplyr::tibble(
    id = "ui_3cldesc3",
    english = "It will take around",
    french = "Cela prendra environ",
    spanish = "Tomar\u00e1 alrededor de"
  ),
  dplyr::tibble(
    id = "ui_3cldesc4",
    english = "to generate confidence limits.",
    french = "pour g\u00e9n\u00e9rer les bornes de l'intervalle de confiance.",
    spanish = "para generar l\u00edmites de confianza."
  ),
  dplyr::tibble(
    id = "ui_4help",
    english = "Copy and paste code below to reproduce results. Code is added as functions are executed within the app. (e.g., code for generating confidence limits will appear after 'Get CL' is clicked.)",
    french = "Copier et coller le code R ci-dessous pour reproduire les r\u00e9sultats. Le code est ajout\u00e9 apr\u00e8s chaque ex\u00e9cution de fonctions dans l\u2019application (par exemple : le code qui g\u00e9n\u00e8re l\u2019estimation des intervalles de confiance apparaitra apr\u00e8s que 'Obtenir bornes' soit cliqu\u00e9).",
    spanish = "Copie y pegue el c\u00f3digo a continuaci\u00f3n para reproducir los resultados. El c\u00f3digo se agrega a medida que se ejecutan las funciones dentro de la aplicaci\u00f3n (por ejemplo, el c\u00f3digo para generar l\u00edmites de confianza aparecer\u00e1 despu\u00e9s de hacer clic en 'Obtener LC')."
  ),
  dplyr::tibble(
    id = "ui_getreport",
    english = "Get Report",
    french = "Obtenir le rapport",
    spanish = "Obtener Informe"
  ),
  dplyr::tibble(
    id = "ui_prevreport",
    english = "Preview report",
    french = "Aper\u00e7u du rapport",
    spanish = "Vista previa del informe"
  ),
  dplyr::tibble(
    id = "ui_draft",
    english = "This is a draft and may change at some point in the future.<br><br>",
    french = "Cette version est une \u00e9bauche et elle pourrait \u00eatre modifi\u00e9e en tout temps.<br><br>",
    spanish = "Este es un borrador y puede cambiar en alg\u00fan momento en el futuro.<br><br>"
  ),
  dplyr::tibble(
    id = "ui_about",
    english = "This webpage fits species sensitivity distributions to concentration data. The user is able to select more than one distribution and plot the individual fits. <br/><br/>The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). The prediction is the model averaged (using aicc) estimate of the fit. The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.<br/><br/>To cite package ssdtools in publications use:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>To cite the web app use:<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/",
    french = "Cette page Web ajuste les fonctions de distribution de sensibilit\u00e9 des esp\u00e8ces aux donn\u00e9es de concentration. L'utilisateur peut s\u00e9lectionner plus d'une distribution et repr\u00e9senter individuellement chaque courbe d'ajustement dans un graphique. <br/><br/> Les colonnes du tableau de l'\u00e9valuation de la qualit\u00e9 de l\u2019ajustement des courbes de distributiont sont la distribution (dist), la statistique d\u2019Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le crit\u00e8re d\u2019information Akaike (aic), le crit\u00e8re d\u2019information Akaike corrig\u00e9 pour la taille de l\u2019\u00e9chantillon (aicc), le crit\u00e8re d\u2019information Bay\u00e9sien (bic), la diff\u00e9rence entre AICc (delta) et la pond\u00e9ration des crit\u00e8res d'information AICc (coefficient de pond\u00e9ration). L\u2019estimation de la fonction de distribution finale est bas\u00e9e sur l\u2019inf\u00e9rence multimod\u00e8le (\u00e0 partir de l\u2019AICc). La concentration pr\u00e9sentant un risque est la concentration estim\u00e9e d\u2019une substance affectant un centile (seuil) s\u00e9lectionn\u00e9 de l\u2019ensemble des esp\u00e8ces.<br/><br/>Pour citer l\u2019application R \u2018ssdtools\u2019:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>Pour citer l\u2019application web :<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/"
  ),
  dplyr::tibble(
    id = "ui_hintdata",
    english = "You have not added a dataset.",
    french = "Aucun ensemble de donn\u00e9es n\u2019a \u00e9t\u00e9 ajout\u00e9.",
    spanish = "No ha agregado un conjunto de datos."
  ),
  dplyr::tibble(
    id = "ui_hintconc0",
    english = "Concentration must not be 0.",
    french = "La concentration ne doit pas \u00eatre \u00e0 z\u00e9ro.",
    spanish = "La concentraci\u00f3n no debe ser 0."
  ),
  dplyr::tibble(
    id = "ui_hintconcmiss",
    english = "Concentration must not be missing.",
    french = "La valeur de concentration ne doit pas \u00eatre vide.",
    spanish = "La concentraci\u00f3n no debe faltar."
  ),
  dplyr::tibble(
    id = "ui_hintnum",
    english = "Concentration column must contain number.",
    french = "La colonne 'concentration' doit contenir des nombres.",
    spanish = "La columna de concentraci\u00f3n debe contener n\u00fameros."
  ),
  dplyr::tibble(
    id = "ui_hintmiss",
    english = "Concentration values must not be missing.",
    french = "Aucune valeur de concentration ne doit \u00eatre manquante.",
    spanish = "Los valores de concentraci\u00f3n no deben faltar."
  ),
  dplyr::tibble(
    id = "ui_hintpos",
    english = "Concentration values must be positive.",
    french = "Les valeurs de concentration doivent \u00eatre positives.",
    spanish = "Los valores de concentraci\u00f3n deben ser positivos."
  ),
  dplyr::tibble(
    id = "ui_hintfin",
    english = "Concentration values must be finite.",
    french = "Les valeurs de concentration ne peuvent pas \u00eatre censur\u00e9es.",
    spanish = "Los valores de concentraci\u00f3n deben ser finitos."
  ),
  dplyr::tibble(
    id = "ui_hintident",
    english = "Concentration values must not all be identical.",
    french = "Les valeurs de concentration ne doivent pas \u00eatre toutes identiques.",
    spanish = "Los valores de concentraci\u00f3n no deben ser todos id\u00e9nticos."
  ),
  dplyr::tibble(
    id = "ui_hint6",
    english = "There must be at least 6 concentration values.",
    french = "Six valeurs de concentration sont minimalement requises.\u00a0",
    spanish = "Debe haber al menos 6 valores de concentraci\u00f3n."
  ),
  dplyr::tibble(
    id = "ui_hintdist",
    english = "At least one distribution must be selected.",
    french = "Au moins une distribution doit \u00eatre s\u00e9lectionn\u00e9e.",
    spanish = "Se debe seleccionar al menos una distribuci\u00f3n."
  ),
  dplyr::tibble(
    id = "ui_hintpred",
    english = "You must select the 'Fit' tab before using the 'Predict' tab.",
    french = "S\u00e9lectionner l\u2019onglet 'Ajustement' avant l\u2019onglet 'Estimation'.",
    spanish = "Debe seleccionar la pesta\u00f1a 'Ajuste' antes de usar la pesta\u00f1a 'Predicci\u00f3n'."
  ),
  dplyr::tibble(
    id = "ui_hintfit",
    english = "You have not successfully fit any distributions yet. Run the 'Fit' tab first.",
    french = "Aucune distribution n\u2019a encore \u00e9t\u00e9 ajust\u00e9e avec succ\u00e8s. Ex\u00e9cuter d'abord l\u2019onglet 'Ajustement'.",
    spanish = "A\u00fan no ha ajustado ninguna distribuci\u00f3n con \u00e9xito. Ejecute primero la pesta\u00f1a 'Ajuste'."
  ),
  dplyr::tibble(
    id = "ui_hintpredict",
    english = "You have not successfully generated predictions yet. Run the 'Predict' tab first.",
    french = "Aucune pr\u00e9diction n'a encore \u00e9t\u00e9 g\u00e9n\u00e9r\u00e9e avec succ\u00e8s. Ex\u00e9cutez d'abord l'onglet \u00ab Estimation \u00bb.",
    spanish = "A\u00fan no ha generado predicciones con \u00e9xito. Ejecute primero la pesta\u00f1a 'Predicci\u00f3n'."
  ),
  dplyr::tibble(
    id = "ui_hintfail",
    english = "distribution(s) failed to fit. Run R code to get more information.",
    french = "\u00e9chec dans l\u2019ajustement des distributions. Ex\u00e9cuter le code R pour plus d\u2019informations.",
    spanish = "distribuci\u00f3n(es) no se pudieron ajustar. Ejecute el c\u00f3digo R para obtener m\u00e1s informaci\u00f3n."
  ),
  dplyr::tibble(
    id = "ui_hintcolour",
    english = "Colour variable cannot be numeric.",
    french = "La variable de couleur ne peut pas \u00eatre num\u00e9rique.",
    spanish = "La variable de color no puede ser num\u00e9rica."
  ),
  dplyr::tibble(
    id = "ui_hintsym",
    english = "Symbol variable cannot be numeric.",
    french = "La colonne 'Symboles' ne doit pas contenir de nombres.",
    spanish = "La variable de s\u00edmbolo no puede ser num\u00e9rica."
  ),
  dplyr::tibble(
    id = "ui_hintthresh",
    english = "Affecting % species must not be missing.",
    french = "Le pourcentage d'esp\u00e8ces affect\u00e9es ne doit pas \u00eatre vide.",
    spanish = "El % de especies afectadas no debe faltar."
  ),
  dplyr::tibble(
    id = "ui_hintthreshpc",
    english = "Protecting % species must not be missing.",
    french = "Le pourcentage d'esp\u00e8ces prot\u00e9g\u00e9es ne doit pas \u00eatre vide.",
    spanish = "El % de especies protegidas no debe faltar."
  ),
  dplyr::tibble(
    id = "ui_hintboot",
    english = "Bootstrap samples must not be missing.",
    french = "Le nombre d'\u00e9chantillons bootstrap ne doit pas \u00eatre vide.",
    spanish = "Las muestras bootstrap no deben faltar."
  ),
  dplyr::tibble(
    id = "ui_copy",
    english = "Copy code",
    french = "Copier le code",
    spanish = "Copiar c\u00f3digo"
  ),
  dplyr::tibble(
    id = "ui_3conc",
    english = "Concentration",
    french = "Concentration",
    spanish = "Concentraci\u00f3n"
  ),
  dplyr::tibble(
    id = "ui_thresh_type",
    english = "Get estimate by",
    french = "Obtenir l\u2019estimation par",
    spanish = "Obtener estimaci\u00f3n por"
  ),
  dplyr::tibble(
    id = "ui_checkHc",
    english = "Plot Threshold/Concentration",
    french = "Graphique seuil/concentration",
    spanish = "Gr\u00e1fico Umbral/Concentraci\u00f3n"
  ),
  # dplyr::tibble(
  #   id = "ui_3hc",
  #   english = "The model averaged estimate of the concentration that affects {percent} % of species is {conc}",
  #   french = "Selon le mod\u00e8le agr\u00e9g\u00e9, la concentration affectant {percent} % des esp\u00e8ces est de {conc}"
  # ),
  # dplyr::tibble(
  #   id = "ui_3hc2",
  #   english = "The model averaged estimate of the fraction affected by a concentration of {conc} is {percent} % of species",
  #   french = "L'estimation par inf\u00e9rence multimod\u00e8le du seuil pour une concentration de {conc} est de {percent} % des esp\u00e8ces."
  # ),
  dplyr::tibble(
    id = "ui_1table1",
    english = "Table",
    french = "Table",
    spanish = "Tabla"
  ),
  dplyr::tibble(
    id = "ui_2rescale",
    english = "Rescale data prior to fitting",
    french = "Redimensionner les donn\u00e9es avant l'ajustement",
    spanish = "Reescalar datos antes de ajustar"
  ),
  dplyr::tibble(
    id = "ui_xmax",
    english = "X-axis maximum",
    french = "Maximum de l'axe des X",
    spanish = "M\u00e1ximo del eje X"
  ),
  dplyr::tibble(
    id = "ui_xmin",
    english = "X-axis minimum",
    french = "Minimum de l'axe des X",
    spanish = "M\u00ednimo del eje X"
  ),
  dplyr::tibble(
    id = "ui_xlog",
    english = "Log x-axis",
    french = "Log de l'axe X",
    spanish = "Eje X logar\u00edtmico"
  ),
  dplyr::tibble(
    id = "ui_sizeLabel",
    english = "Label size",
    french = "Taille de l'\u00e9tiquette",
    spanish = "Tama\u00f1o de etiqueta"
  ),
  dplyr::tibble(
    id = "ui_size",
    english = "Text size",
    french = "Taille du texte",
    spanish = "Tama\u00f1o de texto"
  ),
  dplyr::tibble(
    id = "ui_adjustLabel",
    english = "Shift label",
    french = "\u00c9tiquette de changement",
    spanish = "Desplazar etiqueta"
  ),
  dplyr::tibble(
    id = "ui_xbreaks",
    english = "X-axis ticks",
    french = "Rep\u00e8res de l'axe des X",
    spanish = "Marcas del eje X"
  ),
  dplyr::tibble(
    id = "ui_2unit",
    english = "Select units",
    french = "S\u00e9lectionner les unit\u00e9s",
    spanish = "Seleccionar unidades"
  ),
  dplyr::tibble(
    id = "ui_3byconc",
    english = "by concentration",
    french = "par concentration",
    spanish = "por concentraci\u00f3n"
  ),
  dplyr::tibble(
    id = "ui_3affecting",
    english = "affecting % species",
    french = "affectant % des esp\u00e8ces",
    spanish = "afectando % de especies"
  ),
  dplyr::tibble(
    id = "ui_3protecting",
    english = "protecting % species",
    french = "prot\u00e9geant % des esp\u00e8ces",
    spanish = "protegiendo % de especies"
  ),
  dplyr::tibble(
    id = "ui_1toxname",
    english = "Toxicant name (optional)",
    french = "Nom de la substance (optionnel)",
    spanish = "Nombre del t\u00f3xico (opcional)"
  ),
  dplyr::tibble(
    id = "ui_4toxname",
    english = "Toxicant name (optional)",
    french = "Nom de la substance (optionnel)",
    spanish = "Nombre del t\u00f3xico (opcional)"
  ),
  dplyr::tibble(
    id = "ui_4download",
    english = "Download Report",
    french = "T\u00e9l\u00e9charger le Rapport",
    spanish = "Descargar Informe"
  ),
  dplyr::tibble(
    id = "ui_4pdf",
    english = "PDF file",
    french = "Fichier PDF",
    spanish = "Archivo PDF"
  ),
  dplyr::tibble(
    id = "ui_4html",
    english = "HTML file",
    french = "Fichier HTML",
    spanish = "Archivo HTML"
  ),
  dplyr::tibble(
    id = "ui_4rmd",
    english = "RMD file",
    french = "Fichier RMD",
    spanish = "Archivo RMD"
  ),
  dplyr::tibble(
    id = "ui_4gentitle",
    english = "Generating report ...",
    french = "G\u00e9n\u00e9ration du rapport en cours ... ",
    spanish = "Generando informe ..."
  ),
  dplyr::tibble(
    id = "ui_4genbody",
    english = "This may take a minute, depending on the number of bootstrap samples selected.",
    french = "Cela peut prendre une minute, en fonction du nombre de simulations bootstrap s\u00e9lectionn\u00e9.",
    spanish = "Esto puede tomar un minuto, dependiendo del n\u00famero de muestras bootstrap seleccionadas."
  ),
  dplyr::tibble(
    id = "ui_1htconc",
    english = "Conc",
    french = "Conc",
    spanish = "Conc"
  ),
  dplyr::tibble(
    id = "ui_1htspp",
    english = "Species",
    french = "Esp\u00e8ce",
    spanish = "Especies"
  ),
  dplyr::tibble(
    id = "ui_1htgrp",
    english = "Group",
    french = "Groupe",
    spanish = "Grupo"
  ),
  dplyr::tibble(
    id = "ui_bcanz_file",
    english = "bcanz_report.Rmd",
    french = "bcanz_report_fr.Rmd",
    spanish = "bcanz_report.Rmd"
  ),
  dplyr::tibble(
    id = "ui_bcanz_filename",
    english = "bcanz_report",
    french = "rapport_bcanz",
    spanish = "informe_bcanz"
  ),
  dplyr::tibble(
    id = "ui_update_fit",
    english = "Update Fit",
    french = "Mettre \u00e0 jour l'ajustement",
    spanish = "Actualizar Ajuste"
  ),
  dplyr::tibble(
    id = "ui_update_data",
    english = "Update",
    french = "Mettre \u00e0 jour",
    spanish = "Actualizar"
  )
)

chk::check_key(translations, "id")

boron.data <- ssddata::ccme_boron

pal <- RColorBrewer::brewer.pal.info
pals <- pal[which(pal$category == "qual"), ] %>% row.names()

default.dists <- ssdtools::ssd_dists_bcanz()
extra.dists <- setdiff(ssdtools::ssd_dists_all(), default.dists)

usethis::use_data(
  boron.data,
  translations,
  pals,
  default.dists,
  extra.dists,
  internal = TRUE,
  overwrite = TRUE
)
