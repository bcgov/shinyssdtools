Cette application web ajuste les fonctions de distribution de la sensibilité des espèces aux données de concentration. L'application est basée sur le progiciel R ssdtools version 0.2.0 et partage les mêmes fonctions. Les changements et les mises à niveau apportés à ssdtools vont résulter en modifications de cette application. Il est recommandé, lorsque l'estimation du HC5 généré par cette application est rapportée, d'inclure aussi la version de ssdtools et le nom des fonctions de distribution ajustées aux données.

Les colonnes du tableau de l'évaluation de la qualité de l’ajustement des courbes de distributiont sont la distribution (dist), la statistique d’Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le critère d’information Akaike (aic), le critère d’information Akaike corrigé pour la taille de l’échantillon (aicc), le critère d’information Bayésien (bic), la différence entre AICc (delta) et la pondération des critères d'information AICc (coefficient de pondération). L’estimation de la fonction de distribution finale est basée sur l’inférence multimodèle (à partir de l’AICc). La concentration présentant un risque est la concentration estimée d’une substance affectant un centile (seuil) sélectionné de l’ensemble des espèces.

Pour citer l’application R ‘ssdtools’:
Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082

Pour citer l’application web :
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/

Pour plus d'information sur l'utilisation de l'inférence multimodèle afin d'obtenir des estimations de HC5, veuillez consulter:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)