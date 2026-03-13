Cette application ajuste des distributions de sensibilité des espèces aux données de concentration.
Elle est basée sur le progiciel R [ssdtools](https://bcgov.github.io/ssdtools/) et offre les mêmes fonctionnalités. 
L'application sera mise à jour et redéployée à la suite de toute modification pertinente apportée à ssdtools.
Lorsque vous rapportez les estimations de HC5 générées à l'aide de cette application, il est recommandé d'indiquer la version de ssdtools ainsi que le nom des distributions ajustées aux données.

Pour plus d'informations sur les méthodes utilisées, consultez les articles de ssdtools :

- [Distributions](https://bcgov.github.io/ssdtools/articles/distributions.html) - distributions de probabilité disponibles pour l'ajustement des DSE
- [Inférence multimodèle](https://bcgov.github.io/ssdtools/articles/model-averaging.html) - calcul des estimations de DSE moyennées entre modèles
- [Intervalles de confiance](https://bcgov.github.io/ssdtools/articles/confidence-intervals.html) - méthodes de calcul des bornes d'incertitude des concentrations dangereuses
- [Biais des petits échantillons](https://bcgov.github.io/ssdtools/articles/small-sample-bias.html) - considérations relatives aux biais lorsque la taille de l'échantillon est limitée
- [Personnalisation des graphiques](https://bcgov.github.io/ssdtools/articles/customising-plots.html) - modification des graphiques de DSE
- [FAQ](https://bcgov.github.io/ssdtools/articles/faqs.html) - questions fréquemment posées

Les colonnes du tableau de l’évaluation de la qualité de l’ajustement sont : la distribution (dist), le nombre de paramètres (npars), le nombre d’observations (nobs), la log-vraisemblance (log_lik), le critère d’information Akaike (aic), le critère d’information Akaike corrigé pour la taille de l’échantillon (aicc), la différence d’AICc (delta), le poids d’Akaike fondé sur l’AICc (wt), le critère d’information Bayésien (bic), la statistique d’Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks) et la statistique de Cramer-von Mises (cvm). 
L’estimation de la fonction de distribution finale est basée sur l’inférence multimodèle (à partir de l’AICc). 
La concentration présentant un risque est la concentration estimée d’une substance affectant un centile (seuil) sélectionné de l’ensemble des espèces.

Pour signaler un bogue, un comportement inattendu ou pour proposer une nouvelle fonctionnalité, veuillez soumettre un issue sur [GitHub à l’adresse suivante](https://github.com/poissonconsulting/shinyssdtools/issues).

Pour citer l’application R ‘ssdtools’ :
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492. doi:10.21105/joss.07492.

Pour citer l’application web :
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. `https://bcgov-env.shinyapps.io/ssdtools/`

Pour plus d'information sur l'utilisation de l'inférence multimodèle afin d'obtenir des estimations de HC5, veuillez consulter :
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)