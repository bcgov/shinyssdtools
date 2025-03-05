This app fits species sensitivity distributions to concentration data. 
The app is built from the R package ssdtools, and shares the same functionality. 
Changes and upgrades in ssdtools will result in changes to this app. 
It is recommended that when reporting the HC5 estimates generated using this app that the version of ssdtools and the name of the distributions fit to the dataset are listed.

The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). 
The prediction is the model averaged (using aicc) estimate of the fit. 
The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.

To cite package ssdtools in publications use:
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492. doi:10.21105/joss.07492.

To cite the web app use:
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/

For more information on using model averaging to generate HC5 estimates, please see:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)