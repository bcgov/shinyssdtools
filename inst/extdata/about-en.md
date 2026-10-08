This app fits species sensitivity distributions to concentration data. 
The app is built from the R package [ssdtools](https://bcgov.github.io/ssdtools/), and shares the same functionality. 
It will be updated and redeployed following any relevant changes to ssdtools. 
It is recommended that when reporting the HC5 estimates generated using this app that the version of ssdtools and the name of the distributions fit to the dataset are listed.

For more information on the methods used, see the ssdtools articles:

- [Distributions](https://bcgov.github.io/ssdtools/articles/distributions.html) - probability distributions available for fitting SSDs
- [Model Averaging](https://bcgov.github.io/ssdtools/articles/model-averaging.html) - how model-averaged SSD estimates are calculated
- [Confidence Intervals](https://bcgov.github.io/ssdtools/articles/confidence-intervals.html) - methods for calculating uncertainty bounds on hazard concentrations
- [Small Sample Bias](https://bcgov.github.io/ssdtools/articles/small-sample-bias.html) - bias considerations when working with limited sample sizes
- [Customising Plots](https://bcgov.github.io/ssdtools/articles/customising-plots.html) - modifying SSD plot outputs
- [FAQs](https://bcgov.github.io/ssdtools/articles/faqs.html) - frequently asked questions

The columns in the goodness of fit table are the distribution (dist), the number of parameters (npars), the number of observations (nobs), the log-likelihood (log_lik), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), the AICc difference (delta), the AICc based Akaike weight (wt), Bayesian Information Criterion (bic), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), and the Cramer-von Mises statistic (cvm). 
The prediction is the model averaged (using aicc) estimate of the fit. 
The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.

To report a bug, unexpected behaviour, or to request a feature, please file a [GitHub issue here](https://github.com/poissonconsulting/shinyssdtools/issues).

To cite the ssdtools package in publications, use:
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. [ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492.](https://joss.theoj.org/papers/10.21105/joss.07492) `doi:10.21105/joss.07492`.

To cite the web app, use:
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. `https://bcgov-env.shinyapps.io/ssdtools/`

For more information on using model averaging to generate HC5 estimates, please see:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)