Esta aplicación ajusta distribuciones de sensibilidad de especies a datos de concentración.
La aplicación está construida a partir del paquete R [ssdtools](https://bcgov.github.io/ssdtools/) y comparte la misma funcionalidad.
Se actualizará y reimplementará después de cualquier cambio relevante en ssdtools.
Se recomienda que al informar las estimaciones de HC5 generadas usando esta aplicación, se enumeren la versión de ssdtools y el nombre de las distribuciones ajustadas al conjunto de datos.

Para más información sobre los métodos utilizados, consulte los artículos de ssdtools:

- [Distribuciones](https://bcgov.github.io/ssdtools/articles/distributions.html) - distribuciones de probabilidad disponibles para ajustar las DSE
- [Promediado de modelos](https://bcgov.github.io/ssdtools/articles/model-averaging.html) - cómo se calculan las estimaciones promediadas de las DSE
- [Intervalos de confianza](https://bcgov.github.io/ssdtools/articles/confidence-intervals.html) - métodos para calcular los límites de incertidumbre en las concentraciones peligrosas
- [Sesgo de muestras pequeñas](https://bcgov.github.io/ssdtools/articles/small-sample-bias.html) - consideraciones sobre el sesgo al trabajar con tamaños de muestra limitados
- [Personalización de gráficos](https://bcgov.github.io/ssdtools/articles/customising-plots.html) - modificación de los gráficos de DSE
- [Preguntas frecuentes](https://bcgov.github.io/ssdtools/articles/faqs.html) - preguntas frecuentes

Las columnas en la tabla de bondad de ajuste son la distribución (dist), el número de parámetros (npars), el número de observaciones (nobs), la log-verosimilitud (log_lik), el Criterio de Información de Akaike (aic), el Criterio de Información de Akaike corregido para el tamaño de la muestra (aicc), la diferencia de AICc (delta), el peso de Akaike basado en AICc (wt), el Criterio de Información Bayesiana (bic), la estadística de Anderson-Darling (ad), la estadística de Kolmogorov-Smirnov (ks) y la estadística de Cramer-von Mises (cvm).
La predicción es la estimación promediada del modelo (usando aicc) del ajuste.
La concentración de peligro porcentual es la concentración del químico que se predice que afectará ese porcentaje de las especies probadas.

Para informar un error, comportamiento inesperado o solicitar una función, presente un [problema de GitHub aquí](https://github.com/poissonconsulting/shinyssdtools/issues).

Para citar el paquete ssdtools en publicaciones, use:
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492. doi:10.21105/joss.07492.

Para citar la aplicación web, use:
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. `https://bcgov-env.shinyapps.io/ssdtools/`

Para obtener más información sobre el uso del promediado de modelos para generar estimaciones de HC5, consulte:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)
