## Fit distributions


1. Specify **which column contains concentration values**. The app attempts to guess which column contains concentration values based on data column names. This may need to be corrected.
2. **Select or deselect distributions to fit the data**.  
The fitted distributions plot includes the model average estimates. 
Note that if two or more models have overlapping fits then support for this model shape will be over inflated in the model averaged parameters.  
Please see the article [here](https://bcgov.github.io/ssdtools/articles/distributions.html) for more information.  
Click `Update fit` to update the outputs after changing selected distributions or data rescaling option. 
3. **Select whether to rescale data**
This specifies whether to leave the concentration values unchanged (unchecked) or to rescale concentration values by dividing by the geometric mean of the minimum and maximum positive finite values. Rescaling provides better numerical stability in cases where distributions are failing to fit. The estimates and goodness-of-fit statistics are unaffected. 
4. Format the plot using inputs in the sidebar and **download the plot** as PNG or RDS and the **goodness of fit table** as CSV or XLSX. Select units to display on the x-axis of the plot.

Additional information about the **goodness of fit table**:
The columns in the goodness of fit table are the distribution (dist), the number of parameters (npars), the number of observations (nobs), the log-likelihood (log_lik), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), the Information Criterion differences (delta), the Information Criterion weights (wt), Bayesian Information Criterion (bic), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), and the Cramer-von Mises statistic (cvm). 
The prediction is the model averaged (using aicc) estimate of the fit. 
The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.
