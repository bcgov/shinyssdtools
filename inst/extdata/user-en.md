This app **fits species sensitivity distributions to concentration data**. The app is built from the R package [ssdtools](https://github.com/bcgov/ssdtools), and shares the same functionality. 
More information on ssdtools methods can be found in [articles on the website](https://bcgov.github.io/ssdtools/articles/).

*Hint: Find and click the info icons throughout the app to find more information on a particular input.*  

### Step 1: Provide data 

* Data should be provided for **only one chemical** at a time.
* Each species should have only one concentration value.
* Data must have **at least one column** containing **at least 6 positive, non-missing concentration values**.
* Any additional columns such as **species** or **group** are accepted but are only used by plotting functions if there are two or more unique values.

<center>

Concentration&nbsp;&nbsp; | Species&nbsp;&nbsp; | Group &nbsp;
--- | --- | ---
2.1 | Oncorhynchus mykiss &nbsp; | Fish
2.4 | Ictalurus punctatus &nbsp;| Fish  
4.1 | Micropterus salmoides &nbsp;| Fish
10  | Brachydanio rerio &nbsp;| Fish
15.6 | Carassius auratus &nbsp;| Fish
18.3 | Pimephales promelas &nbsp;| Fish 
6 | Daphnia magna &nbsp;| Invertebrate
10 | Opercularia bimarginata &nbsp;| Invertebrate

</center>

There are three options to provide data to the app:  

1. **Use the demo Boron dataset**. 
    - Quickly preview the app functionality on a dataset that 'works'. 
    - Citation: [Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian  environmental  quality guidelines, 2009, Canadian Council of  Ministers of the Environment, Winnipeg.](https://ccme.ca/en/res/boron-en-canadian-water-quality-guidelines-for-the-protection-of-aquatic-life.pdf)
2. **Upload a csv file**. 
    - Excel file formats are not accepted. If you have an excel file, try exporting a worksheet to csv. 
3. **Fill out the interactive table**. 
    - Species and Group columns are optional. Click on a cell to begin entering data. Right-click on the table to delete/insert rows or columns. Column names cannot be changed. 
    
Optionally, enter a **toxicant name** which will be used as the default plot title.

Finally, preview the data provided in the table on the right hand side of the tab. The dataset can be downloaded as CSV or XLSX.

### Step 2: Fit distributions 

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

### Step 3: Predict hazard concentration or percent of species affected
1. There are two options: Estimate the **Concentration** affecting/protecting a selected fraction of species (%) OR estimate the fraction of species (%) affected by a selected concentration. This affects the plot (dotted line), text displayed below the plot and calculations of confidence limits. 
2. Select the number of **bootstrap samples used to calculate confidence limits**. The recommended number of samples is 10,000, although this can take some time to process. 
Select a lower number of bootstrap samples to reduce processing time.  
Distributions are treated as constituting a single mixture distribution (as opposed to taking the mean) for calculation of model averaged estimates. 
Distributions are not treated as constituting a single distribution for calculating confidence intervals as this increases processing time considerably. 

3. Since confidence limits take time to calculate, they are not calculated automatically; you must press the `Get CL` button.
4. **Format plot** using various inputs in sidebar and **download the plot** as PNG or RDS and the **confidence limits table** as CSV or XLSX.

### Step 4: Get BCANZ report
Generate a report in HTML or PDF format including the fitted distribution plot, goodness of fit table, model-averaged fit plot and table of estimated hazardous/protective concentrations. 
Any options selected in the app will be incorporated into the report. 

### Step 5: Get R code

Copy R code to reproduce outputs programmatically. 
Code is dynamically generated based on user inputs and functions executed within the app (e.g., code for generating confidence limits will appear after `Get CL` button is clicked). 
 

