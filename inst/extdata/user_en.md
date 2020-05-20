This app **fits species sensitivity distributions to concentration data**. The app is built from the R package [ssdtools](https://github.com/bcgov/ssdtools), and shares the same functionality.


*Hint: Find and click the info icons  throughout the app to find more information on a particular input.*  

### Step 1: Provide data 

* Data should be provided for **only one chemical** at a time. 
* Each species should have only one concentration value. 
* Data must have **at least one column** containing **at least 8 distinct, positive, non-missing concentration values**. 
* Optionally, **species and group** columns can be included, which are used to label and color plot output, respectively.  
* Any additional columns are accepted but are not used by any functions.


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
    - Citation: [Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian  environmental  quality guidelines, 2009, Canadian Council of  Ministers of the Environment, Winnipeg.](http://ceqg-rcqe.ccme.ca/download/en/324/)
2. **Upload a csv file**. 
    - Excel file formats are not accepted. If you have an excel file, try exporting a worksheet to csv. 
3. **Fill out the interactive table**. 
    - Species and Group columns are optional. Click on a cell to begin entering data. Right-click on the table to delete/insert rows or columns. Column names cannot be changed. 
    
Finally, preview the data provided in the table on the right hand side of the tab.  

<center>
![Provide data in tab `1. Data`](https://media.giphy.com/media/fjyG7Wbgp1RKV8sS9s/giphy.gif)
</center>

### Step 2: Fit distributions 

1. Specify **which column contains concentration values**. The app attempts to guess which column contains concentration values based on data column names. This may need to be corrected.
2. **Select or deselect distributions to fit the data**.  Note that if two or more models have overlapping fits then support for this model shape will be over inflated in the model averaged parameters.  Please see the article [here](https://bcgov.github.io/ssdtools/articles/distributions.html) for more information.  The outputs may take a moment to update.
3. Format the plot using inputs in the sidebar and **download plot and goodness of fit table** as png and csv files, respectively.

<center>
![Fit distributions in tab `2. Fit`](https://media.giphy.com/media/yIjxGcFKt0zK2vj38j/giphy.gif)
</center>

Additional information about the **goodness of fit table**:
The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike’s Information Criterion (aic), Akaike’s Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). The prediction is the model averaged (using aicc) estimate of the fit. The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.

### Step 3: Predict hazard concentration or percent of species effected
1. Select the **threshold % species affected** to calculate **estimated hazard concentration** OR select **concentration** to calculate the percentage of species affected by a specified concentration. This affects the plot (dotted line), text displayed below the plot and calculations of confidence limits.  
2. Select the number of **bootstrap samples used to calculate confidence limits**. The recommended number of samples is 10,000, although this can take around 3 minutes to process. Select lower number of bootstrap samples to reduce processing time.  

<center>

Bootstrap Samples &nbsp;&nbsp; | Estimated Processing Time
--- | ---
10,000 &nbsp; | 45 seconds
5,000 &nbsp;| 20 seconds 
1,000 &nbsp;| 10 seconds
500 &nbsp;| 5 seconds

</center>

3. Since confidence limits take time to calculate, they are not calculated automatically; you must press the `Get CL` button.
4. **Format plot** using various inputs in sidebar and **download plot and table** as png and csv file, respectively.

<center>
![Get hazard concentration estimates and confidence limits in tab `3. Predict`](https://media.giphy.com/media/xKb9nQsPFlqTCGzUgF/giphy.gif)
</center>

### Step 4: Get R code

Copy R code to reproduce outputs programmatically. Code is dynamically generated based on user inputs and functions executed within the app (e.g., code for generating confidence limits will appear after 'Get CL' button is clicked). 

<center>
![Get R code to reproduce results programmatically in tab `R Code`](https://media.giphy.com/media/XIgsL03rRnEfn8nNas/giphy.gif)
</center>

To generate a graph with confidence bands, copy the R code and paste in R.  Then set ci = TRUE in the predict and ssd_plot functions.

 

