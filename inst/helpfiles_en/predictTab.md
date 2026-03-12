## Estimate hazard concentration

1. There are two options: Estimate the **Concentration** affecting/protecting a selected fraction of species (%) OR estimate the fraction of species (%) affected by a selected concentration. This affects the plot (dotted line), text displayed below the plot and calculations of confidence limits. 
2. Select the number of **bootstrap samples used to calculate confidence limits**. The recommended number of samples is 10,000, although this can take some time to process. 
Select a lower number of bootstrap samples to reduce processing time.  
Distributions are treated as constituting a single mixture distribution (as opposed to taking the mean) for calculation of model averaged estimates. 
Distributions are not treated as constituting a single distribution for calculating confidence intervals as this increases processing time considerably. 

3. Since confidence limits take time to calculate, they are not calculated automatically; you must press the `Get CL` button.
4. **Format plot** using various inputs in sidebar and **download the plot** as PNG or RDS and the **confidence limits table** as CSV or XLSX.
