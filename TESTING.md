# Steps for manual testing of the app

This document is to guide developers on how to manually test the app.
Steps should be followed in order and be done in a single session from start to finish.

1. Open app: https://bcgov-env.shinyapps.io/ssdtools/
2. Be on Data tab
	a. Click use boron dataset 
		i. Ensure preview dataset box populates with data
3. Click on Fit tab
	a. Ensure plot and fit table appear without errors
	b. Change select column with concentration values from Conc to Chemical
		i. It should provide a message saying you need to have your concentration column be a number 
		ii. Go back to Conc
	c. Add burrlll3 distribution
		i. Make sure it appears in plot and in table
	d. Remove gamma distribution 
		i. It should be removed from plot and table
	e. Change X-axis label to say Conc
		i. Label on plot should update
	f. Change Y-axis label to say Species 
		i. Label on plot should update
	g. Select unit dropdown, select mg/L
		i. Units on Conc should show mg/L in plot
	h. Increase text size to 30
		i. Text size on plot should increase
	i. Decrease text size to 2
		i. Text should be tiny 
	j. Test download buttons of plot.png, plot.rds and table.csv
		i. Objects should download, check png and csv that they open and look correct 
	k. Change values in width, height and Dpi boxes
		i. Nothing will change as it relates to download things but app should not crash or do anything weird 
4. Click on Predict tab
	a. Hit the Get CL button 
		i. Waiting screen should appear and then bring you back to app page with results when it is done calculating 
		ii. Table should be present under Get CL button 
	b. Test download buttons of plot.png, plot.rds and table.csv 
		i. Objects should download, check png and csv that they open and look correct 
	c. Click on fraction affected radio button
		i. Plot should update
	d. Increase by concentration to 1.5
		i. Plot should update and put dotted line at 1.5
	e. Change bootstrap samples to 500
		i. Nothing should update other then the text under the Get confidence limit section
	f. Change label by to chemical 
		i. Plot should update with chemical names instead of species 
		ii. Check through rest of options and ensure they do as expected and no errors occur and out back to Species when done 
	g. Change to colour by to units
		i. Points on plot should change colour and colour label appears on plot
	h. Change symbol by to chemical
		i. Symbols won't change as only 1 in this data set but symbol label should appear on plot
	i. Change colour palette to paired 
		i. Colours on plot should change colour
	j. Change x-axis label to Conc
		i. Plot should update with new label name
	k. Change y-axis label to Species 
		i. Plot should update with new label name 
	l. Add plot title of Title 
		i. Title should appear at top of plot
	m. Change colour legend title to Colour
		i. Colour legend title should update
	n. Change shape legend title to Shape
		i. Shape legend title should update  
	o. Change text size to 4
		i. Text should become really small 
	p. Change label size to 10
		i. Point labels should become really large 
		ii. Put back to 3
	q. Change shift label to 5
		i. The labels should shift to the right of the points
		ii. Put back to 1.05
	r. Change X-axis minimum to 10
		i. Should see plot shift for starting value on x-axis is 0
		ii. Put back to blank value
	s. Change X-axis maximum to 10
		i. Should see the x-axis switch to 10 and cut off a bunch of points
		ii. Put back to a blank value
	t. Untick Log x-axis
		i. Plot should change shape and not be on a log scale anymore
		ii. Re-tick box
	u. X-axis ticks
		i. Remove 100
			1) Value should disappear
		ii. Add 150
			1) Value should appear
	v. Hit download plot.png and confirm plot has the new changes we have made to it in the downloaded file
5. Go to the BCANZ Report tab
	a. Fill in toxicant name with "Toxic Frog"
	b. Click download Report
		i. Select PDF file
			1) Waiting screen will appear
			2) Confirm toxicant is listed in the top of the report
			3) Confirm the formatting and information looks correct
		ii. Select HTML file
			1) Waiting screen will appear
			2) Confirm toxicant is listed in the top of the report
			3) Confirm the formatting and information looks correct
		iii. Select RMD file
			1) Waiting screen will appear 
			2) Confirm toxicant is listed in the top of the report
			3) Confirm the formatting and information looks correct
6. Go to R Code tab
	a. Confirm R code is written and looks valid
7. Go to About tab
	a. Confirm formatting looks good
	b. Check links point to right things 
8. Go to User Guide tab
	a. Confirm formatting looks good
	b. Check links point to right things 
9. Click on French button
	a. User Guide tab should turn into French
	b. Go to the A propos tab 
		i. It should be in French
	c. Click on the English button
		i. Should turn back to English 
