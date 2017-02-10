Data Preparation and Analysis

1. Downloading Data: FDI_download.R
	STP and CIV cannot be scraped due to Uni-code issues:Downloaded and formatted manually
	Had to rewrite \Sao Tao and Principe" in its le; Changed \Cote d?Voire" to \Ivory Coast"
	Does not run from makefile due to these manual changes

2. Cleaning Data: FDI_clean.R
	Gets rid of empty columns and empty rows, then appends all countries to together
	Gets rid of aggregates and adds country codes
	File output fdi clean.csv

3. Adding data: FDI_merge.R
	Adds control variables
	File output: replaces fdi_merge.csv

4. Subset data: FDI_subset.R
	Subsets data to create full edge list for all variables

5. Preliminary Results: fdi_MRQAP.R
	Runs OLS models
	Output: FE and MRQAP results
	
6. ERGM Analysis: fdi_ergmCount.R
	Runs analysis for different ERGM models
