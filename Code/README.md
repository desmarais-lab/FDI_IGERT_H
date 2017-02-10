5 Data Preparation

1. Downloading Data: FDI_download.R
 STP and CIV cannot be scraped due to Uni-code issues:Downloaded and formatted manually
 Had to rewrite \Sao Tao and Principe" in its le; Changed \Cote d?Voire" to \Ivory Coast"
 Does not run from makele due to these manual changes

2. Cleaning Data: FDI_clean.R
 Gets rid of empty columns and empty rows, then appends all countries to together
 Gets rid of aggregates and adds country codes
 File output fdi clean.csv

3. Adding data: FDI_merge.R
 Adds control variables
 File output: replaces fdi merge.csv

4. Subset data: FDI_subset.R
 Subsets data to create full edge list for all variables

5. Preliminary Results: fdi_MRQAP.R
 Runs OLS models
 Output: FE and MRQAP results
