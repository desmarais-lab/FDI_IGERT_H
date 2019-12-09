
Includes folders to create datasets, run analysis, and create figures. 


Run scripts in order listed as some scripts create objects needed by other subsequent scripts. 


Folder: "create_datasets"

Scripts: 
1. raw_download.R - downloads files for FDI from UNCTAD
2. raw_clean_data.R - cleans the raw files downloaded from UNCTAD
3. clean_add_covariates.R - creates full panel
4. clean_subset_panel.R - subsets covariates and observations based on data 
				missingness and transform variables for analysis. 
				Files are written to the analysis folders.
			
							
Folder: "main"

Scripts: 
1. main_ergm_prep.R - breaks data down by year and appends into lists of networks and adjacent matrices for edge covariates
2. main_ergm.R - runs main models
3. ModelFitComparison.R - Figure 1
3. main_rl_plots.R - Figure 3 and Table 1
4. ModelInterpretation.R - Figure 4
5. NetworkStatisticsFit.R - Figure 2
6. CovariateInterpretation.R - Figure 5
7. contagion_simulation_interpretation.R - Figure 6


Folder: "supplementary"

Subfolders: 
1. TERGM
	Scripts:
	a. TERGM_OutsidePooledFunction.R - Runs time pooled ERGMS writes out results
	b. TERGM_rl_plots.R - produces plots of covariates for TERGM results
2. Omit_Tax_Havens - models with tax rates and without tax havens
	Scripts:
	a. tax_ergm_prep.R - preps data
	b. tax_ergm.R - Runs ERGMS and writes out results
	c. tax_plots.R - produces plots of covariates for results
	a. haven_ergm_prep.R - preps data
	b. haven_ergm.R - Runs ERGMS and writes out results
	c. haven_rl_plots.R - produces plots of covariates for results
3. Omit_EU
	Scripts:
	a. EU_ergm_prep.R - preps data
	b. EU_ergm.R - Runs ERGMS and writes out results
	c. EU_rl_plots.R - produces plots of covariates for results
4. Omit_Missing_Values
	Scripts:
	a. q_p.R - create subset lists
	b. q_ergm_prep.R - preps data
	c. q_ergm.R - Runs ERGMS and writes out results
	d. q_rl_plots.R - produces plots of covariates for results
5. Impute_Missing_Values
	Scripts:
	a. amelia_ergm_prep.R - preps data by creating imputed value datasets
	b. amelia_ergm.R - Runs ERGMS and writes out results
	c. amelia_rl_plots.R - produces plots of covariates for results





