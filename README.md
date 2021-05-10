# SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America

Data and replication files for "SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America" by  Rachid Laajaj, Camilo de los Rios, [Ignacio Sarmiento-Barbieri](https://ignaciomsarmiento.github.io), Danilo Aristizabal, Eduardo Behrentz, Raquel Bernal, Giancarlo Buitrago, Zulma Cucunubá, Fernando de la Hoz, Gabriela Delgado, Alejandro Gaviria, Luis Jorge Hernández, Leonardo León, Elkin Osorio, Andrea Ramírez Varela, Silvia Restrepo, Rodrigo Rodríguez, Martha Vives, Duncan Webb

The working paper can be found [here](https://repositorio.uniandes.edu.co/handle/1992/49763)

# Abstract

In many developing countries, the COVID-19 pandemic has spread much faster and wider than the number of detected cases implies. By combining data from 59,770 RT-PCR tests on mostly asymptomatic individuals with administrative data on all detected cases, we capture the spread and dynamics of the COVID-19 pandemic in Bogotá from June 2020 to early March 2021. Our data provide unusually broad and detailed information on mostly asymptomatic adults in Bogotá, allowing to describe various features of the pandemic that appear to be specific to a developing country context. We find that, by the end of March 2021, slightly more than half of the population in Bogotá has been infected, despite only a small fraction of this population being detected. In July 2020, after four months of generalized quarantine that mitigated the pandemic without curving it, the initial buildup of immunity contributed to the end of the first wave. We also show that the share of the population infected by February 2021 varies widely by occupation, socio-economic stratum, and location. This, in turn, has affected the dynamics of the spread: while the first wave of infections was driven by the lowest economic strata and highly-exposed occupations, the second peak affected the population more evenly. A better understanding of the spread and dynamics of the pandemic across different groups provides valuable guidance for efficient targeting of health policy measures and restrictions. 



## Data files

- `Data/Data_CoVIDA.dta` CoVIDA Sample
- `Data/sds_dta.dta`     HBS Sample
- `Data/pob_cats.dta`    Population by ocupational category
- `Data/pob_strat.dta`   Population by socioeconomic strata
- `Data/Region_Mobility_Report_CSVs/2020_CO_Region_Mobility_Report.csv` Data from Google mobility report
- `Data/translate.xlsx`  contains ocupational categories translated to english

## Software:

- The analysis is conducted using Stata-16 version 16.1 and R version 4.0.2 (2020-06-22) software

- All the code was run on a MacBookPro 2020 running macOS Big Sur Version 11.2.1

## Code files:

- `_Main.sh` Contains the sequence of execution of scripts to reproduce the figures and tables in the paper and appendix. It also creates a folder (logs) to house  log files: 

	
	
	- `0_clean_covida.R` cleans and prepares the covida data set to be used in the paper. Fixes dates and keeps the variables used in the analysis (for internal use, contains confidential data). Creates the database used in the paper `Datos_CoVIDA.dta`
	- `1a_fig1_analytic_calculations.R` generates the calculations for Figures 1a and 1b, saves it to `temp/Fig1_calculations.RData`
	- `1b_fig1_analytic_plots.R` plots  Figures 1a and 1b, calls `temp/Fig1_calculations.RData`
	- `2_Fig2a_analytic.R` plots Figure 2a
	- `3_Fig2b_analytic.R` plots Figure 2b
	- `4_Fig2c_analytic.R` plots Figure 2c and maps in Figure SI.9
	- `5_Fig3a_Ocupation_CoVIDA.R` plots Figure 3a and SI.7a
	- `6_Fig3b_Strata_CoVIDA.R` plots Figure 3b and SI.7b
	- `7_Fig3c_Strata_SDS.R` plots Figure 3c and SI.7c
	- `8_Fig3d_Strata_Deaths_SDS.R` plots Figure 3d and SI.7d
	- `9_Table_Positivy.txt` creates contents table Table Positivity rate using different CoVIDA subsamples
	- `10_fig1_analytic_no_weights.R` replicates Figure 1a and 1b excluding occupations weights.
	- `11_fig1_analytic_exclude_public_campaign.R` replicates Figure 1a and 1b excluding participants from the public campaign
	- `12_Table_One_case_detected_of.txt`
	- `13_Table_Epidemiological_week.R` creates Table SI.3. 
	- `14_case_mortality_rate.R`
	- `15_Population_by_Locality.txt` creates  Table SI.5
	- `16_Population_by_Strata.do` creates  Table SI.6
	- `17_G_mobility.R` plots Figure SI.6
	- `18_daily_dynamics_occupation.R` plots Figure SI.8.
	- `19_Fig_append_INSDET.R` plots Figure SI.10. a and b


## Figures and Tables

- Figures are saved in the `views` folder. 
- Tables are saved in the `Results_tables` folder




 
## Data dictionary

- `Data_CoVIDA.dta`:
	- `personaid`												  person id
	- `positive`                                                  =1 if tested positive
	- `test_day`                                                  day that the test was administered
	- `stratum`													  socioeconomic stratum
	- `date_m`													  month-year that the test was administerd
	- `mes`														  month (numeric) that the test was administerd
	- `year`													  year (numeric) that the test was administerd
	- `ocup_cat`												  ocupation category
	- `weight_ocup`												  weights: ocupation 
	- `weight_ocup_month`									      weights: ocupation x month
	- `localidad`												  locality where the individual resides
	- `sample`													  denotes the sources of the participating companies

- `sds_dta.dta`:

	- casos                                                                           cases
	

