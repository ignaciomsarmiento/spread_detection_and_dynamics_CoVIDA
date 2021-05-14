# SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America

Data and replication files for "SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America" by  [Rachid Laajaj](https://www.laajaj.com/), [Camilo de los Rios](https://sites.google.com/view/cdelosriosru), [Ignacio Sarmiento-Barbieri](https://ignaciomsarmiento.github.io), Danilo Aristizabal, Eduardo Behrentz, Raquel Bernal, Giancarlo Buitrago, Zulma Cucunubá, Fernando de la Hoz, Gabriela Delgado, Alejandro Gaviria, Luis Jorge Hernández, Leonardo León, Elkin Osorio, Andrea Ramírez Varela, Silvia Restrepo, Rodrigo Rodríguez, Martha Vives, Duncan Webb

The working paper can be found [here](https://repositorio.uniandes.edu.co/handle/1992/49763)

# Abstract

We implemented a COVID-19 sentinel epidemiological surveillance study with 59,770 RT-PCR tests on mostly asymptomatic individuals and combine this data with administrative records on all detected cases to capture the spread and dynamics of the COVID-19 pandemic in Bogotá from June 2020 to early March 2021. We describe various features of the pandemic that appear to be specific to a developing country context. We find that, by March 2021, slightly more than half of the population in Bogotá has been infected, despite only a small fraction of this population being detected. The initial buildup of immunity contributed to the containment of the pandemic in the first and second waves. We also show that the share of the population infected by March 2021 varies widely by occupation, socio-economic stratum, and location. This, in turn, has affected the dynamics of the spread with different groups being infected in the two waves. 



## Data files

- `Data/Data_CoVIDA.dta` CoVIDA Sample
- `Data/sds_dta.dta`     HBS Sample
- `Data/pob_cats.dta`    Population by occupational category
- `Data/pob_strat.dta`   Population by socioeconomic strata
- `Data/Region_Mobility_Report_CSVs/2020_CO_Region_Mobility_Report.csv` Data from Google mobility report
- `Data/translate.xlsx`  contains occupational categories translated to English

## Software:

- The analysis is conducted using Stata-16 version 16.1 and R version 4.0.2 (2020-06-22) software

- All the code was run on a MacBookPro 2020 running macOS Big Sur Version 11.2.1

## Code files:

- `_Main.sh` Contains the sequence of execution of scripts to reproduce the figures and tables in the paper and appendix. It also creates a folder (logs) to house  log files: 
	
	- `1a_fig1_analytic_calculations.R` generates the calculations for Figures 1a and 1b, saves it to `temp/Fig1_calculations.RData`
	- `1b_fig1_analytic_plots.R` plots  Figures 1a and 1b, calls `temp/Fig1_calculations.RData`
	- `2_Fig2a_analytic.R` plots Figure 2a
	- `3_Fig2b_analytic.R` plots Figure 2b
	- `4_Fig2c_analytic.R` plots Figure 2c 
	- `5_Fig3a_Ocupation_CoVIDA.R` plots Figure 3a and SI.7a
	- `6_Fig3b_Strata_CoVIDA.R` plots Figure 3b and SI.7b
	- `7_Fig3c_Strata_SDS.R` plots Figure 3c 
	- `8_Fig3d_Strata_Deaths_SDS.R` plots Figure 3d 
	- `9_FigS1_raw_positivity.R` 
	- `10_Table_Positivity.R` creates table Table Positivity rate using different CoVIDA subsamples
	- `11_fig1_analytic_no_weights.R` replicates Figure 1a and 1b excluding occupations weights.
	- `12_fig1_analytic_exclude_public_campaign.R` replicates Figure 1a and 1b excluding participants from the public campaign
	- `13_fig1_robust_days.R` replicates Figure 1a and 1b assuming 15.3 instead of 17 days that an individual can test positive
	- `14_sample_evolution.R`
	- `15_Table_One_case_detected_of.R`
	- `16_Table_Epidemiological_week.R`
	- `17_case_mortality_rate.R`
	- `18_Population_by_Locality.R`
	- `19_Population_by_Strata.R` creates
	- `20_sample_by_age.R`
	- `21_sample_by_gender.R`
	- `22_G_mobility.R`
	- `23_monthly_dynamics_occupation.R`
	- `24_localidades_maps.R`
	- `25_Fig_append_INSDET.R` plots Figure SI.10. a and b




## Figures and Tables

- Figures are saved in a `views` folder. 
- Tables are saved in a `Results_tables` folder




 
## Data dictionary

- `Data_CoVIDA.dta`:
	- `personaid`												  person id
	- `positive`                                                  =1 if tested positive
	- `test_day`                                                  day that the test was administered
	- `stratum`													  socioeconomic stratum
	- `date_m`													  month-year that the test was administered
	- `mes`														  month (numeric) that the test was administered
	- `year`													  year (numeric) that the test was administered
	- `ocup_cat`												  occupation category
	- `desag_ocupation`											  disaggregated occupation
	- `weight_ocup`												  weights: occupation 
	- `weight_ocup_month`									      weights: occupation x month
	- `localidad`												  locality where the individual resides
	- `invite_sample`											  denotes the list from which participants were invited
	- `exclude_symptomatic`										  =1 excludes those with symptoms or known contact
	- `age group`												  age group the participant belongs
	- `gender`													  gender participant

- `sds_dta.dta`:
	- `casos`                                                       =1 if it was a reported case (the db includes all reported cases)	
	- `test_day`													date of consult 
	- `fechatomademuestra` 											date that the sample was taken
	- `recuperado`  												health status
	- `stratum` 													socioeconomic stratum
	- `localidadasis`												locality where the individual resides

