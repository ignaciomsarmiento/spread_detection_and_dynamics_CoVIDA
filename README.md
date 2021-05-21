# SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America

Data and replication files for "SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America" by  [Rachid Laajaj](https://www.laajaj.com/), [Camilo de los Rios](https://sites.google.com/view/cdelosriosru), [Ignacio Sarmiento-Barbieri](https://ignaciomsarmiento.github.io), Danilo Aristizabal, Eduardo Behrentz, Raquel Bernal, Giancarlo Buitrago, Zulma Cucunubá, Fernando de la Hoz, Gabriela Delgado, Alejandro Gaviria, Luis Jorge Hernández, Leonardo León, Elkin Osorio, Andrea Ramírez Varela, Silvia Restrepo, Rodrigo Rodríguez, Martha Vives, Duncan Webb

The working paper can be found [here](https://repositorio.uniandes.edu.co/handle/1992/49763)

# Abstract

We implemented a COVID-19 sentinel epidemiological surveillance study with 59,770 RT-PCR tests on mostly asymptomatic individuals and combine this data with administrative records on all detected cases to capture the spread and dynamics of the COVID-19 pandemic in Bogotá from June 2020 to early March 2021. We describe various features of the pandemic that appear to be specific to a developing-country context. We find that, by March 2021, slightly more than half of the population in Bogotá has been infected, despite only a small fraction of this population being detected. The initial buildup of immunity contributed to the containment of the pandemic in the first and second waves. We also show that the share of the population infected by March 2021 varies widely by occupation, socio-economic stratum, and location. This, in turn, has affected the dynamics of the spread with different groups being infected in the two waves.


## Data files

All necesary data except HBS data are available. Some of the results require the HBS data on all positive detected cases from the Health Secretary of Bogota. We accessed this data through a sharing agreement and are not allowed to share the original data.

- `Data/Data_CoVIDA.dta` CoVIDA Sample
- `Data/pob_cats.dta`    Population by occupational category
- `Data/pob_loc.dta`	 Population by locality
- `Data/pob_strat.dta`   Population by socioeconomic strata
- `Data/Region_Mobility_Report_CSVs` Google mobility report
- `Data/translate.xlsx`  Occupational categories translated to English
- `Data/localidades` 	 Shapefiles of Localities in Bogota
- `Data/INS_detected.xlsx` Seropositivity and Detection Rate from the National Health Institute of Colombia 
- `Data/localidad_formaps.dta` Average stratum by locality




## Software:

- The analysis is conducted using Stata-16 version 16.1 and R version 4.0.2 (2020-06-22) software

- All the code was run on a MacBookPro 2020 running macOS Big Sur Version 11.2.1

## Code files:

- `_Main.sh` Contains the sequence of execution of scripts to reproduce the figures and tables in the paper and appendix. It also creates a folders to house  tables (Results_tables), figures (views), temporary data (Data/temp), and log files. To run it on a terminal with `zsh` shell,  type `sh _Main.sh &` : 
	
	- `1a_fig1_analytic_calculations.R` generates the calculations for Figures 1a and 1b, saves it to `temp/Fig1_calculations.RData`
	- `1b_fig1_analytic_plots.R` generates Figure "Accumulated and Daily Cases" panels (a) and (b), calls `temp/Fig1_calculations.RData`
	- `2_Fig2a_analytic.R` generates Figure "Accumulated Cases by Categories and Period" panel (a)
	- `3_Fig2b_analytic.R` generates Figure "Accumulated Cases by Categories and Period" panel (b)
	- `4_Fig2c_analytic.R` generates Figure "Accumulated Cases by Categories and Period" panel (c)
	- `5_Fig3a_Ocupation_CoVIDA.R` generates Figure "Monthly Dynamics" Panel (a) in main document and in supplementary materials
	- `6_Fig3b_Strata_CoVIDA.R` generates Figure "Monthly Dynamics" Panel (b) in main document and in supplementary materials
	- `7_Fig3c_Strata_SDS.R` generates Figure "Monthly Dynamics" Panel (c)
	- `8_Fig3d_Strata_Deaths_SDS.R` generates Figure "Monthly Dynamics" Panel (d)
	- `9_FigS1_raw_positivity.R` generates  Figure "Raw Positivity"
	- `10_Table_Positivity.R` creates table Table "Positivity rate using different CoVIDA subsamples"
	- `11_fig1_analytic_no_weights.R` replicates Figure "Accumulated and Daily Cases" panels (a) and (b) excluding occupations weights.
	- `12_fig1_analytic_exclude_public_campaign.R` replicates Figure "Accumulated and Daily Cases" panels (a) and (b) excluding participants from the public campaign
	- `13_fig1_robust_days.R` replicates Figure "Accumulated and Daily Cases" panels (a) and (b) assuming 15.5 instead of 17 days that an individual can test positive
	- `14_sample_evolution.R` generates Table  "Number of Tests by Month and Invitation Type"
	- `15_Table_One_case_detected_of.R` generates Table "One case detected out of.."
	- `16_Table_Epidemiological_week.R` generates Table "Positivity Rate and Epidemiological by Occupations"
	- `17_case_mortality_rate.R` generates Table "Case Mortality Rate"
	- `18_Population_by_Locality.R` generates  Table "Population by Locality"
	- `19_Population_by_Strata.R` generates Table "Population by Stratum"
	- `20_sample_by_age.R` generates Table "Positivity Rate by Age"
	- `21_sample_by_gender.R` generates Table "Positivity Rate by Gender"
	- `22_G_mobility.R` generates Figure "Mobility Changes by Location" 
	- `23_monthly_dynamics_occupation.R` generates Figure "Monthly Dynamics by Occupation"
	- `24_localidades_maps.R` generates Figure "Average Socio-economic Stratum and Dynamics by Locality" and Figure "Raw Positivity by Locality"
	- `25_Fig_append_INSDET.R` generates "SARS-CoV-2 Seropositivity and Detection Rate across Colombia"
	- `26_r0_calc.R` generates Figure "Estimate of the exponential rate of growth in new casesr"



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


	
	
	

