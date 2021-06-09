#!/bin/zsh


echo ""
echo "**********************************************************************************************************"
echo "Start Replication Files for SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America" 
echo "**********************************************************************************************************"

#creates folders
mkdir ../views
mkdir ../Results_tables
mkdir ../Data/temp



# Run Codes in a Loop
echo "*" &&

for i in 1a_fig1_analytic_calculations.R 1b_fig1_analytic_plots.R 2_Fig2a_analytic.R 3_Fig2b_analytic.R 4_Fig2c_analytic.R 5_Fig3a_Ocupation_CoVIDA.R 6_Fig3b_Strata_CoVIDA.R 7_Fig3c_Strata_SDS.R 8_Fig3d_Strata_Deaths_SDS.R;
do
  echo "Running Main Figures  $i"
  R CMD  BATCH --vanilla $i 
done

for j in 9_FigS1_raw_positivity.R 10_Table_Positivity.R 11_fig1_analytic_no_weights.R 12_fig1_analytic_exclude_public_campaign.R 13_fig1_robust_days.R 14_sample_evolution.R 15_Table_One_case_detected_of.R 16_Table_Epidemiological_week.R 17_case_mortality_rate.R 18_Population_by_Locality.R 19_Population_by_Strata.R  20_sample_by_age.R 21_sample_by_gender.R 22_G_mobility.R 23_monthly_dynamics_occupation.R 24_localidades_maps.R 25_Fig_append_INSDET.R 26_r0_calc.R;
do
  echo "Running Supplementary Materials  $j"
  R CMD  BATCH --vanilla $j
done


#Move all log files to a single folder
mkdir logs
mv *.Rout logs/
#delete pdf
rm *.pdf &&


echo "**********************************************************************************************************"
echo "End Replication Files"
echo "**********************************************************************************************************"


#End of Script
