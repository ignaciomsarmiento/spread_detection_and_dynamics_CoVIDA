#!/bin/bash

#!/bin/bash


echo ""
echo "**********************************************************************************************************"
echo "Start Replication Files for SARS-CoV-2 spread, detection, and dynamics in a megacity in Latin America" 
echo "**********************************************************************************************************"

# Main Figures 
echo "*" &&

R CMD  BATCH --vanilla 0_clean_covida 					&&
R CMD  BATCH --vanilla 1a_fig1_analytic_calculations    &&
R CMD  BATCH --vanilla 1b_fig1_analytic_plots     		&&
R CMD  BATCH --vanilla 1b_fig1_analytic_plots.R 		&&
R CMD  BATCH --vanilla 2_Fig2a_analytic.R 				&&
R CMD  BATCH --vanilla 3_Fig2b_analytic.R 				&&
R CMD  BATCH --vanilla 4_Fig2c_analytic.R 				&&
R CMD  BATCH --vanilla 5_Fig3a_Ocupation_CoVIDA.R 		&&
R CMD  BATCH --vanilla 6_Fig3b_Strata_CoVIDA.R 			&&
R CMD  BATCH --vanilla 7_Fig3c_Strata_SDS.R 			&&
R CMD  BATCH --vanilla 8_Fig3d_Strata_Deaths_SDS.R 		&&




#Move all log files to a single folder
mkdir Code/logs
mv *.Rout logs/
#delete pdf
rm *.pdf &&


echo "**********************************************************************************************************"
echo "End Replication Files"
echo "**********************************************************************************************************"


#End of Script
