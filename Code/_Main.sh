#!/bin/bash

R CMD  BATCH --vanilla 0_clean_covida &
R CMD  BATCH --vanilla 1a_fig1_analytic_calculations    &
R CMD  BATCH --vanilla 1b_fig1_analytic_plots     &


#End of Script
