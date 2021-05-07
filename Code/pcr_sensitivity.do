use "C:\Users\cdelo\Dropbox\Iceberg Paper\Data\pcr_sensitivity.dta", clear
gen mallet2= mallet if days>5
replace mallet=. if days>5
 grstyle init

 grstyle set plain, horizontal grid dotted


 twoway (line miller days, lpattern(solind) lcolor(black)) ||  ///
(line grassly days, lpattern(dash) lcolor(black)) || ///
(connected mallet days, color(black)) || ///
(connected mallet2 days, color(black)), ///
legend(order(1 "Miller et al. (2020)" 2 "Grassly et al. (2020)" 3 "Mallet et al. (2020)")) ///
legend(ring(0) col(1) pos(2))  graphregion(color(white)) ///
yti("PCR Sensitivity")

gr export "C:\Users\cdelo\Dropbox\Iceberg Paper\views\PCR_sensitivity.pdf", replace


