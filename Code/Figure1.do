/*-----------------------------------------------------------------------------

Objective. make Iceberg to create the integral

------------------------------------------------------------------------------*/


*					PATHS 


global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
global user "~/Dropbox/Research/Covid_los_andes" // for Ignacio Mac
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"

/*-----------------------------------------------------------------------------

							by Occupations

------------------------------------------------------------------------------*/

		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb19.dta", clear // always use the latest version

*Format dates
drop test_day 
replace fechadediligenciamiento=fechatomamuestra if fechadediligenciamiento=="" // is the one with the least number of missings
gen test_day = date(fechadediligenciamiento, "DMY") 

format test_day %td

drop if test_day==.

format test_day %td

gen mes=month(test_day)
gen year=year(test_day)
generate date_m = mofd(test_day)
format date_m %tm

drop if test_day==.
drop if ocup_cat==""

* drop those with covid contact or that had covid symptoms
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7


* create rate by worker category

bys date_m: asgen rate_pos=positive, w(weight_ocup)

collapse (mean) rate_pos, by(date_m)

* save this data base
sa "${data_fig}/Figura2_dta/all_covida.dta",replace


		* Now go to the data base of SDS and make the following

*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear


*Format dates

gen dateoftime=date(fecha_consulta,"DMY")
*generate dateoftime = dofc(fecha_consulta) // this used to be the way to do it before the new data.
replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td

gen mes=month(dateoftime)
gen year=year(dateoftime)
generate date_m = mofd(dateoftime)
format date_m %tm

* simply collapse now

collapse (sum) casos, by(date_m)

* clean a bit
*rename ocupacion_agregada ocup_cat

*drop if ocup_cat==""


* now merge with the covida data base

merge 1:1 date_m using  "${data_fig}/Figura2_dta/all_covida.dta"

* save this data base

sa "${data_fig}/Figura2_dta/all_cdf.dta", replace
use "${data_fig}/Figura2_dta/all_cdf.dta", clear


* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30  if date_m!=tm(2021m1) // this is for every month except january 
replace casos_day=casos/17 if date_m==tm(2021m1) // data is only until 17 Jan. 
gen casos_day_covida=(rate*8044713)/17
bys date_m: egen casos_day_tot=total(casos_day)
bys date_m: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida
drop if date_m==.
drop if date_m==tm(2021m2) // missings

* need this var after
bys date_m: egen casos_day_covida_mes=total(casos_day_covida)



* % detected aggregated July -  Jan all
	
egen av_day_cases_jan_covida_tot=mean(casos_day_covida_mes) if date_m>tm(2020m5)
gen tot_day_cases_jan_covida_tot=av_day_cases_jan_covida_tot*30*8
egen tot_day_cases_jan_sds_tot=total(casos) if date_m>tm(2020m5) // & mes<11
gen detected_jan=tot_day_cases_jan_sds_tot/tot_day_cases_jan_covida_tot
egen detected_jan_all=max(detected_jan)


* % detected aggregated July -  DEC all
	
egen av_day_cases_dec_covida_tot=mean(casos_day_covida_mes) if date_m>tm(2020m5) & date_m<tm(2021m1)
gen tot_day_cases_dec_covida_tot=av_day_cases_dec_covida_tot*30*8
egen tot_day_cases_dec_sds_tot=total(casos) if date_m>tm(2020m5) & date_m<tm(2021m1)
gen detected_dec=tot_day_cases_dec_sds_tot/tot_day_cases_dec_covida_tot
egen detected_dec_all=max(detected_dec)



** Generate total cases in SDS using % detected_dec

gen tot_sds_det_jan=(casos)/8044713
gen tot_sds_det_dec=(casos)/8044713


* Now total cases by covida?

gen tot_covida=30*casos_day_covida
gen accum_covida=tot_covida/8044713
tab accum_covida
tab tot_sds_det_jan

* daily cases by 100k inha


gen casos_day_covida100=casos_day_covida*100000/8044713
gen casos_day100=casos_day*100000/8044713


* Now the CDF


	unique date_m 
	local tope=r(unique)

	foreach x in accum_covida tot_sds_det_jan  {
	gen i_`x'=.

		forvalues n = 1(1)`tope' {
			cap drop i`n'
			 gen i`n' = 1 if  date_m[`n']>=date_m // & (year[`n']-year) <= 50
			 egen casos`n' = total(`x') if i`n'==1
			 replace i_`x' = casos`n' if mi(i_`x')
			drop  i`n'
			drop casos`n'
		}
}

*** Now figures








tsset date_m

twoway (tsline casos_day_covida100 if date_m>tm(2020m5), lcolor(black) lpattern(solid)) ///
(tsline casos_day100 if date_m>tm(2020m5), lcolor(black) lpattern(dash)), ///
legend(order (1 "CoVIDA" 2 "SDS") pos(6) row(1) ring(1)) ///
scheme(plotplainblind) ///
yti("Daily Cases per 100K Inhabitants") xti("")
gr export "${graphs}/Figura1/fig1_1.pdf", replace
 
tsset, clear

generate date_text2 = string(date_m, "%tm")
gen year=substr(date_text2,1,4)
gen month=substr(date_text2,6,.)
gen day="30"
gen date2=day+"/"+month+"/"+year

count
local plus1 = r(N) + 1
set obs `plus1'
replace date2="15/11/2020" if date2=="" 
gen Y_upper_ci=0.27 if date2=="15/11/2020"
gen Y_lower_ci=0.33 if date2=="15/11/2020"
gen marker=0.3 if date2=="15/11/2020"

gen date2day=date(date2,"DMY")
gen date2day2=date2day if date2=="15/11/2020"


format date2day %td

tsset date2day
                  


twoway (tsline i_accum_covida if date_m>=tm(2020m6), lcolor(black) lpattern(solid)) ///
(tsline i_tot_sds_det_jan if date_m>=tm(2020m6), lcolor(black) lpattern(dash)) ///
(rcap Y_upper_ci Y_lower_ci date2day2, lstyle(ci) lcolor(black)) ///
(scatter marker date2day2, mstyle(p1) msize(mdelarge)), ///
legend(order (1 "CoVIDA" 2 "SDS") pos(6) row(1) ring(1)) ///
scheme(plotplainblind) ///
yti("Accumulated Cases as % of Population") xti("") note("Using very period available and % detected from Jun - Jan")
gr export "${graphs}/Figura1/fig1_2.pdf", replace

/* Now using only from July onwards. 

drop i_* 
drop if date_m<=tm(2020m6)

	unique date_m 
	local tope=r(unique)

	foreach x in accum_covida tot_sds_det_jan  {
	gen i_`x'=.

		forvalues n = 1(1)`tope' {
			cap drop i`n'
			 gen i`n' = 1 if  date_m[`n']>=date_m // & (year[`n']-year) <= 50
			 egen casos`n' = total(`x') if i`n'==1
			 replace i_`x' = casos`n' if mi(i_`x')
			drop  i`n'
			drop casos`n'
		}
}


twoway (tsline i_accum_covida if date_m>tm(2020m6), lcolor(black) lpattern(solid)) ///
(tsline i_tot_sds_det_jan if date_m>tm(2020m6), lcolor(black) lpattern(dash)), ///
legend(order (1 "CoVIDA" 2 "SDS") pos(6) row(1) ring(1)) ///
scheme(plotplainblind) ///
yti("Accumulated Cases as % of Population") xti("") note("Using only from July onwards and % detected from Jul - Jan")
gr export "${graphs}/Figura1/fig1_3.pdf", replace







