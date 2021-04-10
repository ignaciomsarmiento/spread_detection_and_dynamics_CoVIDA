
/*------------------------------------------------------------------------------

					Paths

------------------------------------------------------------------------------*/


*global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
*global user "~/Dropbox/Research/Covid_los_andes" // for Ignacio Mac
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"


/*------------------------------------------------------------------------------

					CLEANING SDS DATA

------------------------------------------------------------------------------*/

*use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
use "${data_sds}/casos_SDS_poblaciones_20Feb2021.dta", clear

* rename some vars

rename caso case_id
rename  fechainiciosintomas date_symptoms
rename  fecha_consulta date_consultation
rename  fechadiagnostresultlaboratorio date_results
rename  estratosocioeconomico stratum
rename  estrato stratum_2
rename  poblacion_estrato stratum_pop

* generate your stratum variable

destring stratum, replace force
replace stratum=1 if stratum==1 | stratum==2
replace stratum=2 if stratum==3
replace stratum=3 if stratum==4
replace stratum=4 if stratum==5 | stratum==6
replace upz="UPZ42" if upz=="UPZ42 " // does not change anythng as all upz is stratum 1. 

* create the stratum to impute.
preserve
	collapse (sum) casos, by(stratum upz)
	
	* Keep only the upz that are strings
	destring upz, gen(a) force
	drop if a!=. 
	drop a

	bys upz: egen tot_casos=total(casos)
	gen p=casos/tot_casos
	bys upz: egen max_p=max(p)
	keep if max_p==p
	duplicates tag upz, gen(dup)
	bys upz: egen min_strat=min(stratum) if dup==1
	drop if min_strat!=stratum & dup==1 // two upz have duplicates (2 cases in each stratum)
	rename stratum stratum_upz
	keep upz stratum_upz
	tempfile strat_imp
	sa `strat_imp'
	
restore

merge m:1 upz using `strat_imp'

* replace the missing stratums with UPZ stratum
replace stratum=stratum_upz if stratum==.

* Create your test day variable

gen test_day=date(date_consultation,"DMY") // we are only losing 200 observations. Nothing we can do, there is no var with date for those
replace test_day=test_day-5 // lag 5 days 
format test_day %td
drop if test_day==. // we lose 200 obs; only one obs now


* Save this data set

sa "${data_fig}/sds_dta.dta", replace
