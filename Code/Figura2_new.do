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

							by Occupations categories

------------------------------------------------------------------------------*/
use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)

* create worker categories
replace ocup_cat = "personal de restaurantes" if ocupacion_desagregada == "cocineros"
drop if inlist(ocup_cat,"agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")


collapse (max) poblacion_agregada, by(ocup_cat)

replace ocup_cat="ingeniero y servicios informaticos" if ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="peluqueros y afines" if ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda"
replace ocup_cat="cuidador de niños" if ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante" if ocup_cat=="profesores"
replace ocup_cat="carpinteros y afines" if ocup_cat=="obreros de construccion"
replace ocup_cat="biologo y afines" if ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y actividades culturales" if ocup_cat=="periodistas y escritores"

collapse (sum) poblacion_agregada, by(ocup_cat)

tempfile poblaciondes
sa `poblaciondes'

*Now continue..


		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates

format test_day %td

drop if test_day==.

gen mes=month(test_day)
gen semana=week(test_day)

drop if test_day==.
drop if ocup_cat==""

* drop those with covid contact or that had covid symptoms
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

* create worker categories

replace ocup_cat = "personal de restaurantes" if ocupacion_desagregada == "cocineros"
drop if inlist(ocup_cat,"agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")
replace ocup_cat="ingeniero y servicios informaticos" if ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="peluqueros y afines" if ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda"
replace ocup_cat="cuidador de niños" if ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante" if ocup_cat=="profesores"
replace ocup_cat="carpinteros y afines" if ocup_cat=="obreros de construccion"
replace ocup_cat="biologo y afines" if ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y actividades culturales" if ocup_cat=="periodistas y escritores"


* 






* create rate by worker category

bys mes ocup_cat: egen rate_pos=mean(positive)

* merge with population

collapse (mean) rate_pos, by(mes ocup_cat)

* create population var

merge m:1 ocup_cat using `poblaciondes', nogen

drop if ocup_cat==""
* save this data base
*sa "${user}/COVIDA/iceberg/workers_cat_covida.dta",replace
sa "${data_fig}/Figura2_dta/workers_cat_covida.dta",replace


		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
*generate dateoftime = dofc(fecha_consulta) // this used to be the way to do it before the new data.
replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)

* gen worker categories

replace ocup_cat = "personal de restaurantes" if ocupacion_desagregada == "cocineros"
drop if inlist(ocup_cat,"agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")
replace ocup_cat="ingeniero y servicios informaticos" if ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="peluqueros y afines" if ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda"
replace ocup_cat="cuidador de niños" if ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante" if ocup_cat=="profesores"
replace ocup_cat="carpinteros y afines" if ocup_cat=="obreros de construccion"
replace ocup_cat="biologo y afines" if ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y actividades culturales" if ocup_cat=="periodistas y escritores"


* simply collapse now

collapse (sum) casos, by(ocup_cat mes)

* now merge with the covida data base

merge 1:1 ocup_cat mes using  "${data_fig}/Figura2_dta/workers_cat_covida.dta"
drop if ocup_cat==""
* save this data base

sa "${data_fig}/Figura2_dta/workers_cat_all.dta", replace

use "${data_fig}/Figura2_dta/workers_cat_all.dta", clear

* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30
gen casos_day_covida=(rate_pos*poblacion_agregada)/17
bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida
egen totl_casos_day_tot_covida=total(casos_day_covida)
* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	
	

* for July - October

	
	* First create the variables using COVIDA data.... 
	
		* July October
		
	bys ocup_cat: egen av_day_cases_jul_oct_covida=mean(casos_day_covida) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida=av_day_cases_jul_oct_covida*4*30
	gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/poblacion_agregada  // this is the total percentage of people that already has had covid with COVIDA data. 
	

			* July January
			
	gen mes2=mes
	replace mes2=13 if mes==1 // its just easier to code it like this. 
		
	bys ocup_cat: egen av_day_cases_jul_jan_covida=mean(casos_day_covida) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida=av_day_cases_jul_jan_covida*7*30
	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/poblacion_agregada  // this is the total percentage of people that already has had covid with COVIDA data. 

	
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	
	bys ocup_cat: egen tot_day_cases_jul_oct_sds=total(casos) if mes>6 & mes<11 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_oct_cat=tot_day_cases_jul_oct_sds/tot_day_cases_jul_oct_covida
	
		* July Jan
	
	bys ocup_cat: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida


	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	

	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/poblacion_agregada
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/poblacion_agregada

}


sa "${user}/COVIDA/iceberg/full_iceberg_categories", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_categories", replace // use this data base to replicate and create CI 


******* Now Graph *******



* First with COVIDA. 



graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(ocup_cat, label(ang(50)) sort(acumm_covid_covida_jul_jan))  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Workers' Categories", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/covida_jan_workers_1.pdf", replace

graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(ocup_cat, sort(acumm_covid_covida_jul_jan))  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Workers' Categories", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_jan_workers_2.pdf", replace


* with SDS? We said this will be with CoVIDA. so nop..



/*-----------------------------------------------------------------------------

							by Stratum 
------------------------------------------------------------------------------*/




		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates

format test_day %td

drop if test_day==.

gen mes=month(test_day)
gen semana=week(test_day)

drop if test_day==.


* Out those with covid symptoms or contact

gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1
gen  estrato_cam=stratum
/* generate my stratum variable (merging 5 & 6)
gen estrato_cam=stratum
replace estrato_cam=5 if stratum==6 // putting 5 and 6 together
*/
* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

bys mes estrato_cam: asgen rate_pos=positive, w(weight_ocup)

collapse (mean) rate_pos, by(mes estrato_cam)

drop if estrato_cam==.


*Generate Population var 
/*
 gen pob=735748  if estrato_cam==1
 replace pob=3327722   if estrato_cam==2
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=365459   if estrato_cam==5
*/
 gen pob=735748  if estrato_cam==1
 replace pob=3327722   if estrato_cam==2
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=240570   if estrato_cam==5
 replace pob=124889   if estrato_cam==6

 
 
 
* save this data
*sa "${user}/COVIDA/iceberg/stratum_all.dta", replace
sa "${data_fig}/stratum_all.dta", replace


		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)

* gen stratum categories

gen estrato_cam=estratosocioeconomico
drop if estrato_cam=="SD"
destring estrato_cam, replace
*replace estrato_cam=5 if estrato_cam==6 // putting 5 and 6 together

drop if estrato_cam==.

* simply collapse now

collapse (sum) casos, by(estrato_cam mes)

* now merge with the covida data base

*merge 1:1 estrato_cam mes using  "${user}/COVIDA/iceberg/stratum_all.dta"
merge 1:1 estrato_cam mes using "${data_fig}/stratum_all.dta"
drop if estrato_cam==0
* save this data base

*sa "${user}/COVIDA/iceberg/stratum_all.dta", replace

sa "${data_fig}/stratum_all.dta", replace


* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30
gen casos_day_covida=(rate*pob)/17
bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida

* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	

* for July - October

	
	* First create the variables using COVIDA data.... 
	
		* July October
		
	bys estrato_cam: egen av_day_cases_jul_oct_covida=mean(casos_day_covida) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida=av_day_cases_jul_oct_covida*4*30
	gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/pob  // this is the total percentage of people that already has had covid with COVIDA data. 
		
	
			* July January
			
	gen mes2=mes
	replace mes2=13 if mes==1 // its just easier to code it like this. 
		
	bys estrato_cam: egen av_day_cases_jul_jan_covida=mean(casos_day_covida) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida=av_day_cases_jul_jan_covida*7*30
	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/pob  // this is the total percentage of people that already has had covid with COVIDA data. 

	
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	
	bys estrato_cam: egen tot_day_cases_jul_oct_sds=total(casos) if mes>6 & mes<11 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_oct_cat=tot_day_cases_jul_oct_sds/tot_day_cases_jul_oct_covida
	

		* July Jan
	
	bys estrato_cam: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida


	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	
	
	
	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct  jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/pob
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/pob

}


sa "${user}/COVIDA/iceberg/full_iceberg_stratum", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_stratum", replace // use this data base to replicate and create CI 


******* Now Graph *******
use "${user}/COVIDA/iceberg/full_iceberg_stratum", clear // use this data base to replicate and create CI 


label define estrat_lab 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6"
label values estrato_cam estrat_lab


* First with COVIDA. 




graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/sds_jan_stratum_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_jan_tot , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using aggregated % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/sds_jan_stratum_tot.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Stratum", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/covida_jan_stratum.pdf", replace



/*-----------------------------------------------------------------------------

							by Stratum (5&6 together)
------------------------------------------------------------------------------*/




		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates

format test_day %td

drop if test_day==.

gen mes=month(test_day)
gen semana=week(test_day)

drop if test_day==.


* Out those with covid symptoms or contact

gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1
*gen estrato_cam=stratum
* generate my stratum variable (merging 5 & 6)
gen estrato_cam=stratum
replace estrato_cam=5 if stratum==6 // putting 5 and 6 together

* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

bys mes estrato_cam: asgen rate_pos=positive, w(weight_ocup)

collapse (mean) rate_pos, by(mes estrato_cam)

drop if estrato_cam==.


*Generate Population var 

 gen pob=735748  if estrato_cam==1
 replace pob=3327722   if estrato_cam==2
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=365459   if estrato_cam==5
/*
 gen pob=735748  if estrato_cam==1
 replace pob=3327722   if estrato_cam==2
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=240570   if estrato_cam==5
 replace pob=124889   if estrato_cam==6
*/
 
 
 
* save this data
sa "${user}/COVIDA/iceberg/stratum_all.dta", replace



		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)

* gen stratum categories

gen estrato_cam=estratosocioeconomico
drop if estrato_cam=="SD"
destring estrato_cam, replace
replace estrato_cam=5 if estrato_cam==6 // putting 5 and 6 together

drop if estrato_cam==.

* simply collapse now

collapse (sum) casos, by(estrato_cam mes)

* now merge with the covida data base

merge 1:1 estrato_cam mes using  "${user}/COVIDA/iceberg/stratum_all.dta"
drop if estrato_cam==0
* save this data base

sa "${user}/COVIDA/iceberg/stratum_all.dta", replace




* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30
gen casos_day_covida=(rate*pob)/17
bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida

* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	

* for July - October

	
	* First create the variables using COVIDA data.... 
	
		* July October
		
	bys estrato_cam: egen av_day_cases_jul_oct_covida=mean(casos_day_covida) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida=av_day_cases_jul_oct_covida*4*30
	gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/pob  // this is the total percentage of people that already has had covid with COVIDA data. 
		
	
			* July January
			
	gen mes2=mes
	replace mes2=13 if mes==1 // its just easier to code it like this. 
		
	bys estrato_cam: egen av_day_cases_jul_jan_covida=mean(casos_day_covida) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida=av_day_cases_jul_jan_covida*7*30
	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/pob  // this is the total percentage of people that already has had covid with COVIDA data. 

	
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	
	bys estrato_cam: egen tot_day_cases_jul_oct_sds=total(casos) if mes>6 & mes<11 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_oct_cat=tot_day_cases_jul_oct_sds/tot_day_cases_jul_oct_covida
	

		* July Jan
	
	bys estrato_cam: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida


	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	
	
	
	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct  jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/pob
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/pob

}


sa "${user}/COVIDA/iceberg/full_iceberg_stratum", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_stratum", replace // use this data base to replicate and create CI 


******* Now Graph *******
use "${user}/COVIDA/iceberg/full_iceberg_stratum", clear // use this data base to replicate and create CI 


label define estrat_lab 1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" 
label values estrato_cam estrat_lab


* First with COVIDA. 




graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/sds_jan_stratum_cats2.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_jan_tot , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using aggregated % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/sds_jan_stratum_tot2.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Stratum", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/covida_jan_stratum2.pdf", replace
