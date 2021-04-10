/*-----------------------------------------------------------------------------

Objective. make Iceberg to create the integral

------------------------------------------------------------------------------*/


*					PATHS 


global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
*global user "~/Dropbox/Research/Covid_los_andes" // for Ignacio Mac
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"



/*-----------------------------------------------------------------------------

							by Occupations categories

------------------------------------------------------------------------------*/
use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates
drop test_day 
replace fechadediligenciamiento=fechatomamuestra if fechadediligenciamiento=="" // is the one with the least number of missings
gen test_day = date(fechadediligenciamiento, "DMY") 

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


collapse (max) ocup_real_pop, by(ocup_cat)

replace ocup_cat="ingenieros" if ocup_cat=="ingeniero y servicios informaticos" | ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="restaurantes y tiendas" if  ocup_cat=="peluqueros y afines" | ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda" | ocup_cat=="vendedor ambulante"
replace ocup_cat="niños y limpieza" if ocup_cat=="cuidador de niños" | ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante y profesores" if ocup_cat=="profesores" | ocup_cat=="estudiante"
replace ocup_cat="obreros y carpinteros" if ocup_cat=="carpinteros y afines" | ocup_cat=="obreros de construccion"
replace ocup_cat="biologos, psicologos, afines" if ocup_cat=="biologo y afines" | ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y peridostas" if ocup_cat=="artistas y actividades culturales" | ocup_cat=="periodistas y escritores"


collapse (sum) ocup_real_pop, by(ocup_cat)

tempfile poblaciondes
sa `poblaciondes'

*Now continue..


		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates
drop test_day 
replace fechadediligenciamiento=fechatomamuestra if fechadediligenciamiento=="" // is the one with the least number of missings
gen test_day = date(fechadediligenciamiento, "DMY") 

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
replace ocup_cat="ingenieros" if ocup_cat=="ingeniero y servicios informaticos" | ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="restaurantes y tiendas" if  ocup_cat=="peluqueros y afines" | ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda" | ocup_cat=="vendedor ambulante"
replace ocup_cat="niños y limpieza" if ocup_cat=="cuidador de niños" | ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante y profesores" if ocup_cat=="profesores" | ocup_cat=="estudiante"
replace ocup_cat="obreros y carpinteros" if ocup_cat=="carpinteros y afines" | ocup_cat=="obreros de construccion"
replace ocup_cat="biologos, psicologos, afines" if ocup_cat=="biologo y afines" | ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y peridostas" if ocup_cat=="artistas y actividades culturales" | ocup_cat=="periodistas y escritores"

* 



generate date_m = mofd(test_day)
format date_m %tm



* create rate for the two cases....

bys ocup_cat: egen rate_pos_oct=mean(positive) if date_m<tm(2020m11)
bys ocup_cat: egen rate_pos_jan=mean(positive)

* merge with population

collapse (mean) rate_pos*, by(ocup_cat)

* create population var

merge m:1 ocup_cat using `poblaciondes', nogen

drop if ocup_cat==""
* save this data base
*sa "${user}/COVIDA/iceberg/workers_cat_covida.dta",replace
sa "${data_fig}/Figura2_dta/workers_cat_covida.dta",replace


		* Now deal with SDS data base



/*Open latest data base

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

generate date_m = mofd(dateoftime)
format date_m %tm


* generate two casos variables
bys ocup_cat: egen casos_oct=total(casos) if date_m<tm(2020m11)
bys ocup_cat: egen casos_jan=total(casos)


* simply collapse now

collapse (mean) casos_*, by(ocup_cat)

* now merge with the covida data base

merge 1:1 ocup_cat using  "${data_fig}/Figura2_dta/workers_cat_covida.dta"
drop if ocup_cat==""
* save this data base

sa "${data_fig}/Figura2_dta/workers_cat_all.dta", replace*/

use "${data_fig}/Figura2_dta/workers_cat_covida.dta", clear

* 						Create the Iceberg


/* 	Number of days in SDS 
321	marzo-enero
245	marzo - octubre
290	Abril-enero
214	Abril - octubre

Number of days un COVIDA
Only this categories have decent numbers for June. For the rest, lets count starting in July
                personal transporte
           trabajadores de la salud 
123	Julio-octubre
215	Julio - enero


*/
	
/* for the SDS 

gen casos_day_oct_mar=casos_oct/245
gen casos_day_oct_abr=casos_oct/214
gen casos_day_jan_mar=casos_oct/321
gen casos_day_jan_abr=casos_oct/290
*/
* for the COVIDA....

gen casos_day_covida_jan=(rate_pos_jan*ocup_real_pop)/17
gen casos_day_covida_oct=(rate_pos_oct*ocup_real_pop)/17

*OCT

gen tot_day_cases_jul_oct_covida=casos_day_covida_oct*123
replace tot_day_cases_jul_oct_covida=casos_day_covida_oct*153 if inlist(ocup_cat,"personal transporte","trabajadores de la salud") // I think we should take out this restriction in the main figure and put it for the robustness. 
gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	
*Jan

gen tot_day_cases_jul_jan_covida=casos_day_covida_jan*215
replace tot_day_cases_jul_jan_covida=casos_day_covida_jan*245 if inlist(ocup_cat,"personal transporte","trabajadores de la salud") // I think we should take out this restriction in the main figure and put it for the robustness. 
gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	

* Now is ready.
	

sa "${user}/COVIDA/iceberg/full_iceberg_categories", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_categories", replace // use this data base to replicate and create CI 


******* Now Graph *******



* First with COVIDA. 



graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan if ocup_cat!="vendedor ambulante", over(ocup_cat, label(ang(50)) sort(acumm_covid_covida_jul_jan))  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Workers' Categories", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label vertical

gr export "${graphs}/Figura2/covida_jan_workers_1b.pdf", replace






/*-----------------------------------------------------------------------------

							by Stratum 
------------------------------------------------------------------------------*/




/*-----------------------------------------------------------------------------

							by Stratum (1&2 and 5&6 together)
------------------------------------------------------------------------------*/




		* First deal with COVIDA data base

*Open latest data base

use "${data}/Datos_Salesforce_treated_feb01.dta", clear

*Format dates

drop test_day 
replace fechadediligenciamiento=fechatomamuestra if fechadediligenciamiento=="" // is the one with the least number of missings
gen test_day = date(fechadediligenciamiento, "DMY") 

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
replace estrato_cam=1 if stratum==2 // putting 1 and 2 together
replace estrato_cam=5 if stratum==6 // putting 5 and 6 together

* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

generate date_m = mofd(test_day)
format date_m %tm



bys estrato_cam: asgen rate_pos_oct=positive, w(weight_ocup) if date_m<tm(2020m11)
bys estrato_cam: asgen rate_pos_jan=positive, w(weight_ocup) 

collapse (mean) rate_pos*, by(estrato_cam)

drop if estrato_cam==.


*Generate Population var 

 gen pob=4063470  if estrato_cam==1
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=365459   if estrato_cam==5
 
/*
 gen pob=735748  if estrato_cam==1
 replace pob=3327722   if estrato_cam==2
 replace pob=2857861   if estrato_cam==3
 replace pob=757923   if estrato_cam==4
 replace pob=365459   if estrato_cam==5
 
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
replace estrato_cam=1 if estrato_cam==2 // putting 5 and 6 together

drop if estrato_cam==.

* simply collapse now

generate date_m = mofd(dateoftime)
format date_m %tm


* generate two casos variables

bys ocup_cat: egen casos_oct=total(casos) if date_m<tm(2020m11)
bys ocup_cat: egen casos_jan=total(casos)

bys ocup_cat: egen casos_jun_oct=total(casos) if mes>6 & mes<11 
bys ocup_cat: egen casos_jun_jan=total(casos) if date_m>tm(2020m6) & date_m<=tm(2021m1) 

egen casos_jun_oct_tot=total(casos) if mes>6 & mes<11 
egen casos_jun_jan_tot=total(casos) if date_m>tm(2020m6) & date_m<=tm(2021m1) 


* simply collapse now

collapse (mean) casos_*, by(ocup_cat)


* now merge with the covida data base

merge 1:1 estrato_cam mes using  "${user}/COVIDA/iceberg/stratum_all.dta"
drop if estrato_cam==0
* save this data base

sa "${user}/COVIDA/iceberg/stratum_all.dta", replace


/* 	Number of days in SDS 
321	marzo-enero
245	marzo - octubre
290	Abril-enero
214	Abril - octubre

Number of days un COVIDA
123	Julio-octubre
215	Julio - enero


*/
	
* for the SDS 

gen casos_day_oct_mar=casos_oct/245
gen casos_day_oct_abr=casos_oct/214
gen casos_day_jan_mar=casos_oct/321
gen casos_day_jan_abr=casos_oct/290




* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30


gen casos_day_covida=(rate*pob)/17


bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida

* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	

	
	
* for the COVIDA....

gen casos_day_covida_jan=(rate_pos_jan*ocup_real_pop)/17
gen casos_day_covida_oct=(rate_pos_oct*ocup_real_pop)/17

egen casos_day_covida_jan_tot=total(casos_day_covida_jan)
egen casos_day_covida_oct_tot=total(casos_day_covida_oct)


*OCT

gen tot_day_cases_jul_oct_covida=casos_day_covida_oct*123
gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	
*Jan

gen tot_day_cases_jul_jan_covida=casos_day_covida_jan*215
gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	

	
		
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	gen detected_jul_oct_cat=casos_jun_oct/tot_day_cases_jul_oct_covida
	

		* July Jan
	gen detected_jul_jan_cat=casos_jun_oct/tot_day_cases_jul_jan_covida


	
	
	
* Now the aggregated measure

	* July Oct
		
	gen tot_day_cases_jul_oct_covida_all=casos_day_covida_oct_tot*123
	gen detected_jul_oct=casos_jun_oct_tot/tot_day_cases_jul_oct_covida_all
	
	
	
	* July Jan
		
	gen tot_day_cases_jul_jan_covida_all=casos_day_covida_jan_tot*215
	gen detected_jul_jan=casos_jun_jan_tot/tot_day_cases_jul_jan_covida_all
	
	

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
