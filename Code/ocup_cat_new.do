
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"



* First create your new population measure. 

*Open latest data base

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

*Now merge the population with the new data

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
replace ocup_cat="ingeniero y servicios informaticos" if ocup_cat=="personal ingenieria mecanica"
replace ocup_cat="peluqueros y afines" if ocup_cat=="personal de restaurantes" | ocup_cat=="vendedor tienda"
replace ocup_cat="cuidador de niños" if ocup_cat=="personal limpieza"
replace ocup_cat="personal transporte" if ocup_cat=="taxistas"
replace ocup_cat="estudiante" if ocup_cat=="profesores"
replace ocup_cat="carpinteros y afines" if ocup_cat=="obreros de construccion"
replace ocup_cat="biologo y afines" if ocup_cat=="psicologos, sociologos y afines"
replace ocup_cat="artistas y actividades culturales" if ocup_cat=="periodistas y escritores"

* merge with pob

drop poblacion_agregada
merge m:1 ocup_cat using `poblaciondes' // ready for SDS




****** Now using CoVIDA data********

use "${data}/Datos_Salesforce_treated.dta", clear

* dates

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
rename ocup_real_pop poblacion_agregada
* for the population measures 
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

use "${data}/Datos_Salesforce_treated.dta", clear

* dates

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

* Now merge population

drop  ocup_real_pop

merge m:1 ocup_cat using `poblaciondes' // ready for covida. 










