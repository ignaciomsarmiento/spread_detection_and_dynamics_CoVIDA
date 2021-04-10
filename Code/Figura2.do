/*-----------------------------------------------------------------------------

Objective. make Iceberg to create the integral

------------------------------------------------------------------------------*/


*					PATHS 


global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"


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


* save this data
sa "${user}/COVIDA/iceberg/stratum_all.dta", replace



		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
*generate dateoftime = dofc(fecha_consulta) // this used to be the way to do it before the new data.
*replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
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
	
			* July December
		
	bys estrato_cam: egen av_day_cases_jul_dec_covida=mean(casos_day_covida) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida=av_day_cases_jul_dec_covida*6*30
	gen acumm_covid_covida_jul_dec=tot_day_cases_jul_dec_covida/pob  // this is the total percentage of people that already has had covid with COVIDA data. 
	
	
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
	
		* July Dec
	
	bys estrato_cam: egen tot_day_cases_jul_dec_sds=total(casos) if mes>6 & mes<13 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_dec_cat=tot_day_cases_jul_dec_sds/tot_day_cases_jul_dec_covida

		* July Jan
	
	bys estrato_cam: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida


	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	
	* July Dec
		
	egen av_day_cases_jul_dec_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida_all=av_day_cases_jul_dec_covida_tot*6*30
	egen tot_day_cases_jul_dec_sds_tot=total(casos) if mes>6 & mes<13
	gen detected_jul_dec=tot_day_cases_jul_dec_sds_tot/tot_day_cases_jul_dec_covida_all
	
	
	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct dec jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/pob
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/pob

}


sa "${user}/COVIDA/iceberg/full_iceberg_stratum", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_stratum", replace // use this data base to replicate and create CI 


******* Now Graph *******

label define estrat_lab 1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6"
label values estrato_cam estrat_lab


* First with COVIDA. 


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_dec , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

				
gr export "${graphs}/Figura2/sds_dec_stratum_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_stratum_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_dec_tot , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using aggregated % detected to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_dec_stratum_tot.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_jan_tot , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Stratum", size(small)) note("Using aggregated % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_stratum_tot.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_dec , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Stratum", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_dec_stratum.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(estrato_cam)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Stratum", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_jan_stratum.pdf", replace





/*-----------------------------------------------------------------------------

							by Occupations categories

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
drop if ocup_cat==""

* drop those with covid contact or that had covid symptoms
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

* create worker categories

gen essentialfel = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "cuidador de niños" | ocup_cat == "guardias seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" |  ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" 

replace essentialfel = 2 if ocup_cat =="artistas y actividades culturales" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines"   | ocup_cat == "servicios apoyo produccion" | ocup_cat == "personal de restaurantes"  | ocup_cat == "periodistas y escritores" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "personal limpieza" | ocup_cat == "vendedor tienda"  

replace essentialfel = 3 if ocup_cat == "abogados" |  ocup_cat == "ama de casa" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" |  ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores"  | ocup_cat =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label define esencia 1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential" 0 "NA"
label values essentialfel esencia

* 


* create population var

preserve

collapse (first) ocup_real_pop essentialfel, by(ocup_cat)
collapse (sum) ocup_real_pop, by(essentialfel)
sa "${user}/COVIDA/iceberg/pob_workers_cat.dta",replace

restore


* create rate by worker category

bys mes essentialfel: asgen rate_pos=positive, w(weight_ocup)

* merge with population

collapse (mean) rate_pos, by(mes essentialfel)
merge m:1 essentialfel using "${user}/COVIDA/iceberg/pob_workers_cat.dta", keep(3) nogen
sort essentialfel mes
drop if essentialfel==0
* save this data base
sa "${user}/COVIDA/iceberg/workers_cat_covida.dta",replace


		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
*generate dateoftime = dofc(fecha_consulta) // this used to be the way to do it before the new data.
*replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)

* gen worker categories

gen essentialfel = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "cuidador de niños" | ocup_cat == "guardias seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" |  ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" 

replace essentialfel = 2 if ocup_cat =="artistas y actividades culturales" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines"   | ocup_cat == "servicios apoyo produccion" | ocup_cat == "personal de restaurantes"  | ocup_cat == "periodistas y escritores" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "personal limpieza" | ocup_cat == "vendedor tienda"  

replace essentialfel = 3 if ocup_cat == "abogados" |  ocup_cat == "ama de casa" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" |  ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores"  | ocup_cat =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label values essentialfel esencia

* simply collapse now

collapse (sum) casos, by(essentialfel mes)

* now merge with the covida data base

merge 1:1 essentialfel mes using  "${user}/COVIDA/iceberg/workers_cat_covida.dta"
drop if essentialfel==0
* save this data base

sa "${user}/COVIDA/iceberg/workers_cat_all.dta", replace



* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30
gen casos_day_covida=(rate*ocup_real_pop)/17
bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida

* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	
	

* for July - October

	
	* First create the variables using COVIDA data.... 
	
		* July October
		
	bys essentialfel: egen av_day_cases_jul_oct_covida=mean(casos_day_covida) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida=av_day_cases_jul_oct_covida*4*30
	gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	
			* July December
		
	bys essentialfel: egen av_day_cases_jul_dec_covida=mean(casos_day_covida) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida=av_day_cases_jul_dec_covida*6*30
	gen acumm_covid_covida_jul_dec=tot_day_cases_jul_dec_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	
	
			* July January
			
	gen mes2=mes
	replace mes2=13 if mes==1 // its just easier to code it like this. 
		
	bys essentialfel: egen av_day_cases_jul_jan_covida=mean(casos_day_covida) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida=av_day_cases_jul_jan_covida*7*30
	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 

	
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	
	bys essentialfel: egen tot_day_cases_jul_oct_sds=total(casos) if mes>6 & mes<11 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_oct_cat=tot_day_cases_jul_oct_sds/tot_day_cases_jul_oct_covida
	
		* July Dec
	
	bys essentialfel: egen tot_day_cases_jul_dec_sds=total(casos) if mes>6 & mes<13 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_dec_cat=tot_day_cases_jul_dec_sds/tot_day_cases_jul_dec_covida

		* July Jan
	
	bys essentialfel: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida


	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	
	* July Dec
		
	egen av_day_cases_jul_dec_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida_all=av_day_cases_jul_dec_covida_tot*6*30
	egen tot_day_cases_jul_dec_sds_tot=total(casos) if mes>6 & mes<13
	gen detected_jul_dec=tot_day_cases_jul_dec_sds_tot/tot_day_cases_jul_dec_covida_all
	
	
	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct dec jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/ocup_real_pop
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/ocup_real_pop

}


sa "${user}/COVIDA/iceberg/full_iceberg_categories", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_categories", replace // use this data base to replicate and create CI 


******* Now Graph *******



* First with COVIDA. 


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_dec , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Workers' Categories", size(small)) note("Using % detected by category to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

				
gr export "${graphs}/Figura2/sds_dec_workers_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_jan , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Workers' Categories", size(small)) note("Using % detected by category to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_workers_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_dec_tot , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Workers' Categories", size(small)) note("Using aggregated % detected to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_dec_workers_tot.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_jan_tot , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Workers' Categories", size(small)) note("Using aggregated % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_workers_tot.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_dec , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Workers' Categories", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_dec_workers.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(essentialfel)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Workers' Categories", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_jan_workers.pdf", replace





/*-----------------------------------------------------------------------------

							by desaggregated Category

------------------------------------------------------------------------------*/









/*-----------------------------------------------------------------------------

							by Localidad

------------------------------------------------------------------------------*/



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

* generate my localidad variable

rename localidadderesidencianombredeloc localidad
tab localidad

			  
* exclude militaries in July

drop if ocup_cat=="militares y fuerza publica" &  mes==7

bys mes localidad: asgen rate_pos=positive, w(weight_ocup)

collapse (mean) rate_pos , by(mes localidad)

drop if localidad==""



*Generate Population var I used this document: http://200.69.105.199/sites/default/files/boletin69.pdf
* 7838  Sumapaz
gen pob_loc=108976 if localidad=="Antonio Nariño"
replace pob_loc=276453 if localidad=="Barrios Unidos"
replace pob_loc=799660 if localidad=="Bosa"
replace pob_loc=125294 if localidad=="Chapinero"
replace pob_loc=776351 if localidad=="Ciudad Bolivar"
replace pob_loc=892169 if localidad=="Engativa"
replace pob_loc=444951 if localidad=="Fontibon"
replace pob_loc=1273390 if localidad=="Kennedy"
replace pob_loc=21830  if localidad=="La Candelaria"
replace pob_loc=92234 if localidad=="Martires"
replace pob_loc=211802 if localidad=="Puente Aranda"
replace pob_loc=341886 if localidad=="Rafael Uribe Uribe"
replace pob_loc=387560 if localidad=="San Cristobal"
replace pob_loc=91111 if localidad=="Santa Fe"
replace pob_loc=1381597 if localidad=="Suba"
replace pob_loc=139369 if localidad=="Teusaquillo"
replace pob_loc=183067 if localidad=="Tunjuelito"
replace pob_loc=476931 if localidad=="Usaquen"
replace pob_loc=348332 if localidad=="Usme"

drop if pob_loc==. 

* save this data
sa "${user}/COVIDA/iceberg/localidad_all.dta", replace



		* Now deal with SDS data base



*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
rename ocupacion_agregada ocup_cat

* generate localidad_all

gen localidad= substr(localidadasis,6,.)

replace localidad="Ciudad Bolivar" if localidad=="Ciudad Bolívar"
replace localidad="Engativa" if localidad=="Engativá"
replace localidad="Fontibon" if localidad=="Fontibón"
replace localidad="Martires" if localidad=="Los Mártires"
replace localidad="San Cristobal" if localidad=="San Cristóbal"
replace localidad="Santa Fe" if localidad=="Santafe"
replace localidad="Usaquen" if localidad=="Usaquén"

*Generate Population var I used this document: http://200.69.105.199/sites/default/files/boletin69.pdf
* 7838  Sumapaz
gen pob_loc=108976 if localidad=="Antonio Nariño"
replace pob_loc=276453 if localidad=="Barrios Unidos"
replace pob_loc=799660 if localidad=="Bosa"
replace pob_loc=125294 if localidad=="Chapinero"
replace pob_loc=776351 if localidad=="Ciudad Bolivar"
replace pob_loc=892169 if localidad=="Engativa"
replace pob_loc=444951 if localidad=="Fontibon"
replace pob_loc=1273390 if localidad=="Kennedy"
replace pob_loc=21830  if localidad=="La Candelaria"
replace pob_loc=92234 if localidad=="Martires"
replace pob_loc=211802 if localidad=="Puente Aranda"
replace pob_loc=341886 if localidad=="Rafael Uribe Uribe"
replace pob_loc=387560 if localidad=="San Cristobal"
replace pob_loc=91111 if localidad=="Santa Fe"
replace pob_loc=1381597 if localidad=="Suba"
replace pob_loc=139369 if localidad=="Teusaquillo"
replace pob_loc=183067 if localidad=="Tunjuelito"
replace pob_loc=476931 if localidad=="Usaquen"
replace pob_loc=348332 if localidad=="Usme"
replace pob_loc=7838 if localidad=="Sumapaz"

drop if pob_loc==. 

*Format dates


gen dateoftime=date(fecha_consulta,"DMY")
*generate dateoftime = dofc(fecha_consulta) // this used to be the way to do it before the new data.
*replace dateoftime=dateoftime-5 // lag 5 days given data collection structure. This is new, to replicate previous tables simply drop this line. 
format dateoftime %td
gen mes=month(dateoftime)


drop if localidad==""





* simply collapse now

collapse (sum) casos, by(localidad mes)

* now merge with the covida data base

merge 1:1 localidad mes using  "${user}/COVIDA/iceberg/localidad_all.dta"


drop if localidad=="Sumapaz" // this is just in SDS

* save this data base

sa "${user}/COVIDA/iceberg/localidad_all.dta", replace



* 						Create the Iceberg
	* % detected each month
gen casos_day=casos/30
gen casos_day_covida=(rate*pob_loc)/17
bys mes: egen casos_day_tot=total(casos_day)
bys mes: egen casos_day_tot_covida=total(casos_day_covida)
gen detected_mes=casos_day_tot/casos_day_tot_covida

* need this var after
bys mes: egen casos_day_covida_mes=total(casos_day_covida)
	
	

* for July - October

	
	* First create the variables using COVIDA data.... 
	
		* July October
		
	bys localidad: egen av_day_cases_jul_oct_covida=mean(casos_day_covida) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida=av_day_cases_jul_oct_covida*4*30
	
	
	*gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. We need to calculate this after creating the % because of the replacement we want to make. 
	
			* July December
		
	bys localidad: egen av_day_cases_jul_dec_covida=mean(casos_day_covida) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida=av_day_cases_jul_dec_covida*6*30
	*gen acumm_covid_covida_jul_dec=tot_day_cases_jul_dec_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 
	
	
			* July January
			
	gen mes2=mes
	replace mes2=13 if mes==1 // its just easier to code it like this. 
		
	bys localidad: egen av_day_cases_jul_jan_covida=mean(casos_day_covida) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida=av_day_cases_jul_jan_covida*7*30
*	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/ocup_real_pop  // this is the total percentage of people that already has had covid with COVIDA data. 

	
	
	
* Now you can start creating the % detected to make the transformation of SDS data. 

* first by category in case we want to "be fancy"

	* July Oct
	
	bys localidad: egen tot_day_cases_jul_oct_sds=total(casos) if mes>6 & mes<11 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_oct_cat=tot_day_cases_jul_oct_sds/tot_day_cases_jul_oct_covida
	
	replace tot_day_cases_jul_oct_covida=tot_day_cases_jul_oct_sds if detected_jul_oct_cat>=1 // just take the number of SDS in case we predict somehting higher than 100%
	replace detected_jul_oct_cat=1 if detected_jul_oct_cat>1 & detected_jul_oct_cat!=.
	gen acumm_covid_covida_jul_oct=tot_day_cases_jul_oct_covida/pob_loc
	
		* July Dec
	
	bys localidad: egen tot_day_cases_jul_dec_sds=total(casos) if mes>6 & mes<13 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_dec_cat=tot_day_cases_jul_dec_sds/tot_day_cases_jul_dec_covida

	replace tot_day_cases_jul_dec_covida=tot_day_cases_jul_dec_sds if detected_jul_dec_cat>=1
	replace detected_jul_dec_cat=1 if detected_jul_dec_cat>1 & detected_jul_dec_cat!=.
	gen acumm_covid_covida_jul_dec=tot_day_cases_jul_dec_covida/pob_loc


		* July Jan
	
	bys localidad: egen tot_day_cases_jul_jan_sds=total(casos) if mes2>6 & mes2<14 // this is the ttal number of cases in the SDS data base. 
	gen detected_jul_jan_cat=tot_day_cases_jul_jan_sds/tot_day_cases_jul_jan_covida

	replace tot_day_cases_jul_jan_covida=tot_day_cases_jul_jan_sds if detected_jul_jan_cat>=1
	replace detected_jul_jan_cat=1 if detected_jul_jan_cat>1 & detected_jul_jan_cat!=.
	gen acumm_covid_covida_jul_jan=tot_day_cases_jul_jan_covida/pob_loc



	
	
	
	
* Now the aggregated measure

	* July Oct
		
	egen av_day_cases_jul_oct_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<11
	gen tot_day_cases_jul_oct_covida_all=av_day_cases_jul_oct_covida_tot*4*30
	egen tot_day_cases_jul_oct_sds_tot=total(casos) if mes>6 & mes<11
	gen detected_jul_oct=tot_day_cases_jul_oct_sds_tot/tot_day_cases_jul_oct_covida_all
	
	* July Dec
		
	egen av_day_cases_jul_dec_covida_tot=mean(casos_day_covida_mes) if mes>6 & mes<13
	gen tot_day_cases_jul_dec_covida_all=av_day_cases_jul_dec_covida_tot*6*30
	egen tot_day_cases_jul_dec_sds_tot=total(casos) if mes>6 & mes<13
	gen detected_jul_dec=tot_day_cases_jul_dec_sds_tot/tot_day_cases_jul_dec_covida_all
	
	
	* July Jan
		
	egen av_day_cases_jul_jan_covida_tot=mean(casos_day_covida_mes) if mes2>6 & mes2<14
	gen tot_day_cases_jul_jan_covida_all=av_day_cases_jul_jan_covida_tot*6*30
	egen tot_day_cases_jul_jan_sds_tot=total(casos) if mes2>6 & mes2<14
	gen detected_jul_jan=tot_day_cases_jul_jan_sds_tot/tot_day_cases_jul_jan_covida_all

* Transform the data of the SDS. easy

foreach x in oct dec jan{

* using the percentage by categories...

	gen acumm_covid_sds_jul_`x'=(tot_day_cases_jul_`x'_sds/detected_jul_`x'_cat)/pob_loc
	

* using the percentage aggregated..

	gen acumm_covid_sds_jul_`x'_tot=(tot_day_cases_jul_`x'_sds/detected_jul_`x')/pob_loc

}


sa "${user}/COVIDA/iceberg/full_iceberg_localidades", replace // use this data base to replicate and create CI 
sa "${data_fig}/Figura2_dta/full_iceberg_localidades", replace // use this data base to replicate and create CI 


******* Now Graph *******



* First with COVIDA. 


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_dec , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Localidades", size(small)) note("Using % detected by category to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

				
gr export "${graphs}/Figura2/sds_dec_localidad_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct acumm_covid_sds_jul_jan , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Localidades", size(small)) note("Using % detected by category to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_localidad_cats.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_dec_tot , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Localidades", size(small)) note("Using aggregated % detected to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_dec_localidad_tot.pdf", replace


graph dot acumm_covid_sds_jul_oct_tot acumm_covid_sds_jul_jan_tot , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("SDS data by Localidades", size(small)) note("Using aggregated % detected by stratum to extrapolate", size(vsmall)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/sds_jan_localidad_tot.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_dec , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Localidades", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - December" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_dec_localidad.pdf", replace


graph dot acumm_covid_covida_jul_oct acumm_covid_covida_jul_jan , over(localidad)  title("Predicted Accumulated Covid Cases as Percentage of Population") subti("CoVIDA data by Localidades", size(small)) scheme(plotplainblind) legend(pos(6) rows(1) order (1 "July - October" 2 "July - January" )) marker(1, msize(medlarge) mcolor(black)) marker(2, msize(medlarge) mcolor(black)) label

gr export "${graphs}/Figura2/covida_jan_localidad.pdf", replace


























