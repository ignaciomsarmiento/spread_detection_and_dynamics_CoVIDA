

/*

Author: Camilo De Los Rios
Objective: create graphs showing the positivity rate overtime for several occupation categories and socioeconomic stratum with COVIDA data


*/




/*
							COVIDA data graphs. First by categories, then by stratum
*/




* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

global user "~/Dropbox/Research/Covid_los_andes" // for Ignacio Mac

global data "${user}/covid-project/data/UNIANDES/processed"

*Open

use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

* Generate Categories of essential workers. 
/*
				
*/
gen essentialfel = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "cuidador de niños" | ocup_cat == "guardias de seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" |  ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" 

replace essentialfel = 2 if ocup_cat =="artistas y actividades culturales" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines"   | ocup_cat == "servicios apoyo produccion" | ocup_cat == "personal de restaurantes"  | ocup_cat == "periodistas y escritores" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "personal limpieza" | ocup_cat == "vendedor tienda"  

replace essentialfel = 3 if ocup_cat == "abogados" |  ocup_cat == "ama de casa" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" |  ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores"  | ocup_cat =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label define esencia 1 "essential" 2 "moderate essential" 3 "no essential" 0 "NA"
label values essentialfel esencia


* A somehow more aggregate measure of essential workers

gen essential2fel=1 if essentialfel==1 | essentialfel==2
replace essential2fel=2 if essentialfel==3
recode essential2fel(.=0)
label values essential2fel esencia2 



/*--------------------

			FOR THE WEIGHTED POSITIVITY RATES

------------------*/

*Now taking only those who did not have symtoms, contacts, covid contacts. 

gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

foreach y in fel {

	foreach x in 1 2 3{
			bys test_day: asgen ratew_ess`y'_`x'_excl=positive if essential`y'==`x' & exclude==0, w(weight_ocup)
			label var ratew_ess`y'_`x'_excl "weighted positive rate essential `y' category `x' and no symptoms etc"
		}


} 





/*
		Now simply collapse

*/



collapse (mean) ratew_*, by(test_day)


* Create the 30 day moving avergae for all the rates



foreach w in w {
	foreach y in fel {
		foreach z in  excl{

		
			foreach x in 1 2 3{
				rangestat (mean) mo30_rate`w'_ess`y'_`x'_`z' = rate`w'_ess`y'_`x'_`z', interval(test_day -30 0)
			}			
		}
	}
}




* Graph the first  

tsset test_day

foreach w in w {
	foreach y in fel {
		foreach z in excl{

				
		twoway 	(tsline mo30_rate`w'_ess`y'_1_`z', lcolor(black) lpattern(dash)) ///
				(tsline mo30_rate`w'_ess`y'_2_`z', lcolor(gs10) lpattern(solid)) ///
				(tsline mo30_rate`w'_ess`y'_3_`z', lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day MA") ylab(0(0.05)0.15) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v3/mo30`w'_ess`y'_`z'.pdf", replace	
	
		}
	}
}






* Stratum


	
use "${data}/Datos_Salesforce_treated.dta", clear
format test_day %td


* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together

forvalues x=1(1)5{
bys test_day: asgen ratew_`x'=positive if strat==`x', w(weight_ocup)
}


collapse (mean) ratew_*, by(test_day)

foreach y in w {
	forvalues x=1(1)5{
		rangestat (mean) mo30_rate`y'_strat_`x' = rate`y'_`x', interval(test_day -30 0)
	}
}

tsset  test_day 

foreach x in w{

twoway 	(tsline mo30_rate`x'_strat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate`x'_strat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_rate`x'_strat_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.18 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20)
		
		gr export "${user}/Apps/Overleaf/COVIDA/v3/mo30`x'_strat.pdf", replace
}







/*
							SDS data graphs. First by categories, then by stratum
*/

*Open

use "${user}/covid-project/data/SDS/processed/casos_SDS_poblaciones_23Jan2021.dta", clear
*use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta.dta", clear

keep fecha_consulta ocupacion_agregada casos ocupacion_agregada poblacion_agregada estratosocioeconomico  poblacion_estrato 


*gen date

*generate dateoftime = dofc(fecha_consulta)
generate dateoftime = date(fecha_consulta, "DMY")
*Format dates

format dateoftime %td

* Generate Categories of essential workers. 
/*
				According to felipe. 
*/
gen essentialfel = 1 if ocupacion_agregada == "agricultores y afines"  | ocupacion_agregada == "cuidador de niños" | ocupacion_agregada == "guardias de seguridad" | ocupacion_agregada == "mensajero" | ocupacion_agregada == "militares y fuerza publica" |  ocupacion_agregada == "personal servicios financieros" | ocupacion_agregada == "personal transporte" | ocupacion_agregada =="psicologos, sociologos y afines" | ocupacion_agregada == "taxistas" | ocupacion_agregada == "trabajadores de la salud" 

replace essentialfel = 2 if ocupacion_agregada =="artistas y actividades culturales" | ocupacion_agregada == "obreros de construccion" | ocupacion_agregada == "peluqueros y afines"   | ocupacion_agregada == "servicios apoyo produccion" | ocupacion_agregada == "personal de restaurantes"  | ocupacion_agregada == "periodistas y escritores" | ocupacion_agregada == "personal de servicio a bordo" | ocupacion_agregada == "personal limpieza" | ocupacion_agregada == "vendedor tienda"  

replace essentialfel = 3 if ocupacion_agregada == "abogados" |  ocupacion_agregada == "ama de casa" | ocupacion_agregada =="biologo y afines" | ocupacion_agregada =="carpinteros y afines" | ocupacion_agregada =="desempleado" | ocupacion_agregada =="directores y gerentes de empresas" | ocupacion_agregada =="entrenadores actividades deportivas" | ocupacion_agregada =="estudiante" | ocupacion_agregada =="ingeniero y servicios informaticos" | ocupacion_agregada =="pensionado" | ocupacion_agregada =="personal ingenieria mecanica" | ocupacion_agregada =="personal secretaria" |  ocupacion_agregada =="personal servicio comunitario" | ocupacion_agregada =="profesores"  | ocupacion_agregada =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label define esencia 1 "essential" 2 "moderate essential" 3 "no essential" 0 "NA"
label values essentialfel esencia


* A somehow more aggregate measure of essential workers

gen essential2fel=1 if essentialfel==1 | essentialfel==2
replace essential2fel=2 if essentialfel==3
recode essential2fel(.=0)
label values essential2fel esencia2 





/*

					Now lets make the number of cases per day per category

*/

foreach y in fel{
	
	foreach x in 1 2 3{
	bys fecha_consulta: egen casos_ess`y'_`x'_all=total(casos) if essential`y'==`x'
	label var casos_ess`y'_`x'_all "number of total daily cases essential `y' category `x'"
	}


}

*******************************************************************************


/*

Now I need a measure of the population per category of essential 
*/

preserve
	collapse (mean) poblacion_agregada, by(ocupacion_agregada)
	egen todos=total(poblacion_agregada)
	
			* Generate Categories of essential workers. 
		/*
						According to felipe. 
		*/
gen essentialfel = 1 if ocupacion_agregada == "agricultores y afines"  | ocupacion_agregada == "cuidador de niños" | ocupacion_agregada == "guardias de seguridad" | ocupacion_agregada == "mensajero" | ocupacion_agregada == "militares y fuerza publica" |  ocupacion_agregada == "personal servicios financieros" | ocupacion_agregada == "personal transporte" | ocupacion_agregada =="psicologos, sociologos y afines" | ocupacion_agregada == "taxistas" | ocupacion_agregada == "trabajadores de la salud" 

replace essentialfel = 2 if ocupacion_agregada =="artistas y actividades culturales" | ocupacion_agregada == "obreros de construccion" | ocupacion_agregada == "peluqueros y afines"   | ocupacion_agregada == "servicios apoyo produccion" | ocupacion_agregada == "personal de restaurantes"  | ocupacion_agregada == "periodistas y escritores" | ocupacion_agregada == "personal de servicio a bordo" | ocupacion_agregada == "personal limpieza" | ocupacion_agregada == "vendedor tienda"  

replace essentialfel = 3 if ocupacion_agregada == "abogados" |  ocupacion_agregada == "ama de casa" | ocupacion_agregada =="biologo y afines" | ocupacion_agregada =="carpinteros y afines" | ocupacion_agregada =="desempleado" | ocupacion_agregada =="directores y gerentes de empresas" | ocupacion_agregada =="entrenadores actividades deportivas" | ocupacion_agregada =="estudiante" | ocupacion_agregada =="ingeniero y servicios informaticos" | ocupacion_agregada =="pensionado" | ocupacion_agregada =="personal ingenieria mecanica" | ocupacion_agregada =="personal secretaria" |  ocupacion_agregada =="personal servicio comunitario" | ocupacion_agregada =="profesores"  | ocupacion_agregada =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label values essentialfel esencia


* A somehow more aggregate measure of essential workers

gen essential2fel=1 if essentialfel==1 | essentialfel==2
replace essential2fel=2 if essentialfel==3
recode essential2fel(.=0)
label values essential2fel esencia2 

		
		
foreach y in fel{
	
	foreach x in 1 2 3{
	egen pob_ess`y'_`x'_all=total(poblacion_agregada) if essential`y'==`x'
	label var  pob_ess`y'_`x'_all "población agregada essential `y' category `x'"
	}

	foreach x in 1 2{
	egen pob_ess2`y'_`x'_all=total(poblacion_agregada) if essential2`y'==`x'
	label var pob_ess2`y'_`x'_all "población agregada essential2 `y' category `x'"
	} 
}

tempfile poblaciones

gen casos=1
collapse (mean) pob_*, by(casos)
sa `poblaciones'
sum pob_*

restore



*******************************************************************************
* merge and collapse daily

merge m:1 casos using `poblaciones'
	
foreach y in fel{

			
	foreach x in 1 2 3{
		replace pob_ess`y'_`x'_all=. if casos_ess`y'_`x'_all==.
	}			
}	
	
collapse (mean) casos_* pob_*, by(dateoftime)

*collapse (sum) casos_*


* MOVING STOCK over populaiton

foreach y in fel {
	foreach x in 1 2{
		gen mo17_ess2_pob`y'_`x' = mo17_ess2`y'_`x'/pob_ess2`y'_`x'_all
	}
			
	foreach x in 1 2 3{
		gen mo17_ess_pob`y'_`x' = mo17_ess`y'_`x'/pob_ess`y'_`x'_all
	}			
}

* Graphs of positivity rate.
tsset  dateoftime 

foreach y in fel {


			
		twoway 	(tsline mo17_ess_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17_ess_pob`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo17_ess_pob`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day sum over pop") ylab(0(0.005)0.025) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v3/sdsmo17_ess`y'.pdf", replace	
				
				
				
	
	}



	
	
	
	
	
	

use "${data_sds}/casos_SDS_poblaciones.dta", clear

keep fecha_consulta ocupacion_agregada casos ocupacion_agregada poblacion_agregada estratosocioeconomico  poblacion_estrato 

generate dateoftime = dofc(fecha_consulta)

*Format dates

format dateoftime %td

gen stratum=estratosocioeconomico
destring stratum, replace force
replace stratum=5 if stratum==6 // putting 5 and 6 together


gen pob_stratum=poblacion_estrato
replace pob_stratum=174889+240570 if stratum==5


* create populaiton measures

forvalues x=1(1)5{

	gen pob_str_`x'=pob_stratum if stratum==`x'
}


/* Create the number of cases per stratum
forvalues x=1(1)5{
	bys fecha_consulta: egen casos_strat_`x'=total(casos) if stratum==`x'
	gen
}
*/
drop if stratum==.
bys dateoftime stratum: egen casos_strattot=total(casos) 
	
forvalues x=1(1)5{
	gen casos_strat_`x'=casos_strattot if stratum==`x'
}


* Collapse by stratum
unique casos_strat_1
tab casos_strat_1
collapse (mean) casos_strat_* pob_str_*, by (dateoftime)
unique casos_strat_1
tab casos_strat_1


* create a 17/30 day moving daily average by stratum

forvalues x=1(1)5{
rangestat (mean) mo30d_str_`x'=casos_strat_`x', interval(dateoftime -30 0)

}


* create a 17/30 day moving stock by stratum 

forvalues x=1(1)5{
rangestat (sum) mo30_strat_`x'=casos_strat_`x', interval(dateoftime -30 0)

}

forvalues x=1(1)5{

gen mo30_str_pob_`x'=mo30_strat_`x'/pob_str_`x'

}



tsset  dateoftime 

* Positivity rates



twoway 	(tsline mo30_str_pob_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_str_pob_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_str_pob_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_str_pob_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_str_pob_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.02 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day sum over pop") ylab(0(0.005)0.022)

gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30pob_strat_SDS.pdf", replace

