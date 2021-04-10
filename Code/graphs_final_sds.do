

/*

Author: Camilo De Los Rios
Objective: create graphs showing the positivity rate overtime for several occupation categories and socioeconomic stratum with SDS data


*/




* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

global data_sds "${user}/covid-project/data/SDS/processed"

*Open

use "${data_sds}/casos_SDS_poblaciones.dta", clear

keep fecha_consulta ocupacion_agregada casos ocupacion_agregada poblacion_agregada estratosocioeconomico  poblacion_estrato 


*gen date

generate dateoftime = dofc(fecha_consulta)

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

	foreach x in 1 2{
	bys fecha_consulta: egen casos_ess2`y'_`x'_all=total(casos) if essential2`y'==`x'
	label var casos_ess2`y'_`x'_all "number of total daily cases essential2 `y' category `x'"
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
	foreach x in 1 2{
		replace pob_ess2`y'_`x'_all=. if casos_ess2`y'_`x'_all==.
	}
			
	foreach x in 1 2 3{
		replace pob_ess`y'_`x'_all=. if casos_ess`y'_`x'_all==.
	}			
}	
	
collapse (mean) casos_* pob_*, by(dateoftime)



* MOVING DAILY AVERAGE STOCK 

foreach y in fel{
	foreach x in 1 2{
		rangestat (mean) mo17d_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (mean) mo30d_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -30 0)
	}
			
	foreach x in 1 2 3{
		rangestat (mean) mo17d_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (mean) mo30d_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -30 0)
	}			
}



* MOVING STOCK 


foreach y in fel{
	foreach x in 1 2{
		rangestat (sum) mo17_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (sum) mo30_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -30 0)
	}
			
	foreach x in 1 2 3{
		rangestat (sum) mo17_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (sum) mo30_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -30 0)
	}			
}





* MOVING STOCK over populaiton

foreach y in fel {
	foreach x in 1 2{
		gen mo17_ess2_pob`y'_`x' = mo17_ess2`y'_`x'/pob_ess2`y'_`x'_all
		gen mo30_ess2_pob`y'_`x' = mo30_ess2`y'_`x'/pob_ess2`y'_`x'_all
	}
			
	foreach x in 1 2 3{
		gen mo17_ess_pob`y'_`x' = mo17_ess`y'_`x'/pob_ess`y'_`x'_all
		gen mo30_ess_pob`y'_`x' = mo30_ess`y'_`x'/pob_ess`y'_`x'_all
	}			
}

* Graphs of positivity rate.
tsset  dateoftime 

foreach y in fel {


		twoway 	(tsline mo17_ess2_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17_ess2_pob`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day sum over pop") ylab(0(0.005)0.01) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo17_ess2`y'.pdf", replace
			
		twoway 	(tsline mo17_ess_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17_ess_pob`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo17_ess_pob`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day sum over pop") ylab(0(0.005)0.025) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo17_ess`y'.pdf", replace	
				
				
				
	
		twoway 	(tsline mo30_ess2_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30_ess2_pob`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day sum over pop") ylab(0(0.005)0.02) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo30_ess2`y'.pdf", replace
			
		twoway 	(tsline mo30_ess_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30_ess_pob`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo30_ess_pob`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day sum over pop") ylab(0(0.005)0.035) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo30_ess`y'.pdf", replace	
	}



	
	* Graphs of average cases in per day in last 30 days
tsset  dateoftime 

foreach y in fel {


		twoway 	(tsline mo17d_ess2`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17d_ess2`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1400 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo17d_ess2`y'.pdf", replace
			
		twoway 	(tsline mo17d_ess`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17d_ess`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo17d_ess`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1400 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo17d_ess`y'.pdf", replace	
				
				
				
	
		twoway 	(tsline mo30d_ess2`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30d_ess2`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1400 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo30d_ess2`y'.pdf", replace
			
		twoway 	(tsline mo30d_ess`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30d_ess`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo30d_ess`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1400 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/sdsmo30d_ess`y'.pdf", replace	
	}





	
	
/*------------------------------------------------------------------------------

					The same but without health services workers

------------------------------------------------------------------------------*/

	
* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

global data_sds "${user}/covid-project/data/SDS/processed"

*Open

use "${data_sds}/casos_SDS_poblaciones.dta", clear
keep fecha_consulta ocupacion_agregada casos ocupacion_agregada poblacion_agregada estratosocioeconomico  poblacion_estrato 


*gen date

generate dateoftime = dofc(fecha_consulta)

*Format dates

format dateoftime %td

drop if ocupacion_agregada == "trabajadores de la salud" 

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





/*

					Now lets make the number of cases per day per category

*/

foreach y in fel{
	
	foreach x in 1 2 3{
	bys fecha_consulta: egen casos_ess`y'_`x'_all=total(casos) if essential`y'==`x'
	label var casos_ess`y'_`x'_all "number of total daily cases essential `y' category `x'"
	}

	foreach x in 1 2{
	bys fecha_consulta: egen casos_ess2`y'_`x'_all=total(casos) if essential2`y'==`x'
	label var casos_ess2`y'_`x'_all "number of total daily cases essential2 `y' category `x'"
	} 
}

*******************************************************************************


/*

Now I need a measure of the population per category of essential 
*/

preserve
	collapse (mean) poblacion_agregada, by(ocupacion_agregada)
	
	drop if ocupacion_agregada == "trabajadores de la salud" 
	
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
	foreach x in 1 2{
		replace pob_ess2`y'_`x'_all=. if casos_ess2`y'_`x'_all==.
	}
			
	foreach x in 1 2 3{
		replace pob_ess`y'_`x'_all=. if casos_ess`y'_`x'_all==.
	}			
}	
	
collapse (mean) casos_* pob_*, by(dateoftime)



* MOVING DAILY AVERAGE STOCK 

foreach y in fel{
	foreach x in 1 2{
		rangestat (mean) mo17d_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (mean) mo30d_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -30 0)
	}
			
	foreach x in 1 2 3{
		rangestat (mean) mo17d_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (mean) mo30d_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -30 0)
	}			
}





* MOVING STOCK 


foreach y in fel{
	foreach x in 1 2{
		rangestat (sum) mo17_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (sum) mo30_ess2`y'_`x' = casos_ess2`y'_`x'_all, interval(dateoftime -30 0)
	}
			
	foreach x in 1 2 3{
		rangestat (sum) mo17_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -17 0)
		rangestat (sum) mo30_ess`y'_`x' = casos_ess`y'_`x'_all, interval(dateoftime -30 0)
	}			
}





* MOVING STOCK over populaiton

foreach y in fel {
	foreach x in 1 2{
		gen mo17_ess2_pob`y'_`x' = mo17_ess2`y'_`x'/pob_ess2`y'_`x'_all
		gen mo30_ess2_pob`y'_`x' = mo30_ess2`y'_`x'/pob_ess2`y'_`x'_all
	}
			
	foreach x in 1 2 3{
		gen mo17_ess_pob`y'_`x' = mo17_ess`y'_`x'/pob_ess`y'_`x'_all
		gen mo30_ess_pob`y'_`x' = mo30_ess`y'_`x'/pob_ess`y'_`x'_all
	}			
}

* Graphs of positivity rate.
tsset  dateoftime 

foreach y in fel {


		twoway 	(tsline mo17_ess2_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17_ess2_pob`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day sum over pop") ylab(0(0.005)0.010) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo17_ess2`y'.pdf", replace
			
		twoway 	(tsline mo17_ess_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17_ess_pob`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo17_ess_pob`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day sum over pop") ylab(0(0.005)0.025) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo17_ess`y'.pdf", replace	
				
				
				
	
		twoway 	(tsline mo30_ess2_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30_ess2_pob`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day sum over pop") ylab(0(0.005)0.035) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo30_ess2`y'.pdf", replace
			
		twoway 	(tsline mo30_ess_pob`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30_ess_pob`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo30_ess_pob`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.015 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day sum over pop") ylab(0(0.005)0.020) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo30_ess`y'.pdf", replace	
	}



	
	* Graphs of average cases in per day in last 30 days
tsset  dateoftime 


foreach y in fel {


		twoway 	(tsline mo17d_ess2`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17d_ess2`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo17d_ess2`y'.pdf", replace
			
		twoway 	(tsline mo17d_ess`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo17d_ess`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo17d_ess`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("17 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo17d_ess`y'.pdf", replace	
				
				
				
	
		twoway 	(tsline mo30d_ess2`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30d_ess2`y'_2, lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" 3 "All") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo30d_ess2`y'.pdf", replace
			
		twoway 	(tsline mo30d_ess`y'_1, lcolor(black) lpattern(dash)) ///
				(tsline mo30d_ess`y'_2, lcolor(gs10) lpattern(solid)) ///
				(tsline mo30d_ess`y'_3, lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day daily average") ylab(0(250)1500) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhsdsmo30d_ess`y'.pdf", replace	
	}


	
	
	
	
	
	

/*------------------------------------------------------------------------------

					SOCIOECONOMIC STRATUM

------------------------------------------------------------------------------*/


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
rangestat (mean) mo17d_str_`x'=casos_strat_`x', interval(dateoftime -17 0)
rangestat (mean) mo30d_str_`x'=casos_strat_`x', interval(dateoftime -30 0)

}


* create a 17/30 day moving stock by stratum 

forvalues x=1(1)5{
rangestat (sum) mo17_strat_`x'=casos_strat_`x', interval(dateoftime -17 0)
rangestat (sum) mo30_strat_`x'=casos_strat_`x', interval(dateoftime -30 0)

}

forvalues x=1(1)5{

gen mo17_str_pob_`x'=mo17_strat_`x'/pob_str_`x'
gen mo30_str_pob_`x'=mo30_strat_`x'/pob_str_`x'

}



tsset  dateoftime 

* Positivity rates


twoway 	(tsline mo17_str_pob_1, lcolor(black) lpattern(solid)) ///
		(tsline mo17_str_pob_2, lcolor(black) lpattern(dash)) ///
		(tsline mo17_str_pob_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo17_str_pob_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo17_str_pob_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.01 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("17 day sum over pop") ylab(0(0.005)0.011)
		
gr export "${user}/Apps/Overleaf/COVIDA/v2/mo17pob_strat_SDS.pdf", replace

	

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



* Daily Average


twoway 	(tsline mo17d_str_1, lcolor(black) lpattern(solid)) ///
		(tsline mo17d_str_2, lcolor(black) lpattern(dash)) ///
		(tsline mo17d_str_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo17d_str_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo17d_str_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("17 day daily average") ylab(0(250)1500)
		
gr export "${user}/Apps/Overleaf/COVIDA/v2/mo17d_strat_SDS.pdf", replace

	

twoway 	(tsline mo30d_str_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30d_str_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30d_str_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30d_str_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30d_str_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day daily average") ylab(0(250)1500)

gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30d_strat_SDS.pdf", replace



/*------------------------------------------------------------------------------
					
				The same but without health services workers

------------------------------------------------------------------------------*/



use "${data_sds}/casos_SDS_poblaciones.dta", clear

keep fecha_consulta ocupacion_agregada casos ocupacion_agregada poblacion_agregada estratosocioeconomico  poblacion_estrato 

generate dateoftime = dofc(fecha_consulta)
drop if ocupacion_agregada == "trabajadores de la salud" 


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
rangestat (mean) mo17d_str_`x'=casos_strat_`x', interval(dateoftime -17 0)
rangestat (mean) mo30d_str_`x'=casos_strat_`x', interval(dateoftime -30 0)

}


* create a 17/30 day moving stock by stratum 

forvalues x=1(1)5{
rangestat (sum) mo17_strat_`x'=casos_strat_`x', interval(dateoftime -17 0)
rangestat (sum) mo30_strat_`x'=casos_strat_`x', interval(dateoftime -30 0)

}

forvalues x=1(1)5{

gen mo17_str_pob_`x'=mo17_strat_`x'/pob_str_`x'
gen mo30_str_pob_`x'=mo30_strat_`x'/pob_str_`x'

}



tsset  dateoftime 

* Positivity rates


twoway 	(tsline mo17_str_pob_1, lcolor(black) lpattern(solid)) ///
		(tsline mo17_str_pob_2, lcolor(black) lpattern(dash)) ///
		(tsline mo17_str_pob_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo17_str_pob_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo17_str_pob_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.01 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("17 day sum over pop") ylab(0(0.005)0.011)
		
gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo17pob_strat_SDS.pdf", replace

	

twoway 	(tsline mo30_str_pob_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_str_pob_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_str_pob_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_str_pob_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_str_pob_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.02 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day sum over pop") ylab(0(0.005)0.022)

gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo30pob_strat_SDS.pdf", replace



* Daily Average


twoway 	(tsline mo17d_str_1, lcolor(black) lpattern(solid)) ///
		(tsline mo17d_str_2, lcolor(black) lpattern(dash)) ///
		(tsline mo17d_str_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo17d_str_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo17d_str_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("17 day daily average") ylab(0(250)1500)
		
gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo17d_strat_SDS.pdf", replace

	

twoway 	(tsline mo30d_str_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30d_str_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30d_str_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30d_str_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30d_str_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(1450 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day daily average") ylab(0(250)1500)

gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo30d_strat_SDS.pdf", replace





/*------------------------------------------------------------------------------
			PARA EL TAMAÑO DEL ICEBERG Y LA TASA DE DETECCION.....
------------------------------------------------------------------------------*/



/*---------------------------
			ESTRATO
-----------------------------*/

* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

global data_sds "${user}/covid-project/data/SDS/processed"

*Open

use "${data_sds}/casos_SDS_poblaciones.dta", clear
*use "C:\Users\cdelo\Desktop\sds.dta", clear

*gen date

generate dateoftime = dofc(fecha_consulta)

* lag 5 days
replace dateoftime=dateoftime-5

*Format dates

format dateoftime %td


keep  estratosocioeconomico casos dateoftime

gen stratum=estratosocioeconomico
drop if stratum=="SD"
destring stratum, replace
replace stratum=5 if stratum==6 // putting 5 and 6 together


gen mes=month(dateoftime)
drop if stratum==.
bys mes stratum: egen casos_strattot=total(casos) // total number of cases by month and stratum 
	
forvalues x=1(1)5{
	gen casos_strat_`x'=casos_strattot if stratum==`x'
}


* Collapse by stratum

collapse (mean) casos_strat_* , by (mes)
*export excel using "C:\Users\cdelo\Dropbox\COVIDA\sds_estrato_iceberg.xls", firstrow(variables)
export excel using "C:\Users\cdelo\Dropbox\COVIDA\sds_estrato_iceberg2.xls", firstrow(variables)



/*---------------------------
			without HEALTH WORKERS
-----------------------------*/



use "${data_sds}/casos_SDS_poblaciones.dta", clear
*use "C:\Users\cdelo\Desktop\sds.dta", clear

drop if ocupacion_agregada == "trabajadores de la salud" 

*gen date

generate dateoftime = dofc(fecha_consulta)

*Format dates

format dateoftime %td


keep  estratosocioeconomico casos dateoftime

gen stratum=estratosocioeconomico
drop if stratum=="SD"
destring stratum, replace
replace stratum=5 if stratum==6 // putting 5 and 6 together


gen mes=month(dateoftime)
drop if stratum==.
bys mes stratum: egen casos_strattot=total(casos) // total number of cases by month and stratum 
	
forvalues x=1(1)5{
	gen casos_strat_`x'=casos_strattot if stratum==`x'
}


* Collapse by stratum

collapse (mean) casos_strat_* , by (mes)
*export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhsds_estrato_iceberg.xls", firstrow(variables)
export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhsds_estrato_iceberg2.xls", firstrow(variables)





/*---------------------------
			Ocupaciones. 
-----------------------------*/

* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

global data_sds "${user}/covid-project/data/SDS/processed"

*Open

use "${data_sds}/casos_SDS_poblaciones.dta", clear
*use "C:\Users\cdelo\Desktop\sds.dta", clear

*gen date

generate dateoftime = dofc(fecha_consulta)

* lag 5 days
replace dateoftime=dateoftime-5

*Format dates

format dateoftime %td


keep  ocupacion_agregada casos dateoftime



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

gen mes=month(dateoftime)
drop if ocupacion_agregada==""
bys mes essentialfel: egen casos_esstot=total(casos) // total number of cases by month and stratum 
bys mes essential2fel: egen casos_ess2tot=total(casos) // total number of cases by month and stratum 

	
foreach y in fel{
	
	foreach x in 1 2 3{
	
	gen casos_ess`y'_`x'=casos_esstot if essential`y'==`x'
	
	
	}

	foreach x in 1 2{
	
	gen casos_ess2`y'_`x'=casos_ess2tot if essential2`y'==`x'

	} 
}




* Collapse by stratum

collapse (mean) casos_ess2* casos_essf* , by (mes)
*export excel using "C:\Users\cdelo\Dropbox\COVIDA\sds_essent_iceberg.xls", firstrow(variables)
export excel using "C:\Users\cdelo\Dropbox\COVIDA\sds_essent_iceberg2.xls", firstrow(variables)

/*---------------------------
			without HEALTH WORKERS
-----------------------------*/



use "${data_sds}/casos_SDS_poblaciones.dta", clear
*use "C:\Users\cdelo\Desktop\sds.dta", clear

drop if ocupacion_agregada == "trabajadores de la salud" 


*gen date

generate dateoftime = dofc(fecha_consulta)

*Format dates

format dateoftime %td


keep  ocupacion_agregada casos dateoftime



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

gen mes=month(dateoftime)
drop if ocupacion_agregada==""
bys mes essentialfel: egen casos_esstot=total(casos) // total number of cases by month and stratum 
bys mes essential2fel: egen casos_ess2tot=total(casos) // total number of cases by month and stratum 

	
foreach y in fel{
	
	foreach x in 1 2 3{
	
	gen casos_ess`y'_`x'=casos_esstot if essential`y'==`x'
	
	
	}

	foreach x in 1 2{
	
	gen casos_ess2`y'_`x'=casos_ess2tot if essential2`y'==`x'

	} 
}




* Collapse by stratum

collapse (mean) casos_ess2* casos_essf* , by (mes)
*export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhsds_essent_iceberg.xls", firstrow(variables)
export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhsds_essent_iceberg2.xls", firstrow(variables)