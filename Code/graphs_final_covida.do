

/*

Author: Camilo De Los Rios
Objective: create graphs showing the positivity rate overtime for several occupation categories and socioeconomic stratum with COVIDA data


*/







* PATHS
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC

global user "C:/Users/cdelo/Dropbox" // for Camilo PC

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

*FIRST: for all; regardless of whether they presented symptoms or had COVID contact. 


foreach y in fel{
	
	foreach x in 1 2 3{
	bys test_day: asgen ratew_ess`y'_`x'_all=positive if essential`y'==`x', w(weight_ocup)
	label var ratew_ess`y'_`x'_all "weighted positive rate essential `y' category `x'"
	}

	foreach x in 1 2{
	bys test_day: asgen ratew_ess2`y'_`x'_all=positive if essential2`y'==`x', w(weight_ocup)
	label var ratew_ess2`y'_`x'_all "weighted positive rate essential2 `y' category `x'"
	} 
}

*Now taking only those who did not have symtoms, contacts, covid contacts. 

gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

foreach y in fel {

	foreach x in 1 2 3{
			bys test_day: asgen ratew_ess`y'_`x'_excl=positive if essential`y'==`x' & exclude==0, w(weight_ocup)
			label var ratew_ess`y'_`x'_excl "weighted positive rate essential `y' category `x' and no symptoms etc"
		}

	foreach x in 1 2{
			bys test_day: asgen ratew_ess2`y'_`x'_excl=positive if essential2`y'==`x' & exclude==0, w(weight_ocup)
			label var ratew_ess2`y'_`x'_excl "weighted positive rate essential2 `y' category `x' and no symptoms etc"
		} 
} 




/*--------------------

			FOR THE NON WEIGHTED POSITIVITY RATES

------------------*/

*FIRST: for all; regardless of whether they presented symptoms or had COVID contact. 


foreach y in  fel{
	
	foreach x in 1 2 3{
	bys test_day: egen ratenw_ess`y'_`x'_all=mean(positive) if essential`y'==`x'
	label var ratenw_ess`y'_`x'_all "non-weighted positive rate essential `y' category `x'"
	}

	foreach x in 1 2{
	bys test_day: egen ratenw_ess2`y'_`x'_all=mean(positive) if essential2`y'==`x'
	label var ratenw_ess2`y'_`x'_all "non-weighted positive rate essential2 `y' category `x'"
	} 
}

*Now taking only those who did not have symtoms, contacts, covid contacts. 

foreach y in fel {

	foreach x in 1 2 3{
			bys test_day: egen ratenw_ess`y'_`x'_excl=mean(positive) if essential`y'==`x' & exclude==0
			label var ratenw_ess`y'_`x'_excl "non-weighted positive rate essential `y' category `x' and no symptoms etc"
		}

	foreach x in 1 2{
			bys test_day: egen ratenw_ess2`y'_`x'_excl=mean(positive) if essential2`y'==`x' & exclude==0
			label var ratenw_ess2`y'_`x'_excl "non-weighted positive rate essential2 `y' category `x' and no symptoms etc"
		} 
} 



/*
		Now simply collapse

*/



collapse (mean) ratenw_* ratew_*, by(test_day)


* Create the 30 day moving avergae for all the rates



foreach w in w nw{
	foreach y in fel {
		foreach z in all excl{

			foreach x in 1 2{
				rangestat (mean) mo30_rate`w'_ess2`y'_`x'_`z' = rate`w'_ess2`y'_`x'_`z', interval(test_day -30 0)
			}
			
			foreach x in 1 2 3{
				rangestat (mean) mo30_rate`w'_ess`y'_`x'_`z' = rate`w'_ess`y'_`x'_`z', interval(test_day -30 0)
			}			
		}
	}
}




* Graph the first  

tsset test_day

foreach w in w nw{
	foreach y in fel {
		foreach z in all excl{

		twoway 	(tsline mo30_rate`w'_ess2`y'_1_`z', lcolor(black) lpattern(dash)) ///
				(tsline mo30_rate`w'_ess2`y'_2_`z', lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" ) pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day MA") ylab(0(0.05)0.15) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30`w'_ess2`y'_`z'.pdf", replace
				
		twoway 	(tsline mo30_rate`w'_ess`y'_1_`z', lcolor(black) lpattern(dash)) ///
				(tsline mo30_rate`w'_ess`y'_2_`z', lcolor(gs10) lpattern(solid)) ///
				(tsline mo30_rate`w'_ess`y'_3_`z', lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day MA") ylab(0(0.05)0.15) 
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30`w'_ess`y'_`z'.pdf", replace	
	
		}
	}
}




/*-------------------------------------------------------------------------

				Now some of those but excluding health-care workers.

-----------------------------------------------------------------------*/



use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

* Generate Categories of essential workers. 
/*
				According to felipe. 
*/
gen essentialfel = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "cuidador de niños" | ocup_cat == "guardias de seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" |  ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat == "taxistas" 

replace essentialfel = 2 if ocup_cat =="artistas y actividades culturales" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines"   | ocup_cat == "servicios apoyo produccion" | ocup_cat == "personal de restaurantes"  | ocup_cat == "periodistas y escritores" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "personal limpieza" | ocup_cat == "vendedor tienda"  

replace essentialfel = 3 if ocup_cat == "abogados" |  ocup_cat == "ama de casa" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" |  ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores"  | ocup_cat =="vendedor ambulante" 

recode essentialfel(.=0) // they simply have no occupation. 
label define esencia 1 "essential" 2 "moderate essential" 3 "no essential" 0 "NA"
label values essentialfel esencia


* A somwhoe more aggregate measure of essential workers

gen essential2fel=1 if essentialfel==1 | essentialfel==2
replace essential2fel=2 if essentialfel==3
recode essential2fel(.=0)
label values essential2fel esencia2 

 

/*

Now, we will create daily weighted positive rates by category subgorups

Notice: we could simple make something like: 

collapse (mean) positive [aweight=weight_ocup], by(test_day)

But then we would have to make that process as many times as we want different categories


*/

/*--------------------

			FOR THE WEIGHTED POSITIVITY RATES

------------------*/

*FIRST: for all; regardless of whether they presented symptoms or had COVID contact. 


foreach y in fel{
	
	foreach x in 1 2 3{
	bys test_day: asgen ratew_ess`y'_`x'_all=positive if essential`y'==`x', w(weight_ocup)
	label var ratew_ess`y'_`x'_all "weighted positive rate essential `y' category `x'"
	}

	foreach x in 1 2{
	bys test_day: asgen ratew_ess2`y'_`x'_all=positive if essential2`y'==`x', w(weight_ocup)
	label var ratew_ess2`y'_`x'_all "weighted positive rate essential2 `y' category `x'"
	} 
}

*Now taking only those who did not have symtoms, contacts, covid contacts. 

gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

foreach y in fel {

	foreach x in 1 2 3{
			bys test_day: asgen ratew_ess`y'_`x'_excl=positive if essential`y'==`x' & exclude==0, w(weight_ocup)
			label var ratew_ess`y'_`x'_excl "weighted positive rate essential `y' category `x' and no symptoms etc"
		}

	foreach x in 1 2{
			bys test_day: asgen ratew_ess2`y'_`x'_excl=positive if essential2`y'==`x' & exclude==0, w(weight_ocup)
			label var ratew_ess2`y'_`x'_excl "weighted positive rate essential2 `y' category `x' and no symptoms etc"
		} 
} 




/*--------------------

			FOR THE NON WEIGHTED POSITIVITY RATES

------------------*/

*FIRST: for all; regardless of whether they presented symptoms or had COVID contact. 


foreach y in fel{
	
	foreach x in 1 2 3{
	bys test_day: egen ratenw_ess`y'_`x'_all=mean(positive) if essential`y'==`x'
	label var ratenw_ess`y'_`x'_all "non-weighted positive rate essential `y' category `x'"
	}

	foreach x in 1 2{
	bys test_day: egen ratenw_ess2`y'_`x'_all=mean(positive) if essential2`y'==`x'
	label var ratenw_ess2`y'_`x'_all "non-weighted positive rate essential2 `y' category `x'"
	} 
}

*Now taking only those who did not have symtoms, contacts, covid contacts. 

foreach y in fel {

	foreach x in 1 2 3{
			bys test_day: egen ratenw_ess`y'_`x'_excl=mean(positive) if essential`y'==`x' & exclude==0
			label var ratenw_ess`y'_`x'_excl "non-weighted positive rate essential `y' category `x' and no symptoms etc"
		}

	foreach x in 1 2{
			bys test_day: egen ratenw_ess2`y'_`x'_excl=mean(positive) if essential2`y'==`x' & exclude==0
			label var ratenw_ess2`y'_`x'_excl "non-weighted positive rate essential2 `y' category `x' and no symptoms etc"
		} 
} 





/*
		Now simply collapse

*/



collapse (mean) ratenw_* ratew_*, by(test_day)


* Create the 30 day moving avergae for all the rates




foreach w in w nw{
	foreach y in fel {
		foreach z in all excl{

			foreach x in 1 2{
				rangestat (mean) mo30_rate`w'_ess2`y'_`x'_`z' = rate`w'_ess2`y'_`x'_`z', interval(test_day -30 0)
			}
			
			foreach x in 1 2 3{
				rangestat (mean) mo30_rate`w'_ess`y'_`x'_`z' = rate`w'_ess`y'_`x'_`z', interval(test_day -30 0)
			}			
		}
	}
}




* Graph the first  

tsset test_day

foreach w in w nw{
	foreach y in fel {
		foreach z in all excl{

		twoway 	(tsline mo30_rate`w'_ess2`y'_1_`z', lcolor(black) lpattern(dash)) ///
				(tsline mo30_rate`w'_ess2`y'_2_`z', lcolor(black) lpattern(solid)) , ///
				legend(order (1 "Essential" 2 "Non-Essential" ) pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day MA") ylab(0(0.05)0.15) note("Health workers excluded", size(vsmall))
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo30`w'_ess2`y'_`z'.pdf", replace
				
		twoway 	(tsline mo30_rate`w'_ess`y'_1_`z', lcolor(black) lpattern(dash)) ///
				(tsline mo30_rate`w'_ess`y'_2_`z', lcolor(gs10) lpattern(solid)) ///
				(tsline mo30_rate`w'_ess`y'_3_`z', lcolor(black) lpattern(solid)), ///
				legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
				scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
				text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
				xti("") yti("30 day MA") ylab(0(0.05)0.15) note("Health workers excluded", size(vsmall))
				
				gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo30`w'_ess`y'_`z'.pdf", replace	
	
		}
	}
}




/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
				
				NOW FOR DIFFERENT  OCCUPATIONS
				
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/

use "${data}/Datos_Salesforce_treated.dta", clear

* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

*drop those with less than 500 observations in total
gen conteo=1
bys ocup_cat: egen num_cat_tot=count(conteo)
keep if num_cat_tot>499
drop if test_day==.
drop if ocup_cat==""

* now generate a counting variable that calculates the number of total cases used in the movin average. 

collapse (sum) conteo, by(ocup_cat test_day)
gen casos_accum_30=.
levelsof ocup_cat, local(categ2)

foreach x in `categ2'{

unique test_day if ocup_cat=="`x'"
local tope=r(unique)



	forvalues n = 1(1)`tope' {
		cap drop i`n'
		gen i`n' = 1 if  test_day[`n']>=test_day  & (test_day[`n']-test_day) <= 30 & ocup_cat=="`x'"
		egen casos`n' = total(conteo) if i`n'==1 & ocup_cat=="`x'"
		replace casos_accum_30 = casos`n' if mi(casos_accum_30) & ocup_cat=="`x'"
		drop  i`n'
		drop casos`n'
	}


}


gen obs50=1 if casos_accum_30>=50
recode obs50(.=0)
gen obs100=1 if casos_accum_30>=100
recode obs100(.=0)

keep obs100 obs50 test_day ocup_cat
format test_day %td

gen cat_new=substr(ocup_cat,1,3)
replace cat_new="prod" if ocup_cat=="servicios apoyo produccion"
replace cat_new="fin" if ocup_cat=="personal servicios financieros"
replace cat_new="trap" if ocup_cat=="personal transporte"
replace cat_new="secr" if ocup_cat=="personal secretaria"
replace cat_new="amb" if ocup_cat=="vendedor ambulante"
replace cat_new="tien" if ocup_cat=="vendedor tienda"
replace cat_new="limp" if ocup_cat=="personal limpieza"

keep obs100 obs50 test_day cat_new



sa "C:\Users\cdelo\Dropbox\COVIDA\obs30100.dta", replace



use "${data}/Datos_Salesforce_treated.dta", clear

* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

*drop those with less than 500 observations in total
gen conteo=1
bys ocup_cat: egen num_cat_tot=count(conteo)
keep if num_cat_tot>499
drop if test_day==.
drop if ocup_cat==""




gen cat_new=substr(ocup_cat,1,3)
replace cat_new="prod" if ocup_cat=="servicios apoyo produccion"
replace cat_new="fin" if ocup_cat=="personal servicios financieros"
replace cat_new="trap" if ocup_cat=="personal transporte"
replace cat_new="secr" if ocup_cat=="personal secretaria"
replace cat_new="amb" if ocup_cat=="vendedor ambulante"
replace cat_new="tien" if ocup_cat=="vendedor tienda"
replace cat_new="limp" if ocup_cat=="personal limpieza"


format test_day %td


bys test_day cat_new: asgen ratew=positive, w(weight_ocup)
bys test_day cat_new: egen ratenw=mean(positive) 


collapse (mean) ratew ratenw, by(test_day cat_new)

merge 1:1 test_day cat_new using "C:\Users\cdelo\Dropbox\COVIDA\obs30100.dta"




foreach y in w nw{
	
		rangestat (mean) mo30_rate`y'_cat1 = rate`y', interval(test_day -30 0) by(cat_new)
		gen  mo30_rate`y'_cat2= mo30_rate`y'_cat1
		replace mo30_rate`y'_cat2=. if obs50==0
		replace mo30_rate`y'_cat1=. if obs100==0

		
	}

	
encode cat_new, gen(categoria)

tsset categoria test_day 


foreach x in 1 2{

twoway 	(tsline mo30_ratew_cat`x' if cat_new=="abo", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="ama", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="amb", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="art", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="bio", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="cui", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Lawyers" 2 "House" 3 "Ambulantes" 4 "Artist" 5 "Biologist" 6 "Child care taker") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.09 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.10) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))
	
	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30w_cat1`x'.pdf", replace
	
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="des", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="dir", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="est", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="fin", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="gua", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="ing", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Unemployed" 2 "Directors" 3 "Students" 4 "Financial" 5 "Security" 6 "Engineers") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.145 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30w_cat2`x'.pdf", replace

	
	
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="men", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="mil", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="obr", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="pen", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="per", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="pro", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Delivery" 2 "Military" 3 "Construction" 4 "Retired" 5 "Journalist" 6 "Teachers") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.19 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30w_cat3`x'.pdf", replace
	
		
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="prod", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="psi", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="secr", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="tax", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="tien", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="tra", lcolor(sky) lpattern(dot)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="trap", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Producers" 2 "Psicology" 3 "Secretaries" 4 "Taxi drivers" 5 "Tiendas" 6 "Health Care" 7 "Transport") pos(6) row(2)  ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.145 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30w_cat_4`x'.pdf", replace
	
	
}
	
	
	
	*******NOW 60 DAYS AVERAGE INSTEAD OF 30********
	
	
use "${data}/Datos_Salesforce_treated.dta", clear

* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

*drop those with less than 500 observations in total
gen conteo=1
bys ocup_cat: egen num_cat_tot=count(conteo)
keep if num_cat_tot>499
drop if test_day==.
drop if ocup_cat==""

* now generate a counting variable that calculates the number of total cases used in the movin average. 

collapse (sum) conteo, by(ocup_cat test_day)
gen casos_accum_60=.
levelsof ocup_cat, local(categ2)

foreach x in `categ2'{

unique test_day if ocup_cat=="`x'"
local tope=r(unique)



	forvalues n = 1(1)`tope' {
		cap drop i`n'
		gen i`n' = 1 if  test_day[`n']>=test_day  & (test_day[`n']-test_day) <= 60 & ocup_cat=="`x'"
		egen casos`n' = total(conteo) if i`n'==1 & ocup_cat=="`x'"
		replace casos_accum_60 = casos`n' if mi(casos_accum_60) & ocup_cat=="`x'"
		drop  i`n'
		drop casos`n'
	}


}


gen obs50=1 if casos_accum_60>=50
recode obs50(.=0)
gen obs100=1 if casos_accum_60>=100
recode obs100(.=0)

keep obs100 obs50 test_day ocup_cat
format test_day %td

gen cat_new=substr(ocup_cat,1,3)
replace cat_new="prod" if ocup_cat=="servicios apoyo produccion"
replace cat_new="fin" if ocup_cat=="personal servicios financieros"
replace cat_new="trap" if ocup_cat=="personal transporte"
replace cat_new="secr" if ocup_cat=="personal secretaria"
replace cat_new="amb" if ocup_cat=="vendedor ambulante"
replace cat_new="tien" if ocup_cat=="vendedor tienda"
replace cat_new="limp" if ocup_cat=="personal limpieza"

keep obs100 obs50 test_day cat_new



sa "C:\Users\cdelo\Dropbox\COVIDA\obs30100.dta", replace



use "${data}/Datos_Salesforce_treated.dta", clear

* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

*drop those with less than 500 observations in total
gen conteo=1
bys ocup_cat: egen num_cat_tot=count(conteo)
keep if num_cat_tot>499
drop if test_day==.
drop if ocup_cat==""




gen cat_new=substr(ocup_cat,1,3)
replace cat_new="prod" if ocup_cat=="servicios apoyo produccion"
replace cat_new="fin" if ocup_cat=="personal servicios financieros"
replace cat_new="trap" if ocup_cat=="personal transporte"
replace cat_new="secr" if ocup_cat=="personal secretaria"
replace cat_new="amb" if ocup_cat=="vendedor ambulante"
replace cat_new="tien" if ocup_cat=="vendedor tienda"
replace cat_new="limp" if ocup_cat=="personal limpieza"


format test_day %td


bys test_day cat_new: asgen ratew=positive, w(weight_ocup)
bys test_day cat_new: egen ratenw=mean(positive) 


collapse (mean) ratew ratenw, by(test_day cat_new)

merge 1:1 test_day cat_new using "C:\Users\cdelo\Dropbox\COVIDA\obs30100.dta"




foreach y in w nw{
	
		rangestat (mean) mo30_rate`y'_cat1 = rate`y', interval(test_day -60 0) by(cat_new)
		gen  mo30_rate`y'_cat2= mo30_rate`y'_cat1
		replace mo30_rate`y'_cat2=. if obs50==0
		replace mo30_rate`y'_cat1=. if obs100==0

		
	}

	
encode cat_new, gen(categoria)

tsset categoria test_day 


foreach x in 1 2{

twoway 	(tsline mo30_ratew_cat`x' if cat_new=="abo", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="ama", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="amb", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="art", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="bio", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="cui", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Lawyers" 2 "House" 3 "Ambulantes" 4 "Artist" 5 "Biologist" 6 "Child care taker") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.09 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("60 day MA") ylab(0(0.05)0.10) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))
	
	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo60w_cat1`x'.pdf", replace
	
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="des", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="dir", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="est", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="fin", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="gua", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="ing", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Unemployed" 2 "Directors" 3 "Students" 4 "Financial" 5 "Security" 6 "Engineers") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.145 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("60 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo60w_cat2`x'.pdf", replace

	
	
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="men", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="mil", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="obr", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="pen", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="per", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="pro", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Delivery" 2 "Military" 3 "Construction" 4 "Retired" 5 "Journalist" 6 "Teachers") pos(6) row(2) col(3) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.195 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("60 day MA") ylab(0(0.05)0.20) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo60w_cat3`x'.pdf", replace
	
		
twoway 	(tsline mo30_ratew_cat`x' if cat_new=="prod", lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="psi", lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="secr", lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="tax", lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_cat`x' if cat_new=="tien", lcolor(sky) lpattern(dash)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="tra", lcolor(sky) lpattern(dot)) ///
		(tsline mo30_ratew_cat`x' if cat_new=="trap", lcolor(gs10) lpattern(dash)), ///
		legend(order (1 "Producers" 2 "Psicology" 3 "Secretaries" 4 "Taxi drivers" 5 "Tiendas" 6 "Health Care" 7 "Transport") pos(6) row(2)  ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.145 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("60 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation. Graphing only if obs>`x'", size(vsmall))

	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo60w_cat_4`x'.pdf", replace
	
	
}
	
	
/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
				
							FOR SOCIO-ECONOMIC STRATUM.
						without those with contacts or covid

--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
	
	
	
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
bys test_day: egen ratenw_`x'=mean(positive) if strat==`x'
}


collapse (mean) ratew_* ratenw_*, by(test_day)

foreach y in w nw{
	forvalues x=1(1)5{
		rangestat (mean) mo30_rate`y'_strat_`x' = rate`y'_`x', interval(test_day -30 0)
	}
}

tsset  test_day 

foreach x in w nw{

twoway 	(tsline mo30_rate`x'_strat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate`x'_strat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_rate`x'_strat_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.18 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20)
		
		gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30`x'_strat.pdf", replace
}


* EXCLUDING HEALTH WORKERS

use "${data}/Datos_Salesforce_treated.dta", clear
format test_day %td


* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

drop if ocup_cat=="trabajadores de la salud"

gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together

forvalues x=1(1)5{
bys test_day: asgen ratew_`x'=positive if strat==`x', w(weight_ocup)
bys test_day: egen ratenw_`x'=mean(positive) if strat==`x'
}


collapse (mean) ratew_* ratenw_*, by(test_day)

foreach y in w nw{
	forvalues x=1(1)5{
		rangestat (mean) mo30_rate`y'_strat_`x' = rate`y'_`x', interval(test_day -30 0)
	}
}

tsset  test_day 

foreach x in w nw{

twoway 	(tsline mo30_rate`x'_strat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate`x'_strat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_rate`x'_strat_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.18 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20)
		
		gr export "${user}/Apps/Overleaf/COVIDA/v2/nhmo30`x'_strat.pdf", replace
}





*******Taking out stratum with less than 50 or 100 obs.... 





use "${data}/Datos_Salesforce_treated.dta", clear
format test_day %td


* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together


gen conteo=1

* now generate a counting variable that calculates the number of total cases used in the movin average. 

collapse (sum) conteo, by(strat test_day)
gen casos_accum_30=.
levelsof strat, local(categ2)

foreach x in `categ2'{

unique test_day if strat==`x'
local tope=r(unique)



	forvalues n = 1(1)`tope' {
		cap drop i`n'
		gen i`n' = 1 if  test_day[`n']>=test_day  & (test_day[`n']-test_day) <= 30 & strat==`x'
		egen casos`n' = total(conteo) if i`n'==1 & strat==`x'
		replace casos_accum_30 = casos`n' if mi(casos_accum_30) & strat==`x'
		drop  i`n'
		drop casos`n'
	}


}


gen obs50=1 if casos_accum_30>=50
recode obs50(.=0)
gen obs100=1 if casos_accum_30>=100
recode obs100(.=0)

keep obs100 obs50 test_day strat
format test_day %td

keep obs100 obs50 test_day strat



sa "C:\Users\cdelo\Dropbox\COVIDA\strat30100.dta", replace



use "${data}/Datos_Salesforce_treated.dta", clear

* drop those with covid contact
gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 
drop if exclude==1

gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together


format test_day %td


bys test_day strat: asgen ratew=positive, w(weight_ocup)


collapse (mean) ratew , by(test_day strat)

merge 1:1 test_day strat using "C:\Users\cdelo\Dropbox\COVIDA\strat30100.dta"




foreach y in w {
	
		rangestat (mean) mo30_rate`y'_strat1 = rate`y', interval(test_day -30 0) by(strat)
		gen  mo30_rate`y'_strat2= mo30_rate`y'_strat1
		replace mo30_rate`y'_strat2=. if obs50==0
		replace mo30_rate`y'_strat1=. if obs100==0

		
	}

	

tsset strat test_day 


foreach x in 1 2{

twoway 	(tsline mo30_ratew_strat`x' if strat==1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_ratew_strat`x' if strat==2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_ratew_strat`x' if strat==3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_ratew_strat`x' if strat==4, lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_ratew_strat`x' if strat==5, lcolor(sky) lpattern(dash)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6") pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.09 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.10) ///
		note("Excluding COVID contacted and symptoms.Graphing only if obs used for calculation>`x'. Weighted by worker category", size(vsmall))
	
	gr export "${user}/Apps/Overleaf/COVIDA/v2/mo30w_strat_obs`x'.pdf", replace
	

}



/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
				
							FOR SOCIO-ECONOMIC STRATUM.

--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
	
	
	
use "${data}/Datos_Salesforce_treated.dta", clear
format test_day %td



gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together

forvalues x=1(1)5{
bys test_day: asgen ratew_`x'=positive if strat==`x', w(weight_ocup)
bys test_day: egen ratenw_`x'=mean(positive) if strat==`x'
}


collapse (mean) ratew_* ratenw_*, by(test_day)

foreach y in w nw{
	forvalues x=1(1)5{
		rangestat (mean) mo30_rate`y'_strat_`x' = rate`y'_`x', interval(test_day -30 0)
	}
}

tsset  test_day 

foreach x in w nw{

twoway 	(tsline mo30_rate`x'_strat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate`x'_strat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_rate`x'_strat_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.18 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20)
		
		gr export "${user}/Apps/Overleaf/COVIDA/v2/COVmo30`x'_strat.pdf", replace
}


* EXCLUDING HEALTH WORKERS

use "${data}/Datos_Salesforce_treated.dta", clear
format test_day %td


drop if ocup_cat=="trabajadores de la salud"

gen strat=stratum
replace strat=5 if stratum==6 // putting 5 and 6 together

forvalues x=1(1)5{
bys test_day: asgen ratew_`x'=positive if strat==`x', w(weight_ocup)
bys test_day: egen ratenw_`x'=mean(positive) if strat==`x'
}


collapse (mean) ratew_* ratenw_*, by(test_day)

foreach y in w nw{
	forvalues x=1(1)5{
		rangestat (mean) mo30_rate`y'_strat_`x' = rate`y'_`x', interval(test_day -30 0)
	}
}

tsset  test_day 

foreach x in w nw{

twoway 	(tsline mo30_rate`x'_strat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate`x'_strat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate`x'_strat_4, lcolor(gs10) lpattern(dash)) ///		
		(tsline mo30_rate`x'_strat_5, lcolor(sky) lpattern(solid)), ///
		legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5 & 6" ) pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.18 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.20)
		
		gr export "${user}/Apps/Overleaf/COVIDA/v2/COVnhmo30`x'_strat.pdf", replace
}



/*	
/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
				
				THIS IS A DIFFERENT APPROACH TO CEHCK ROBUSTNESS
				
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
* Now, lets follow a rather alternative approach to see how this changes. I am doing this as a robustness check 

use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

* Generate Categories of essential workers. 

gen essential = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "guardias seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" | ocup_cat == "periodistas y escritores" | ocup_cat == "personal limpieza" | ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" | ocup_cat == "vendedor tienda"

replace essential = 2 if ocup_cat == "ama de casa" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines" | ocup_cat == "personal de restaurantes" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "servicios apoyo produccion"

replace essential = 3 if ocup_cat == "abogados" | ocup_cat =="artistas y actividades culturales" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" | ocup_cat == "cuidador de niños"| ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat =="vendedor ambulante" 


collapse (mean) positive [aweight=weight_ocup] , by(test_day essential)

foreach x in 1 2 3{
rangestat (mean) mo30_rate_essential_aw`x'=positive if essential==`x', interval(test_day -30 0)

}

tsset essential test_day 

twoway 	(tsline mo30_rate_essential_aw1, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate_essential_aw2, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate_essential_aw3, lcolor(black) lpattern(solid)), ///
		legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) 
gr export "${user}/Apps/Overleaf/COVIDA/mo30_ess_all_rob1.pdf", replace


			* SECOND ROBUSTNESS

use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

* Generate Categories of essential workers. 

gen essential = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "guardias seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" | ocup_cat == "periodistas y escritores" | ocup_cat == "personal limpieza" | ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" | ocup_cat == "vendedor tienda"

replace essential = 2 if ocup_cat == "ama de casa" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines" | ocup_cat == "personal de restaurantes" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "servicios apoyo produccion"

replace essential = 3 if ocup_cat == "abogados" | ocup_cat =="artistas y actividades culturales" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" | ocup_cat == "cuidador de niños"| ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat =="vendedor ambulante" 


collapse (mean) positive [fweight=weight_ocup] , by(test_day essential)

foreach x in 1 2 3{
rangestat (mean) mo30_rate_essential_aw`x'=positive if essential==`x', interval(test_day -30 0)

}

tsset essential test_day 

twoway 	(tsline mo30_rate_essential_aw1, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate_essential_aw2, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate_essential_aw3, lcolor(black) lpattern(solid)), ///
		legend(order (1 "Strict-Essential" 2 "Moderate-Essential" 3 "Non-Essential") pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.12 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) 
gr export "${user}/Apps/Overleaf/COVIDA/mo30_ess_all_rob2.pdf", replace






/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
				
				NOW LET US MAKE IT BY OCCUPATIONS 
			At least a few graphs on that
				
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

local k=1
foreach x in "taxistas"  "trabajadores de la salud" "obreros de construccion" "personal secretaria" "ingeniero y servicios informaticos"{
bys test_day: asgen rate_`k'=positive if ocup_cat=="`x'", w(weight_ocup)
local k=`k'+1
}


collapse (mean) rate_*, by(test_day)


forvalues x=1(1)5{
	rangestat (mean) mo30_rate_cat_`x' = rate_`x', interval(test_day -30 0)
}

tsset  test_day 

twoway 	(tsline mo30_rate_cat_1, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate_cat_2, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate_cat_3, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate_cat_4, lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_rate_cat_5, lcolor(sky) lpattern(dash)), ///
		legend(order (1 "Taxi drivers" 2 "Healthcare" 3 "Construction" 4 "Secretaries" 5 "Engineers") pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.14 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation", size(vsmall))
	
		gr export "${user}/Apps/Overleaf/COVIDA/mo30_categories_collap.pdf", replace
	
/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
							And the robustness....				
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/




use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td

* Generate Categories of essential workers. 

gen essential = 1 if ocup_cat == "agricultores y afines"  | ocup_cat == "guardias seguridad" | ocup_cat == "mensajero" | ocup_cat == "militares y fuerza publica" | ocup_cat == "periodistas y escritores" | ocup_cat == "personal limpieza" | ocup_cat == "personal servicios financieros" | ocup_cat == "personal transporte" | ocup_cat == "taxistas" | ocup_cat == "trabajadores de la salud" | ocup_cat == "vendedor tienda"

replace essential = 2 if ocup_cat == "ama de casa" | ocup_cat == "obreros de construccion" | ocup_cat == "peluqueros y afines" | ocup_cat == "personal de restaurantes" | ocup_cat == "personal de servicio a bordo" | ocup_cat == "servicios apoyo produccion"

replace essential = 3 if ocup_cat == "abogados" | ocup_cat =="artistas y actividades culturales" | ocup_cat =="biologo y afines" | ocup_cat =="carpinteros y afines" | ocup_cat =="desempleado" | ocup_cat =="directores y gerentes de empresas" | ocup_cat =="entrenadores actividades deportivas" | ocup_cat =="estudiante" | ocup_cat =="ingeniero y servicios informaticos" | ocup_cat =="pensionado" | ocup_cat =="personal ingenieria mecanica" | ocup_cat =="personal secretaria" | ocup_cat == "cuidador de niños"| ocup_cat =="personal servicio comunitario" | ocup_cat =="profesores" | ocup_cat =="psicologos, sociologos y afines" | ocup_cat =="vendedor ambulante" 



collapse (mean) positive [fweight=weight_ocup] , by(test_day ocup_cat)

egen grupo=group(ocup_cat)
levelsof grupo, local(grupos)

foreach x in `grupos'{
rangestat (mean) mo30_rate_cat_`x'=positive if grupo==`x', interval(test_day -30 0)

}

foreach x in "taxistas"  "trabajadores de la salud" "obreros de construccion" "personal secretaria" "ingeniero y servicios informaticos"{
sum grupo if ocup_cat=="`x'"

}
tsset grupo test_day 
twoway 	(tsline mo30_rate_cat_31, lcolor(black) lpattern(solid)) ///
		(tsline mo30_rate_cat_32, lcolor(black) lpattern(dash)) ///
		(tsline mo30_rate_cat_16, lcolor(gs10) lpattern(solid)) ///
		(tsline mo30_rate_cat_24, lcolor(sky) lpattern(solid)) ///		
		(tsline mo30_rate_cat_13, lcolor(sky) lpattern(dash)), ///
		legend(order (1 "Taxi drivers" 2 "Healthcare" 3 "Construction" 4 "Secretaries" 5 "Engineers") pos(6) row(1) ring(1)) ///
		scheme(plotplainblind)	xline(22150, lpattern(dash) lwidth(thin)) ///
		text(0.14 22152 "Selective isolation", size(vsmall) place(e)) ///
		xti("") yti("30 day MA") ylab(0(0.05)0.15) ///
		note("All workers tested by category used for calculation", size(vsmall))

				
gr export "${user}/Apps/Overleaf/COVIDA/mo30_categories_collap_rob.pdf", replace



*/

/*------------------------------------------------------------------------------
			PARA EL TAMAÑO DEL ICEBERG Y LA TASA DE DETECCION.....
------------------------------------------------------------------------------*/



/*--------------------------------------------

		Estrato 

---------------------------------------------*/

use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td
drop if test_day==.
gen mes=month(test_day)


gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1


gen estrato_cam=stratum
replace estrato_cam=5 if stratum==6 // putting 5 and 6 together

drop if ocup_cat=="militares y fuerza publica" &  mes==7

forvalues x=1(1)5{
bys mes: asgen ratew_`x'=positive if estrato_cam==`x', w(weight_ocup)
}


collapse (mean) ratew_*, by(mes)



export excel using "C:\Users\cdelo\Dropbox\COVIDA\covida_estrato_iceberg.xls", firstrow(variables)


/*--------------------------------------------
		Without health workers 
---------------------------------------------*/


use "${data}/Datos_Salesforce_treated.dta", clear
drop if ocup_cat == "trabajadores de la salud"

*Format dates

format test_day %td
drop if test_day==.
gen mes=month(test_day)


gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1


gen estrato_cam=stratum
replace estrato_cam=5 if stratum==6 // putting 5 and 6 together

drop if ocup_cat=="militares y fuerza publica" &  mes==7

forvalues x=1(1)5{
bys mes: asgen ratew_`x'=positive if estrato_cam==`x', w(weight_ocup)
}


collapse (mean) ratew_*, by(mes)

export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhcovida_estrato_iceberg.xls", firstrow(variables)





/*--------------------------------------------

		Categories 

---------------------------------------------*/

use "${data}/Datos_Salesforce_treated.dta", clear

*Format dates

format test_day %td
drop if test_day==.
gen mes=month(test_day)


gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1
drop if ocup_cat=="militares y fuerza publica" &  mes==7

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

foreach y in fel{
	
	foreach x in 1 2 3{
	bys mes: asgen ratew_ess`y'_`x'=positive if essential`y'==`x', w(weight_ocup)
	}

	foreach x in 1 2{
	bys mes: asgen ratew_ess2`y'_`x'=positive if essential2`y'==`x', w(weight_ocup)
	} 
}



collapse (mean) ratew_*, by(mes)

export excel using "C:\Users\cdelo\Dropbox\COVIDA\covida_cat_iceberg.xls", firstrow(variables)






/*--------------------------------------------
		Without health workers 
---------------------------------------------*/

use "${data}/Datos_Salesforce_treated.dta", clear
drop if ocup_cat == "trabajadores de la salud"
*Format dates

format test_day %td
drop if test_day==.
gen mes=month(test_day)


gen exclude=1 if symptom==1 | contact_COVID==1 | contact==1
recode exclude(.=0)
label var exclude "those who had symptoms or contact" 

drop if exclude==1


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

foreach y in fel{
	
	foreach x in 1 2 3{
	bys mes: asgen ratew_ess`y'_`x'=positive if essential`y'==`x', w(weight_ocup)
	}

	foreach x in 1 2{
	bys mes: asgen ratew_ess2`y'_`x'=positive if essential2`y'==`x', w(weight_ocup)
	} 
}



collapse (mean) ratew_*, by(mes)

export excel using "C:\Users\cdelo\Dropbox\COVIDA\nhcovida_cat_iceberg.xls", firstrow(variables)








