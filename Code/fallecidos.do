/*-----------------------------------------------------------------------------

Objective. make Death graphs

------------------------------------------------------------------------------*/


*					PATHS 


global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
global data "${user}/covid-project/data/UNIANDES/processed"
global data_sds "${user}/covid-project/data/SDS/processed"
global graphs "${user}/Iceberg Paper/views"
global data_fig "${user}/Iceberg Paper/Data"


*Open latest data base

use "${data_sds}/casos_SDS_poblaciones_23Jan2021.dta", clear
gen death=1 if recuperado=="Fallecido"
replace death=0 if recuperado=="Recuperado"
drop if death==.

sa "${graphs}/Figura2/deaths.dta", replace
use "${graphs}/Figura2/deaths.dta", clear


generate dateoftime = date(fechademuerte,"DMY")

*Format dates

format dateoftime %td

gen mes=month(dateoftime)
gen year=year(dateoftime)
generate date_m = mofd(dateoftime)

* gen stratum categories

gen stratum=estratosocioeconomico
drop if stratum=="SD"
destring stratum, replace
drop if stratum==.


 tab stratum if mes==11
 
 
 drop if stratum==.

* simply collapse now

collapse (sum) death, by(stratum date_m)
 
 
 * Population var
 
  gen pob=735748  if stratum==1
 replace pob=3327722   if stratum==2
 replace pob=2857861   if stratum==3
 replace pob=757923   if stratum==4
 replace pob=240570   if stratum==5
 replace pob=124889   if stratum==6


 
 gen percentage_100=(death*100000)/pob
format date_m %tm
 tsset stratum date_m
 
twoway (tsline percentage_100 if stratum==1, lcolor(black) lpattern(solid)) ///
(tsline percentage_100 if stratum==2, lcolor(black) lpattern(dash)) ///
(tsline percentage_100 if stratum==3, lcolor(black) lpattern(shortdash_dot)) ///
(tsline percentage_100 if stratum==4, lcolor(gs10) lpattern(solid)) ///
(tsline percentage_100 if stratum==5, lcolor(gs10) lpattern(dash)) ///
(tsline percentage_100 if stratum==6, lcolor(gs10) lpattern(shortdash_dot)), ///
legend(order (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6") pos(6) row(1) ring(1)) ///
scheme(plotplainblind) ///
xti("Deaths by Stratum with SDS data") yti("Cases per 100K inhabitants") 

gr export "${graphs}/Figura2/deaths_100k.pdf", replace
