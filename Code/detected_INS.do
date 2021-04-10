

/*-----------------------------------------------------------------------------

Objective. make Iceberg to create the integral

------------------------------------------------------------------------------*/


*					PATHS 
global user "C:/Users/rlaaj/Dropbox (Uniandes)/PROJECTS/COVID-19/Datos para COVID-19" // for Rachid PC
global user "C:/Users/cdelo/Dropbox" // for Camilo PC
global data "${user}/Iceberg Paper/Data"
global overleaf "${user}/Apps/Overleaf/COVID_Colombia/figures"





* use datos
import excel "${data}/INS_detected.xlsx", sheet("Hoja1") first clear

keep upper lower pib city fecha detected

twoway (rcap upper lower pib, lstyle(ci) lcolor(black)) ///
(scatter detected pib, mstyle(p1) msize(mdelarge) mlabel(city) mlabposition(2)) ///
(scatter detected pib, mstyle(p1) msize(mdelarge) mlabel(fecha) mlabposition(4) mlabsize(vsmall)), ///
scheme(plotplainblind) xti("Per capita GDP (Thousands COP)", size(small)) ///
yti("% Detected", size(small)) leg(off) xscale(range(10000 31000) noextend) ///
note("Raw seropositivity used for calculation. For Bogotá we adjusted the total number of cases with data made available to us.", size(vsmall))


gr export "${Overleaf}/Detected_INS.pdf", replace