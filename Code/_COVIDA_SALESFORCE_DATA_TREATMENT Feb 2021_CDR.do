****************************************************
/*
TRATAMIENTO DE DATOS COVIDA, 
DO FILES DE RACHID LAAJAJ CON MUCHOS PEDAZOS DE DANILO
*/

global processed C:\Users\rlaaj\Dropbox (Uniandes)\PROJECTS\COVID-19\Datos para COVID-19\covid-project\data\UNIANDES\processed
global uniandes C:\Users\rlaaj\Dropbox (Uniandes)\RepositorioUniandes\Datos Salesforce
global originals C:\Users\rlaaj\Dropbox (Uniandes)\PROJECTS\COVID-19\Datos para COVID-19\covid-project\data\UNIANDES\originals

global ocup_cat_new C:\Users\cdelo\Dropbox\Iceberg Paper\Data
global processed C:\Users\cdelo\Dropbox\covid-project\data\UNIANDES\processed
global originals C:\Users\cdelo\Dropbox\covid-project\data\UNIANDES\originals


*Ignacio
global user "~/Dropbox/Research/Covid_los_andes" // for Ignacio Mac
global processed "${user}/covid-project/data/UNIANDES/processed"
global originals "${user}/covid-project/data/UNIANDES/originals"

*global base C:\Users\rlaaj\Dropbox (Uniandes)\RepositorioUniandes\Datos Salesforce    // original depository
*global base C:\Users\DANILO\Dropbox (Uniandes)\covida informes\datos\originales
*global processed C:\Users\DANILO\Dropbox (Uniandes)\covida informes\datos\procesados

/*
ONLY NEED TO USE THIS TO IMPORT THE CSV DATABASE THE FIRST TIME, THEN JUMP TO THE DTA FILE (REQUIRES STATA V16), number of the date may need to be adapted
*/
import delimited using "${originals}/TodosLosDatos_Instrumento_1_20210219", delimiters(";") clear  // siempre se requiere ajustar la base a la ultima disponible
rename númerodeidentificación cedula

save "${processed}/Datos_Salesforce.dta", replace

use "${processed}/Datos_Salesforce.dta", clear

* We drop observations outside of Bogota
drop if inlist(convenionombredelacuenta,  "PUTUMAYO-ACT", "PUTUMAYO-ACT-BANCO AGRARIO", "PUTUMAYO-ACT-BANCO POPULAR", "PUTUMAYO-ACT-CASA DE JUSTICIA" ///
"PUTUMAYO-ACT-COMERCIO", "PUTUMAYO-ACT-DOMICILIARIOS", "PUTUMAYO-ACT-FISCALIA", "PUTUMAYO-ACT-PUESTO INDEPENDENCIA")

split fechahoradeapertura, parse(",") limit(1) gen(fechahoradeaperturaalone)
gen survey_day = date( fechahoradeaperturaalone , "DMY")


gen fecha_tot=fechadediligenciamiento
foreach x in fecharecepciónmuestralab fechahoradeaperturaalone1{
	replace fecha_tot=`x' if fecha_tot==""
}

gen test_day = date(fecha_tot, "DMY") 

gen test_month=month(test_day)


foreach x in tos fiebre dolorgarganta dificultadrespiratori fatiga diarrea alteracionesolfatoogu confusión {
gen day1_`x' = date(fechainicio`x', "DMY")
}





egen day1_sick = rowmin(day1_*) 

gen days_sick = survey_day - day1_sick

split fechahoradeapertura, parse(",") limit(1) gen(fechanotificacionalone)
gen notification_day = date(fechanotificacionalone, "DMY") 

gen delay = notification_day - day1_sick

sum delay if delay <= 14 & delay >= 0  // time between symptom and obtaining the results


*codebook fechainicioseguimiento

*** 

replace cuálessuocupaciónnombredelaocupa=strlower(cuálessuocupaciónnombredelaocupa)
replace otraocupación=strlower(otraocupación)

rename cuálessuocupaciónnombredelaocupa ocupacionasis
replace ocupacionasis= otraocupación if ocupacionasis==""

replace ocupacionasis="oficiales y operarios de la construcción (obra gruesa) y afines no clasificados" if ocupacionasis=="oficiales y operarios de la construcción (obra gruesa) y afines no clasificados "
replace ocupacionasis=subinstr(ocupacionasis,"Ñ","ñ",.)
replace ocupacionasis="especialistas en bases de datos y en redes de computadores no clasificados bajo" if ocupacionasis=="especialistas en bases de datos y en redes de computadores no clasificados bajo "

*merge m:1 ocupacionasis using "${ocup_cat_new}\cats_new_fixed.dta", keep(1 3)
merge m:1 ocupacionasis using "${processed}/casos_ocupaciones_bogota_feb.dta", keep(1 3)

rename ocupacion_agregada ocup_cat
rename poblacion_agregada ocup_real_pop

drop _merge

recast str939 personaid, force

* This merge brings spacial data (including geoestrato, which is a stratum, based on localization)
merge m:1 personaid using "${processed}/salesforce_georeferenced_home_work.dta",  keep(1 3)  // requires updating here if the version changes

drop _merge

gen positive = (resultadomuestra== "Positivo") if resultadomuestra != "" 
replace positive = 0 if resultadomuestra=="Negativo"


gen one=1 if positive <.  // used for the weights, based on population that was tested only

bysort ocup_cat: gen sum_ocup = sum(one)
bysort ocup_cat: egen ocup_sample_pop = max(sum_ocup)

drop sum_ocup 

gen weight_ocup = ocup_real_pop / ocup_sample_pop
replace weight_ocup= round(weight_ocup,1)

*replace ocupacion_desagregada = "personal de restaurantes" if ocupacion_desagregada == "cocineros"

replace weight_ocup = 300 if weight_ocup > 300 & weight_ocup <.   // winsorize weights around 300 (10th percentile) to limit the influence of a given observation
qui sum weight_ocup
replace weight_ocup = r(mean) if weight_ocup ==.

gen pop_n = sum(one)
egen pop = max(pop_n)

bysort test_month: gen sum_month = sum(one)
bysort test_month: egen month_sample_pop = max(sum_month)

drop sum_month pop_n

gen weight_month = (pop / month_sample_pop) 
replace weight_month= round(weight_month,1)

gen weight_ocup_month = weight_ocup * weight_month
replace weight_ocup_month= round(weight_ocup_month,1)

bysort test_month: sum weight_month if one < .


replace weight_ocup= round(weight_ocup,1)
gen tracing = (convenionombredelacuenta=="RASTREO")

replace resultadomuestra = "" if resultadomuestra =="Inadecuado" | resultadomuestra == "No procesado"  

*table estudiodelqueustedhaceparte resultadomuestra, stubwidth(40)
*table convenionombredelacuenta resultadomuestra, stubwidth(40)

*Renombramos variables muy largas
rename usagelantibacterialeneldiaadia usagelantibacterial
rename pregunta1habitaconalgúncasocovid habitacasocovid
rename pregunta1disponedegelantibacteri disponegelantibact
rename pregunta2tienejabónantibacterial tienejabonantibact
rename pregunta3tienetoallasdesechables tienetoallasdesechables
rename pregunta5siempreusatapabocasn95 siempreusatapabocas
rename pregunta8contactomenosde2metros contactomenosde2metros
rename pregunta11procedimientosaerosole procedimientosaerosole
rename otraocupación otraocupacion

*Las observaciones que tengan género: --seleccione-- las dejamos en missing
replace género = "" if género=="--seleccione--"

*Las observaciones que tengan estrato: --seleccione-- las dejamos en missing
replace estrato = "" if estrato=="--seleccione--"

*Las observaciones que tengan usatapabocasensudiaadia: --seleccione-- las dejamos en missing
replace usatapabocasensudíaadía = "" if usatapabocasensudíaadía=="--seleccione--"

*Las observaciones que tengan usaguantesensusactividadesdiaria: --seleccione-- las dejamos en missing
replace usaguantesensusactividadesdiaria = "" if usaguantesensusactividadesdiaria=="--seleccione--"

*Las observaciones que tengan usagelantibacterialeneldiaadia: --seleccione-- las dejamos en missing
replace usagelantibacterial = "" if usagelantibacterial=="--seleccione--"

*Las observaciones que tengan pregunta1disponedegelantibacteri: --seleccione-- las dejamos en missing
replace disponegelantibact = "" if disponegelantibact=="--seleccione--"

*Las observaciones que tengan pregunta2tienejabónantibacterial: --seleccione-- las dejamos en missing
replace tienejabonantibact = "" if tienejabonantibact=="--seleccione--"

*Las observaciones que tengan pregunta3tienetoallasdesechables: --seleccione-- las dejamos en missing
replace tienetoallasdesechables = "" if tienetoallasdesechables=="--seleccione--"

*Las observaciones que tengan pregunta4usótapabocas: --seleccione-- las dejamos en missing
replace pregunta4usótapabocas = "" if pregunta4usótapabocas=="--seleccione--"

*Las observaciones que tengan pregunta7usoeppatenciónmédica: --seleccione-- las dejamos en missing
replace pregunta7usoeppatenciónmédica = "" if pregunta7usoeppatenciónmédica=="--seleccione--"

*Las observaciones que tengan pregunta8contactomenosde2metros: --seleccione-- las dejamos en missing
replace contactomenosde2metros = "" if contactomenosde2metros=="--seleccione--"

*Las observaciones que tengan pregunta9usoeppmenosde2metros: --seleccione-- las dejamos en missing
replace pregunta9usoeppmenosde2metros = "" if pregunta9usoeppmenosde2metros=="--seleccione--"

*Las observaciones que tengan pregunta11procedimientosaerosole: --seleccione-- las dejamos en missing
replace procedimientosaerosole = "" if procedimientosaerosole=="--seleccione--"

*Las observaciones que tengan habitacasocovid: --seleccione-- las dejamos en missing
replace habitacasocovid = "" if habitacasocovid=="--seleccione--"

*Las observaciones que tengan contactoconcovideneltrabajo: --seleccione-- las dejamos en missing
replace contactoconcovideneltrabajo = "" if contactoconcovideneltrabajo=="-- seleccione --"

*Las observaciones que tengan contactoconcovideneltrabajo: --seleccione-- las dejamos en missing
replace contactoconcovideneltrabajo = "Si" if contactoconcovideneltrabajo=="Sí"


* Replace yes no answers by dummy:
foreach x in contactoconcovideneltrabajo habitacasocovid {  
replace `x' = "1" if `x'=="Si" | `x'=="Sí"  
replace `x' = "0" if `x'=="No" 
destring `x', replace
sum `x'
}

*Creamos variable para agrupar viajes por dia
gen viajesaldia = "1-6 viajes" if pregunta2viajesaldía<=6
replace viajesaldia = "7-12 viajes" if pregunta2viajesaldía>=7 & pregunta2viajesaldía<=12
replace viajesaldia = "13-19 viajes" if pregunta2viajesaldía>=13 & pregunta2viajesaldía<=18
replace viajesaldia = "20-30 viajes" if pregunta2viajesaldía>=19 & pregunta2viajesaldía<=106


destring edad, replace force
*Creamos variable para agrupar edad
gen age_group = "18-29" if edad <30
replace age_group = "30-39" if edad>=31 & edad<40
replace age_group = "40-49" if edad>=41 & edad<50
replace age_group = "50-59" if edad>=51 & edad<60
replace age_group = "60-70" if edad>=61 & edad<70
replace age_group = "70plus" if edad>=71 & edad<.

egen agegroup = group(age_group)
replace agegroup = 0 if agegroup == .


egen nbsymptoms = rsum(tosseca fiebremayor38 dolordegarganta dificultadrespiratoria diarrea alteracionesensentidosolfatogust confusióndeapariciónreciente fatigacansancioextremoadinamia), missing

gen symptom = (nbsymptoms >=1 & nbsymptoms <.)

*Creamos variable numero de veces de lavado de manos
gen lavadomanos = "1-3" if cuántasvecesselavalasmanosaldía=="1" | cuántasvecesselavalasmanosaldía=="2" | cuántasvecesselavalasmanosaldía=="3"
replace lavadomanos = "4-7" if cuántasvecesselavalasmanosaldía=="4" | cuántasvecesselavalasmanosaldía=="5" | cuántasvecesselavalasmanosaldía=="6" | cuántasvecesselavalasmanosaldía=="7"
replace lavadomanos = "8-10" if cuántasvecesselavalasmanosaldía=="8" | cuántasvecesselavalasmanosaldía=="9" | cuántasvecesselavalasmanosaldía=="10"
replace lavadomanos = "más de 10" if cuántasvecesselavalasmanosaldía=="más de 10 veces"

*Creamos variable tiempo de lavado de manos
gen tiempolavadomanos = "0-20 segundos" if cuántoduralavándoselasmanos=="0 a 10 segundos" | cuántoduralavándoselasmanos=="10 a 20 segundos" | cuántoduralavándoselasmanos=="11 a 20 segundos"
replace tiempolavadomanos = "20-30 segundos" if cuántoduralavándoselasmanos == "20 a 30 segundos" | cuántoduralavándoselasmanos == "21 a 30 segundos"
replace tiempolavadomanos = "más de 30 segundos" if cuántoduralavándoselasmanos == "más de 30 segundos"

gen covid_hogar = (habitacasocovid ==1)

*Creamos variable con sintomas o contacto con covid
gen consintomasocontacto = (symptom == 1 | contactoconcovideneltrabajo == 1)

*Cambiamos el label de algunas variables
label var viajesaldia "Pregunta 2 - ¿Cuantos viajes hace al dia?"
label var agegroup "Age group"
label var symptom "At least 1 symptom"
label var lavadomanos "Número de veces que se lava las manos al día"
label var tiempolavadomanos "Cuánto tiempo dura lavandose las manos"
label var habitacasocovid "¿Habita con algún caso covid?"
label var disponegelantibact "Pregunta 1 ¿Dispone de gel antibacterial?"
label var usagelantibacterial "¿Usa gel antibacterial en su día a día?"
label var tienejabonantibact "Pregunta 2 ¿Tiene jabón antibacterial?"
label var tienetoallasdesechables "Pregunta 3 ¿Tiene toallas desechables?"
label var siempreusatapabocas "Pregunta 5 ¿Siempre usa tapabocas N95?"
label var contactomenosde2metros "Contacto menos de 2 metros"
label var procedimientosaerosole "¿Procedimientos aerosoles?"
label var consintomasocontacto "¿Ha tenido síntomas o contacto con alguien con COVID-19?"


gen public_call = (convenionombredelacuenta == "CAMPAÑA PUBLICA")

* Combine 2 variables that both measure nb days of work outside home:
gen work_outside_home = cuantosdíastrabajadosfueradecasa
*replace work_outside_home = pregunta1diastrabajados if work_outside_home == .  // it may just ask days of work (not outside of home) to be checked first


*Creamos variable para dias de trabajo fuera de la casa
gen work_outside_home_cat = " 1-3 dias" if cuantosdíastrabajadosfueradecasa <= 3
replace work_outside_home_cat = " 4-9 dias" if cuantosdíastrabajadosfueradecasa>=4 & pregunta2viajesaldía<=9
replace work_outside_home_cat = "10-14 dias" if cuantosdíastrabajadosfueradecasa>=10 & pregunta2viajesaldía<=14

egen stratum = group(estrato)

replace stratum = geohomeestrato if stratum == .  // con este comando, remplazamos estrato por estrato que se estimaron a partir de la direccion y geolocalizacion


* DUMMY FOR WHETHER WAS TESTED OR NOT
gen tested = (positive <.)

tab tested 

tab tested positive


rename mediodetransporteparairatrabajo transport
rename cuantosdíastrabajadosfueradecasa days_work_outside
rename habitacasocovid lives_with_covid
rename haviajadofueradelaciudad trip

rename obesidad obesity  
rename tabaquismo smoker
rename vacunainfluenzaestacional flu_vaccine 

rename edad age
replace age = . if age <5 | age >110

gen contact_COVID_diagnosed = (últimos14díasdiagnosticadaoproba== "Si con una persona diagnosticada con COVID-19")
gen contact_COVID_notdiagnosed = (últimos14díasdiagnosticadaoproba== "Si con una persona no diagnosticada pero probable COVID-19")

gen contact_COVID = contact_COVID_diagnosed + contact_COVID_notdiagnosed

gen contact = (contact_COVID ==1 | lives_with_covid == 1)

gen isolates = (days_work_outside<=2) if days_work_outside <.


/*
hist days_work_outside if stratum <=3, name(hist1) nodraw
hist days_work_outside if stratum >=4 & stratum <.,  name(hist2) nodraw
graph combine hist1 hist2 
*/



* Replace yes no answers by dummy:
foreach x in flu_vaccine vacunadelneumococo tomaacetaminofénoantinflamatorio usóantibióticoenlaúltimasemana trip {    //  HEHE   ######
replace `x' = "1" if `x'=="Si" | `x'=="Sí" | `x'=="1. Sí"  
replace `x' = "0" if `x'=="No" | `x'== "2. No"
replace `x' = "" if `x'=="--seleccione--" | `x'=="--Seleccione--" | `x'=="3. Desconocido"  
destring `x', replace
*sum `x'
}


sum asma epocenferpulmonarobstructivacrón insuficienciarenal diabetesmellitustipoioii enfermedadcardíaca cáncer hipertensiónarterial rinorrea doloresmuscularesmialgias enfermedadneurológicacrónica  tomafármacocorticoideoinmunosupr   vihuotrainmunodeficiencia  tuberculosis desnutriciónmalnutrición


egen nb_preconditions = rsum(asma epocenferpulmonarobstructivacrón insuficienciarenal diabetesmellitustipoioii enfermedadcardíaca cáncer hipertensiónarterial rinorrea doloresmuscularesmialgias enfermedadneurológicacrónica  vihuotrainmunodeficiencia  tuberculosis desnutriciónmalnutrición), missing

gen precondition = (nb_preconditions >=1 & nb_preconditions <.)


egen nb_medic = rsum(tomaacetaminofénoantinflamatorio usóantibióticoenlaúltimasemana tomafármacocorticoideoinmunosupr), missing
gen takes_drugs = (nb_medic >=1 & nb_medic <.)

gen pub_transp_freq = (frecuenciausotransportepúblico == "menos de una vez") if frecuenciausotransportepúblico != ""
replace pub_transp_freq = 2 if (frecuenciausotransportepúblico == "entre una y 4 veces")
replace pub_transp_freq = 3 if (frecuenciausotransportepúblico == "entre 5 y 9 veces")
replace pub_transp_freq = 4 if (frecuenciausotransportepúblico == "10 veces o mas")


gen gotowork_own_vehicle = (transport == "Bicicleta" | transport == "Carro, moto u otro vehículo propio" | transport == "Caminando")
gen gotowork_taxi = (transport == "Taxi (o aplicación como Uber/Cabify/Lift)" | transport == " Mototaxi")
gen gotowork_public = (transport == "TransMilenio u otro transporte público")


egen antibacterial = group(usagelantibacterial)
replace antibacterial = 0 if antibacterial == 5
egen wash_hands_freq = group(lavadomanos)
egen wash_hands_time = group(tiempolavadomanos)

egen mask_freq = group(usatapabocasensudíaadía)
replace mask_freq = 0 if mask_freq == 3
replace mask_freq = 3 if mask_freq == 1
replace mask_freq = 1 if mask_freq == 0
*tab mask_freq usatapabocasensudíaadía

foreach x in 1 2 3 4 5 6 {
gen strat`x' = (stratum == `x') if stratum <.
gen strat`x'_tested = (stratum == `x' & tested == 1) if stratum <.
}



* Variable confinamiento = 1 si antes del 1 de Septiembre
gen lockdown = (survey_day<22158) if survey_day <. 



save "${processed}/Datos_Salesforce_treated_feb19.dta", replace


st





/*
NEXT STEPS:

* MERGE BY OCCUPATIONS IN ORDER TO USE IT AS WEIGHTS: POP_OCCUP / POP IN SURVEY

* MERGE BY CEDULA O IDENTIFICACION: 
- check issue about duplicates (how frequent, only if positive)
- see if there are lots of cases where the merge fails (in that case may need help)
- Use a separate do file for this work?

* RE-DO SOME OF THE DESCRIPTIVE STATS I HAD TO LOOK AT SELF ISOLATION FOR EXAMPLE

* TEST OF SELECTION:
- RUN REGRESSION WITH DUMMIES FOR GROUP WHERE IT COMES FROM, AND DUMMY FOR TESTED OR NOT, FOR MOST MAIN VARIABLES
- generate the missing variables 



*/

stack

use "${processed}\tracing.dta" , clear

rename idenficacion cedula fechaprueba



** 

use  , clear




