##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("here","dplyr","haven","lubridate")
lapply(pkg, require, character.only=T)
rm(pkg)



# load Data ------------------------------------------------------------------
dta<-read_dta(here("../covid-project/data/UNIANDES/processed/Datos_Salesforce_treated_mar31.dta"))
dta$ocpacio
#fix coding of fecha toma muestra,

dta_covida<- dta %>%
        mutate(fechatomamuestra=ifelse(fechatomamuestra=="2/12/2021","12/02/2021",fechatomamuestra),
               fechatomamuestra=ifelse(fechatomamuestra=="2/10/2021","10/02/2021",fechatomamuestra),
               fechatomamuestra=ifelse(fechatomamuestra=="2/06/2021","06/02/2021",fechatomamuestra))




#According to my encoding fecharecepci? always comes as  fecharecepci?nmuestralab. Change in CPu if not your case
dta_covida<- dta_covida %>%
        mutate(test_day=as.Date(fechatomamuestra, "%d/%m/%Y"),
               #lab_reception=as.Date(fecharecepci?nmuestralab, "%d/%m/%Y"),#, origin="1960-01-01"), #for Camilo PC
               #lab_reception=as.Date(fecharecepciónmuestralab, "%d/%m/%Y"),#, origin="1960-01-01"), #for Camilo PC
               lab_reception=as.Date(fecharecepciónmuestralab,"%d/%m/%Y"), #, origin="1960-01-01"), #for Camilo PC
               fechahoradeaperturaalone1=as.Date(fechahoradeaperturaalone1, "%d/%m/%Y"),#, origin="1960-01-01"),
               test_day= ifelse(is.na(test_day), lab_reception,test_day), #fix test_day with fecharecepciónmuestralab
               test_day= ifelse(is.na(test_day), fechahoradeaperturaalone1,test_day), #fix test_day with fecharecepciónmuestralab
               test_day= as_date(test_day),
               exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0),
               mes=month(test_day),
               year=year(test_day),
               semana=week(test_day),
               stratum = case_when(stratum %in% c(1, 2) ~ 1,
                                   stratum %in% c(3, 4) ~ stratum - 1,
                                   stratum %in% c(5, 6) ~ 4),
               #date_m=dmy(paste("01",mes,year,sep="-")),
               date_m=floor_date(test_day, "month"),
               semana=ifelse(semana==53,52,semana),
               date_week=paste(year,semana,"1",sep="-"),
               date_week=as.Date(date_week, "%Y-%U-%u")
              ) %>%
        filter(date_m>as.Date("2020-04-01")) %>%  #drops 3 obs in may, we start in june
        filter(!is.na(positive)) %>%  #Missing test result (18453 obs, we end up with)57165 obs
        filter(exclude==0) %>%  #Exclude those with symtoms and contacts 
        filter(!(ocup_cat=="militares y fuerza publica" &  test_day==as.Date("2020-07-02")))
        

# We need this for all the figures and tables....
dta_covida<- dta_covida %>%
        mutate(ocup_cat=ifelse(grepl("aseo",ocupacion_desagregada),"personal limpieza",ocup_cat),
               poblacion_desagregada=ifelse(grepl("aseo",ocupacion_desagregada),108800,poblacion_desagregada),
               ocup_cat=ifelse(grepl("gruesa",ocupacionasis),	"obreros de construccion",ocup_cat	),
               poblacion_desagregada=ifelse(grepl("gruesa",ocupacion_desagregada),306000,poblacion_desagregada),
               ocupacion_desagregada=ifelse(grepl("gruesa",ocupacionasis),"obreros de construccion",ocupacion_desagregada	),
               ocup_cat=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocup_cat	),
               ocupacion_desagregada=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocupacion_desagregada	)
        )

#rename locality
dta_covida<- dta_covida %>%
        mutate(localidad=localidadderesidencianombredeloc)

#Vars we keep
dta_covida<- dta_covida %>% dplyr::select(personaid,positive,test_day,stratum,date_m,mes,year,localidad,weight_ocup_month,weight_ocup,exclude,ocup_cat)


write_dta(dta_covida, here("Data/Data_CoVIDA.dta"))
