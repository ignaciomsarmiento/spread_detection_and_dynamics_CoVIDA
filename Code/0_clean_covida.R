##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("here","dplyr","haven","lubridate",'openxlsx',"ids")
lapply(pkg, require, character.only=T)
rm(pkg)



# load Data ------------------------------------------------------------------
dta<-read_dta(here("../covid-project/data/UNIANDES/processed/Datos_Salesforce_treated_feb19.dta"))


#According to my encoding fecharecepci? always comes as  fecharecepci?nmuestralab. Change in CPu if not your case
dta_covida<- dta %>%
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
        filter(exclude==0) %>%  #Exclude those with symtoms and contacts (40292 obs)
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


#Vars we keep
dta_covida<- dta_covida %>% dplyr::select(personaid,positive,test_day,date_m,weight_ocup_month,exclude,ocup_cat)


write_dta(dta_covida, "Data/Datos_CoVIDA_clean.dta")
