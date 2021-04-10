##########################################################
# author: CDLR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","haven","lubridate",'openxlsx','foreign')
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")
setwd("C:/Users/cdelo/Dropbox/Iceberg Paper/")




# load Data ------------------------------------------------------------------
dta<-read_dta("../covid-project/data/UNIANDES/processed/Datos_Salesforce_treated_feb19.dta")

#According to my encoding fecharecepci√ always comes as  fecharecepciÛnmuestralab. Change in CPu if not your case

dta_covida<- dta %>%
  mutate(test_day=as.Date(fechatomamuestra, "%d/%m/%Y"),
         lab_reception=as.Date(fecharecepciÛnmuestralab, "%d/%m/%Y"),#, origin="1960-01-01"),
         fechahoradeaperturaalone1=as.Date(fechahoradeaperturaalone1, "%d/%m/%Y"),#, origin="1960-01-01"),
         test_day= ifelse(is.na(test_day), lab_reception,test_day), #fix test_day with fecharecepci√≥nmuestralab
         test_day= ifelse(is.na(test_day), fechahoradeaperturaalone1,test_day), #fix test_day with fecharecepci√≥nmuestralab
         test_day= as_date(test_day),
         #exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0),
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
  )

# sum(is.na(dta_covida$test_day))
# sum(is.na(dta_covida$date_m))


# Create Occupations ------------------------------------------------------------------
#ocupation_table<-read.xlsx("Data/casos_ocupaciones_bogota_feb_1_0.xlsx")
#ocupation_table<-read.xlsx("C:/Users/cdelo/Dropbox/Iceberg Paper/Data/casos_ocupaciones_bogota_feb_1_0.xlsx")

# this are the only occupations that we are going to use now. 
dta_covida<- dta_covida %>% 
  mutate(ocup_cat=ifelse(grepl("aseo",ocupacion_desagregada),"personal limpieza",ocup_cat),
         poblacion_desagregada=ifelse(grepl("aseo",ocupacion_desagregada),108800,poblacion_desagregada),
         ocup_cat=ifelse(grepl("gruesa",ocupacionasis),	"obreros de construccion",ocup_cat	),
         poblacion_desagregada=ifelse(grepl("gruesa",ocupacion_desagregada),306000,poblacion_desagregada),
         ocupacion_desagregada=ifelse(grepl("gruesa",ocupacionasis),"obreros de construccion",ocupacion_desagregada	),
         ocup_cat=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocup_cat	),
         ocupacion_desagregada=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocupacion_desagregada	)
  )





savethis <- dta_covida %>% 
  select(ocup_cat, ocupacionasis, ocupacion_desagregada)%>%
  filter(!is.na(ocup_cat))  #Missing ocup_cat

savethis<- savethis %>% 
  group_by(ocup_cat,ocupacionasis) %>% 
  summarise(positive=first(ocupacion_desagregada))

population<-read_dta("C:/Users/cdelo/Dropbox/Iceberg Paper/Data/pob_cats.dta")

savethis2 <- merge(savethis,population, by="ocup_cat")
savethis2 <- savethis2 %>% 
  select(ocup_cat, ocupacionasis, poblacion_agregada)
  

write_dta(savethis2, "C:/Users/cdelo/Dropbox/Iceberg Paper/Data/cats_new_fixed.dta")

