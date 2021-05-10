##########################################################
# author: CLDR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","stringr","openxlsx","haven",'tidyr',"lubridate","here")
lapply(pkg, require, character.only=T)
rm(pkg)



# Read Data ---------------------------------------------------------------
sds<-read_dta(here("Data/sds_dta.dta"))
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)
dta_covida$pob_sample<-1
poblacion_loc<-read_dta(here("Data/pob_loc.dta"))
poblacion_strat<-read_dta(here("Data/pob_strat.dta"))

#Localidades
sds_loc<- sds %>%
  mutate(localidad=localidadasis) %>% 
  group_by(localidad) %>% 
  summarise(casos_sds=sum(casos)) %>% 
  filter(!is.na(localidad)) 

sds_loc<- sds_loc %>%
  mutate(localidad=substr(localidad,start=6, length(localidad)))

sds_loc$localidad[sds_loc$localidad=="Engativ?"]<-"Engativa"
sds_loc$localidad[sds_loc$localidad=="Ciudad Bol?var"]<-"Ciudad Bolivar"
sds_loc$localidad[sds_loc$localidad=="Fontib?n"]<-"Fontibon"
sds_loc$localidad[sds_loc$localidad=="Los M?rtires"]<-"Martires"
sds_loc$localidad[sds_loc$localidad=="San Crist?bal"]<-"San Cristobal"
sds_loc$localidad[sds_loc$localidad=="Santafe"]<-"Santa Fe"
sds_loc$localidad[sds_loc$localidad=="Usaqu?n"]<-"Usaquen"

dta_covida_loc<- dta_covida %>%
  group_by(localidad) %>% 
  summarise(pob_sample=sum(pob_sample),.groups="drop") %>% 
  filter(!is.na(localidad)) 


dta_covida_loc<- left_join(dta_covida_loc,poblacion_loc)
dta_covida_loc<- left_join(dta_covida_loc,sds_loc)


dta_covida_loc<-dta_covida_loc %>%
  na.omit() %>% 
  mutate(pob_tot_loc=sum(pob_loc,na.rm = TRUE)) %>% 
  mutate(per_loc_covida=(pob_sample/pob_tot_loc)*100,
         per_loc_sds=(casos_sds/pob_tot_loc)*100)



# view and export...

dta_covida_loc<-dta_covida_loc %>% 
  mutate(pob_sample=formatC(pob_sample, format="f", big.mark=",", digits=0),
         per_loc_covida=formatC(per_loc_covida, format="f", big.mark=",", digits=2),
         casos_sds=formatC(casos_sds, format="f", big.mark=",", digits=0),
         per_loc_sds=formatC(per_loc_sds, format="f", big.mark=",", digits=2),
         pob_loc=formatC(pob_loc, format="f", big.mark=",", digits=0)
         ) %>% 
  dplyr::select(localidad,pob_sample,per_loc_covida,casos_sds,per_loc_sds,pob_loc)

dta_covida_loc


write.xlsx(dta_covida_loc,here("Results_tables/pop_by_locality.xlsx"))
