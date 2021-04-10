#We need a table by stratum and a table by locality (2 separate additional tables) with in each case, 
#the population that we have in CoVIDA and the actual population (should we also have the one in SDS data?). 
#Many doubts in the discussion with SDS were about: do you have enough people in each stratum, etc. 


##########################################################
# author: CLDR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("xtable","sf","sp","dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','rsample','purrr',"lubridate","ggpubr")
lapply(pkg, require, character.only=T)
rm(pkg)


#setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")
setwd("C:/Users/cdelo/Dropbox")



# Parameters --------------------------------------------------------------
set.seed(101010) #seed
boots_replication<-2000 #Nber bootsrap


sds<-read_dta("Iceberg Paper/Data/sds_dta.dta")
dta_covida<-read_dta("Iceberg Paper/Data/Datos_Salesforce_treated_feb19_clean.dta")
dta_covida$pob_sample<-1
poblacion_loc<-read_dta("Iceberg Paper/Data/pob_loc.dta")
poblacion_strat<-read_dta("Iceberg Paper/Data/pob_strat.dta")


#Localidades




sds_loc<- sds %>%
  mutate(localidad=localidadasis) %>% 
  group_by(localidad) %>% 
  summarise(casos_sds=sum(casos)) %>% 
  filter(!is.na(localidad)) 

sds_loc<- sds_loc %>%
  mutate(localidad=substr(localidad,start=6, length(localidad)))

sds_loc$localidad[sds_loc$localidad=="Engativá"]<-"Engativa"
sds_loc$localidad[sds_loc$localidad=="Ciudad Bolívar"]<-"Ciudad Bolivar"
sds_loc$localidad[sds_loc$localidad=="Fontibón"]<-"Fontibon"
sds_loc$localidad[sds_loc$localidad=="Los Mártires"]<-"Martires"
sds_loc$localidad[sds_loc$localidad=="San Cristóbal"]<-"San Cristobal"
sds_loc$localidad[sds_loc$localidad=="Santafe"]<-"Santa Fe"
sds_loc$localidad[sds_loc$localidad=="Usaquén"]<-"Usaquen"

dta_covida_loc<- dta_covida %>%
  mutate(localidad=localidadderesidencianombredeloc) %>% 
  group_by(localidad) %>% 
  summarise(pob_sample=sum(pob_sample)) %>% 
  filter(!is.na(localidad)) 


dta_covida_loc<- merge(dta_covida_loc,poblacion_loc)
dta_covida_loc<- merge(dta_covida_loc,sds_loc)


dta_covida_loc<-dta_covida_loc %>% 
  ungroup %>% 
  mutate(pob_tot_loc=sum(pob_loc)) %>% 
  mutate(per_loc_covida=(pob_sample/pob_tot_loc)*100,
         per_loc_sds=(casos_sds/pob_tot_loc)*100)




#strata


sds_strat<- sds %>%
  group_by(stratum) %>% 
  summarise(casos_sds=sum(casos)) %>% 
  filter(!is.na(stratum)) 


dta_covida_strat<- dta_covida %>%
  group_by(stratum) %>% 
  summarise(pob_sample=sum(pob_sample)) %>% 
  filter(!is.na(stratum)) 


dta_covida_strat<- merge(dta_covida_strat,poblacion_strat)
dta_covida_strat<- merge(dta_covida_strat,sds_strat)


dta_covida_strat<-dta_covida_strat %>% 
  ungroup %>% 
  mutate(pob_tot_strat=sum(pob_stratum)) %>% 
  mutate(per_strat_covida=(pob_sample/pob_tot_strat)*100,
         per_strat_sds=(casos_sds/pob_tot_strat)*100)



# view and export...


dta_covida_strat<-dta_covida_strat %>% 
  select(stratum,pob_sample,per_strat_covida,casos_sds,per_strat_sds,pob_stratum)

dta_covida_loc<-dta_covida_loc %>% 
  select(localidad,pob_sample,per_loc_covida,casos_sds,per_loc_sds,pob_loc)

xtable(dta_covida_loc)

xtable(dta_covida_strat)






