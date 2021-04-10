##########################################################
# author: CDLR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("writexl","sp","dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','rsample','purrr',"lubridate","ggpubr","xtable","dplyr","haven","lubridate",'openxlsx')
lapply(pkg, require, character.only=T)
rm(pkg)

#setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")
setwd("C:/Users/cdelo/Dropbox/Iceberg Paper/")

# Parameters --------------------------------------------------------------
set.seed(101010) #seed


#readdta and create-----

dta<-read_dta("../covid-project/data/UNIANDES/processed/Datos_Salesforce_treated_feb19.dta")


# We need this for lal the figures and tables....
dta<- dta %>% 
  mutate(ocup_cat=ifelse(grepl("aseo",ocupacion_desagregada),"personal limpieza",ocup_cat),
         poblacion_desagregada=ifelse(grepl("aseo",ocupacion_desagregada),108800,poblacion_desagregada),
         ocup_cat=ifelse(grepl("gruesa",ocupacionasis),	"obreros de construccion",ocup_cat	),
         poblacion_desagregada=ifelse(grepl("gruesa",ocupacion_desagregada),306000,poblacion_desagregada),
         ocupacion_desagregada=ifelse(grepl("gruesa",ocupacionasis),"obreros de construccion",ocupacion_desagregada	),
         ocup_cat=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocup_cat	),
         ocupacion_desagregada=ifelse(ocupacionasis=="constructores de casas",	"obreros de construccion",ocupacion_desagregada	)
  )

# take out those without posuitvity result and the military
dta<- dta %>%
  filter(!is.na(positive)) 

dta<- dta %>%
  mutate(test_day=as.Date(fechatomamuestra, "%d/%m/%Y")
  ) %>%
  filter(!(ocup_cat=="militares y fuerza publica" &  test_day==as.Date("2020-07-02"))) 
dta$obs<-1
# create the samples that you need...

# All --

dta_all<- dta

# no symptoms


dta_nosympt<- dta %>%
  mutate(exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0)) %>%
  filter(exclude==0) 

# just symptoms


dta_justsympt<- dta %>%
  mutate(exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0)) %>%
  filter(exclude!=0) 


# just publc campagines

dta_camp<- dta %>%
  mutate(exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0)) %>%
  filter(exclude==0) %>% 
  filter(convenionombredelacuenta=="CAMPAÑA PUBLICA")

# just lists
dta_nocamp<- dta %>%
  mutate(exclude=ifelse( symptom==1 | contact_COVID==1 | contact==1,1,0)) %>%
  filter(exclude==0) %>% 
  filter(convenionombredelacuenta!="CAMPAÑA PUBLICA")




#all observations------------

#para el test de diferencia de medias, armo una sample que tiene weight 1 (la sample not weighted)
#despues la sample weighted, 
#tienen un indicador sample 1 y 0
#hago el test, fijate que la constante, denota la media del weight 


no_weight<-broom::tidy(lm(positive~1,dta_all), conf.int = TRUE)%>% 
  select(term,estimate,conf.low,conf.high) %>% 
#  bind_rows(sample="Non-Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Non-Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight<-broom::tidy(lm(positive~1,dta_all,weights = weight_ocup), conf.int = TRUE) %>%
  select(term,estimate,conf.low,conf.high) %>%
  #  bind_rows(sample="Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

#para el test de diferncias
dta_no_weight<-dta_all %>% mutate(weight_ocup=1, sample=1)
dta_weight<-dta_all %>% mutate(sample=0)
dta_weight_no_weight<-bind_rows(dta_no_weight,dta_weight)
test<-broom::tidy(lm(positive~sample,dta_weight_no_weight,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight
no_weight




#Si conbinas las tres tablas a continuacion sale todo lo que necesitamos, de ahi tendrias que cambiar las bases


#No-Symptoms



no_weight<-broom::tidy(lm(positive~1,dta_nosympt), conf.int = TRUE)%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  #  bind_rows(sample="Non-Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Non-Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight<-broom::tidy(lm(positive~1,dta_nosympt,weights = weight_ocup), conf.int = TRUE) %>%
  select(term,estimate,conf.low,conf.high) %>%
  #  bind_rows(sample="Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

#para el test de diferncias
dta_no_weight<-dta_nosympt %>% mutate(weight_ocup=1, sample=1)
dta_weight<-dta_nosympt %>% mutate(sample=0)
dta_weight_no_weight<-bind_rows(dta_no_weight,dta_weight)
test<-broom::tidy(lm(positive~sample,dta_weight_no_weight,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)


weight
no_weight




#Just-Symptoms




no_weight<-broom::tidy(lm(positive~1,dta_justsympt), conf.int = TRUE)%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  #  bind_rows(sample="Non-Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Non-Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight<-broom::tidy(lm(positive~1,dta_justsympt,weights = weight_ocup), conf.int = TRUE) %>%
  select(term,estimate,conf.low,conf.high) %>%
  #  bind_rows(sample="Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

#para el test de diferncias
dta_no_weight<-dta_justsympt %>% mutate(weight_ocup=1, sample=1)
dta_weight<-dta_justsympt %>% mutate(sample=0)
dta_weight_no_weight<-bind_rows(dta_no_weight,dta_weight)
test<-broom::tidy(lm(positive~sample,dta_weight_no_weight,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)


weight
no_weight






#No Campaña


no_weight<-broom::tidy(lm(positive~1,dta_nocamp), conf.int = TRUE)%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  #  bind_rows(sample="Non-Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Non-Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight<-broom::tidy(lm(positive~1,dta_nocamp,weights = weight_ocup), conf.int = TRUE) %>%
  select(term,estimate,conf.low,conf.high) %>%
  #  bind_rows(sample="Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

#para el test de diferncias
dta_no_weight<-dta_nocamp %>% mutate(weight_ocup=1, sample=1)
dta_weight<-dta_nocamp %>% mutate(sample=0)
dta_weight_no_weight<-bind_rows(dta_no_weight,dta_weight)
test<-broom::tidy(lm(positive~sample,dta_weight_no_weight,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)


weight
no_weight



#Just Campaña


no_weight<-broom::tidy(lm(positive~1,dta_camp), conf.int = TRUE)%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  #  bind_rows(sample="Non-Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Non-Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

weight<-broom::tidy(lm(positive~1,dta_camp,weights = weight_ocup), conf.int = TRUE) %>%
  select(term,estimate,conf.low,conf.high) %>%
  #  bind_rows(sample="Weighted")  # not sure what was this for. It doesnt work I added the following line
  mutate(sample="Weighted",
         estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

#para el test de diferncias
dta_no_weight<-dta_camp %>% mutate(weight_ocup=1, sample=1)
dta_weight<-dta_camp %>% mutate(sample=0)
dta_weight_no_weight<-bind_rows(dta_no_weight,dta_weight)
test<-broom::tidy(lm(positive~sample,dta_weight_no_weight,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)


weight
no_weight




# RTEST CAMPAÑA NO CAMPAÑA
dta_test_camp<-dta_camp %>% mutate(sample=1)
dta_test_nocamp<-dta_nocamp %>% mutate(sample=0)

dta_test<-bind_rows(dta_test_camp,dta_test_nocamp)
test<-broom::tidy(lm(positive~sample,dta_test,weights = weight_ocup), conf.int = TRUE)
test<-test %>% 
  mutate(estimate=estimate*100,
         conf.low=conf.low*100,
         conf.high=conf.high*100)

test
