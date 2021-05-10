##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("here","dplyr","haven","lubridate","openxlsx")
lapply(pkg, require, character.only=T)
rm(pkg)


# SDS ---------------------------------------------------------------
sds_dta<-read_dta(here("Data/sds_dta.dta"))

sds_dta<- sds_dta %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-")))

sum_sds<- sds_dta %>% 
  group_by(stratum) %>% 
  tally() %>% 
  na.omit() %>% 
  rename(sds_pop=n)

# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)

sum_covida<- dta_covida %>% 
  group_by(stratum) %>% 
  tally() %>% 
  na.omit() %>% 
  rename(covida_pop=n)
  

#Official Count
poblacion<-read_dta(here("Data/pob_strat.dta"))
poblacion <- poblacion %>% 
  left_join(.,sum_covida) %>% 
  left_join(.,sum_sds) %>% 
  mutate(covida_perc=covida_pop*100/pob_stratum,
         sds_perc=sds_pop*100/pob_stratum)


#table
pop_by_strata<- poblacion %>% 
      select(label_stratum,covida_pop,covida_perc,sds_pop,sds_perc,pob_stratum)  %>% 
  mutate(covida_pop=formatC(covida_pop, format="f", big.mark=",", digits=0),
         covida_perc=formatC(covida_perc, format="f", big.mark=",", digits=2),
         sds_pop=formatC(sds_pop, format="f", big.mark=",", digits=0),
         sds_perc=formatC(sds_perc, format="f", big.mark=",", digits=2),
         pob_stratum=formatC(pob_stratum, format="f", big.mark=",", digits=0)
  )

pop_by_strata


write.xlsx(pop_by_strata,here("Results_tables/pop_by_strata.xlsx"))
