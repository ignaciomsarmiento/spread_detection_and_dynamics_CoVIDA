##########################################################
# author: CDLR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("writexl","xtable","dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','rsample','ggsci',"lubridate","tidyverse")
lapply(pkg, require, character.only=T)
rm(pkg)



# Paths

setwd("C:/Users/cdelo/Dropbox/")
#setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")

#Set seed, just in case you need it. 
set.seed(101010)

#Read stratum pob
strat_pob<-read_dta("Iceberg Paper/Data/pob_strat.dta")


#SDS---------------------------------------------------------------------------------------
# Read clean SDS data base
sds<-read_dta("Iceberg Paper/Data/sds_dta.dta")

# Create a year/month date var
sds_dta<- sds %>%
  mutate(df_month=floor_date(as_date(test_day), "month")
  )

# For the Iceberg for every month and total aggregated...

dta_sds_all<-sds_dta %>% 
  group_by(df_month)%>%
  dplyr::summarise(casos=sum(casos,na.rm=TRUE),.groups="drop")

# For the Iceberg by Stratum/Month...

dta_sds_strat<-sds_dta %>% 
  group_by(df_month, stratum)%>%
  dplyr::summarise(casos=sum(casos,na.rm=TRUE),.groups="drop")

dta_sds_strat<-dta_sds_strat %>% 
  filter(!is.na(stratum)) #Exclude those without stratum (70 obs)
  

#covida-------------------------------------------------------------------------------------
# Read clean covida data base.

#dta<-readRDS("Iceberg Paper/Data/covida_dta.Rds")

#write_dta(dta_covida, "Data/Datos_Salesforce_treated_feb19_clean.dta")
dta<-read_dta("Iceberg Paper/Data/Datos_Salesforce_treated_feb19_clean.dta")


dta_covida<- dta %>%
  mutate(df_month=floor_date(as_date(test_day), "month"),
  )



# For the Iceberg for every month and total aggregated...

dta_covida_all<-dta_covida %>% 
  group_by(df_month)%>%
  dplyr::summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop")

dta_covida_all<-dta_covida_all%>% 
  mutate(rate_pos= ifelse(df_month < as.Date('2020-06-01'),NA, rate_pos))

# For the Iceberg by Stratum/Month...

dta_covida_strat<-dta_covida %>% 
  group_by(df_month,stratum)%>%
  dplyr::summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop")

dta_covida_strat<-dta_covida_strat %>% 
  filter(!is.na(stratum)) #Exclude those without stratum (1294 obs)

dta_covida_strat<-dta_covida_strat%>% 
  mutate(rate_pos= ifelse(df_month < as.Date('2020-06-01'),NA, rate_pos),
         rate_pos= ifelse(rate_pos == 0 , NA, rate_pos))

#ICEBERGS----------------------------------

# Every Month. For all

iceberg_all_month <- merge(dta_covida_all,dta_sds_all, by="df_month", all= TRUE)

iceberg_all_month<-iceberg_all_month %>% 
  mutate(casos_day = ifelse(df_month == as.Date('2021-02-01'), (casos/14) , (casos/30)), ##we have now data until 14th feb
         tot_day_cases_covida=((rate_pos*8044713)/17)
         )%>%
  mutate(detected_month = casos_day/tot_day_cases_covida)

# Aggregated. For all


iceberg_all_month<-iceberg_all_month %>% 
  mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  mutate(covida_av= mean(tot_day_cases_covida[df_month > as.Date('2020-06-01')], na.rm = TRUE),
         sds_tot=sum(casos2[df_month > as.Date('2020-06-01')], na.rm = TRUE))%>% 
  mutate(covida_tot=covida_av*30*8, #july-feb
         detected_tot = sds_tot/covida_tot)

# Every Month. By stratum
iceberg_strat_month <- merge(dta_covida_strat,dta_sds_strat, by=c("df_month","stratum"), all= TRUE)
iceberg_strat_month <- merge(iceberg_strat_month,strat_pob, by="stratum")

iceberg_strat_month<-iceberg_strat_month %>% 
  arrange(df_month,stratum)%>%
  mutate(casos_day = ifelse(df_month == as.Date("2021-02-01"), casos/14, casos/30),
         tot_day_cases_covida=((rate_pos*pob_stratum)/17)
         )%>%
  mutate(detected_month = casos_day/tot_day_cases_covida)

# Aggregated. By Stratum

iceberg_strat_month<-iceberg_strat_month%>%
 # filter(df_month > as.Date('2020-05-01'))%>%
  mutate(tot_day_cases_covida = ifelse(is.na(tot_day_cases_covida),0,tot_day_cases_covida))%>%
  group_by(df_month)%>%
  mutate(covida_av= sum(tot_day_cases_covida, na.rm = TRUE),
         sds_tot=sum(casos,na.rm = TRUE)
                     )%>%
ungroup%>%
  #filter(df_month > as.Date('2020-05-01'))%>%
  mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  mutate(covida_av2_tot= mean(covida_av[df_month > as.Date('2020-06-01')], na.rm = TRUE),
         sds_tot2_tot=sum(casos2[df_month > as.Date('2020-06-01')],na.rm = TRUE))%>%
  mutate(covida_av2_tot2=covida_av2_tot*30*8, #july-feb
         detected_tot2 = sds_tot2_tot/covida_av2_tot2)


iceberg_strat_month<-iceberg_strat_month%>%
  mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  # filter(df_month > as.Date('2020-05-01'))%>%
  mutate(tot_day_cases_covida = ifelse(is.na(tot_day_cases_covida),0,tot_day_cases_covida))%>%
  group_by(stratum)%>%
  mutate(covida_av2= mean(tot_day_cases_covida[df_month > as.Date('2020-06-01')], na.rm = TRUE),
         sds_tot2=sum(casos2[df_month > as.Date('2020-06-01')],na.rm = TRUE)
  )%>%
  #filter(df_month > as.Date('2020-05-01'))%>%
  mutate(covida_av2= covida_av2*30*8, #june-feb
         detected_tot = sds_tot2/covida_av2)


#Accumulation--------------------
#create the accumulated cases for the table

iceberg_all_month<-iceberg_all_month%>% 
  mutate(accum_cases_covid=30*tot_day_cases_covida/8044713,
         accum_tot=sum(accum_cases_covid, na.rm= TRUE))



#Export to Latex---------

iceberg_exp<-iceberg_all_month%>% 
  select(df_month,detected_month) %>% 
  mutate(df_month=as.character.Date(df_month))
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_months.xlsx")
xtable(iceberg_exp)

iceberg_exp<-iceberg_all_month%>%
  filter(df_month == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
select(detected_tot)
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_agg.xlsx")
xtable(iceberg_exp)


iceberg_exp<-iceberg_strat_month%>%
  filter(df_month > as.Date('2020-05-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(df_month, stratum, detected_month)%>% 
  arrange(stratum, df_month ) %>%
  mutate(df_month=as.character.Date(df_month))
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/strat_month.xlsx")
xtable(iceberg_exp)


iceberg_exp<-iceberg_strat_month%>%
  filter(df_month == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(stratum,detected_tot)%>% 
  arrange(stratum)
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/strat_agg.xlsx")
xtable(iceberg_exp)



  
