##########################################################
# author: CLDR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("xtable","dplyr","ggplot2","stringr","openxlsx","haven","tidyr","lubridate","here")
lapply(pkg, require, character.only=T)
rm(pkg)





#Read SDS----------------------------
sds<-read_dta(here("Data/sds_dta.dta"))



sds_dta <- sds %>% 
  mutate(death=ifelse(recuperado=="Fallecido",1,
                      ifelse(recuperado=="Recuperado",0,NA))) %>% 
 filter(!is.na(death)) %>% 
  mutate(test_day=dmy(fechatomademuestra),
         mes=month(test_day),
         year=year(test_day),
         df_month=dmy(paste("01",mes,year,sep="-"))
         ) %>% 
  group_by(df_month)%>%
  dplyr::summarise(deaths=sum(death),.groups="drop") %>% 
  dplyr::select(deaths, df_month)
  
  

sds_casos<- sds %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         df_month=dmy(paste("01",mes,year,sep="-"))) %>% 
  group_by(df_month)%>%
  dplyr::summarise(casos=sum(casos),.groups="drop") %>% 
  dplyr::select(casos, df_month)





#Read CoVida----------------------------

dta<-readRDS(here("Data/covida_dta.Rds"))
dta_covida<- dta %>%
  mutate(df_month=floor_date(as_date(test_day), "month"),
  )


# For the Iceberg for every month and total aggregated...

dta_covida_all<-dta_covida %>% 
  group_by(df_month)%>%
  dplyr::summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop")

dta_covida_all<-dta_covida_all%>% 
  mutate(rate_pos= ifelse(df_month < as.Date('2020-06-01'),NA, rate_pos))




#Create the number of cases----------------------------------

# Every Month. For all

iceberg_all_month <- merge(dta_covida_all,sds_dta, by="df_month", all= TRUE)
iceberg_all_month <- merge(iceberg_all_month,sds_casos, by="df_month", all= TRUE)


iceberg_all_month<-iceberg_all_month %>% 
  mutate(deaths_day = ifelse(df_month == as.Date('2021-01-01'), (deaths/17) , (deaths/30)),
         tot_day_cases_covida=((rate_pos*8044713)/17),
         casos_day = ifelse(df_month == as.Date('2021-01-01'), (casos/17) , (casos/30))
  ) %>% 
  mutate((death_month = deaths_day/tot_day_cases_covida)*100) %>% 
  mutate(cases_covida_month=tot_day_cases_covida*30) %>% 
  mutate(death_month2=(deaths/cases_covida_month)*100) %>% 
  mutate(death_monthsds=(deaths_day/casos_day)*100) 
  

# Aggregated. For all


iceberg_all_month<-iceberg_all_month%>% 
  mutate(covida_av= mean(tot_day_cases_covida[df_month > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot=sum(deaths[df_month > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot_casos=sum(casos[df_month > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot2=mean(deaths_day[df_month > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot_casos2=mean(casos_day[df_month > as.Date('2020-05-01')], na.rm = TRUE),
         )%>% 
  mutate(covida_tot=covida_av*30*8,
         deaths_agg = (sds_tot/covida_tot)*100,
         deaths_agg_sds=(sds_tot/sds_tot_casos)*100,
         
         deaths_agg2 = (sds_tot2/covida_av)*100,
         deaths_agg_sds2=(sds_tot2/sds_tot_casos2)*100
         )



#---------Extract table-------


iceberg_exp<-iceberg_all_month%>% 
  select(df_month,deaths_day,casos_day,tot_day_cases_covida, death_monthsds, death_month2, ) %>% 
  mutate(df_month=as.character.Date(df_month),
        casos_day=casos_day,
        tot_day_cases_covida=tot_day_cases_covida)
        
#write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_months.xlsx")
xtable(iceberg_exp)

iceberg_exp<-iceberg_all_month%>%
  filter(df_month == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(sds_tot2, sds_tot_casos2, covida_av, deaths_agg_sds2, deaths_agg2) %>% 
  mutate(sds_tot_casos2=sds_tot_casos2,
         covida_av=covida_av)
#write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_agg.xlsx")
xtable(iceberg_exp)

iceberg_exp<-iceberg_all_month%>%
  filter(df_month == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(sds_tot, sds_tot_casos, covida_tot, deaths_agg_sds, deaths_agg) %>% 
  mutate(sds_tot_casos=sds_tot_casos,
         covida_tot=covida_tot)
#write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_agg.xlsx")
xtable(iceberg_exp)


