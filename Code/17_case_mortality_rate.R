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


#Read SDS----------------------------
sds<-read_dta(here("Data/sds_dta.dta"))
sds$death<-NA
sds$death[sds$recuperado == "Fallecido"]<-1

sds$test_day
sds_dta <- sds %>% 
  dplyr::select(test_day, death ) %>% 
 filter(!is.na(death)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-"))
         ) %>% 
  group_by(date_m)%>%
  dplyr::summarise(deaths=sum(death),.groups="drop") %>% 
  dplyr::select(deaths, date_m)
  
  

sds_casos<- sds %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-"))) %>% 
  group_by(date_m)%>%
  dplyr::summarise(casos=sum(casos),.groups="drop") %>% 
  dplyr::select(casos, date_m)





#Read CoVida----------------------------
dta<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)

dta_covida<- dta %>%
  mutate(date_m=floor_date(as_date(test_day), "month"),
  )


# For the Iceberg for every month and total aggregated...

dta_covida_all<-dta_covida %>% 
  group_by(date_m)%>%
  dplyr::summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop")

dta_covida_all<-dta_covida_all%>% 
  mutate(rate_pos= ifelse(date_m < as.Date('2020-06-01'),NA, rate_pos))




#Create the number of cases----------------------------------

# Every Month. For all

iceberg_all_month <- merge(dta_covida_all,sds_dta, by="date_m", all= TRUE)
iceberg_all_month <- merge(iceberg_all_month,sds_casos, by="date_m", all= TRUE)


iceberg_all_month<-iceberg_all_month %>% 
  mutate(deaths_day = ifelse(date_m == as.Date('2021-01-01'), (deaths/17) , (deaths/30)),
         tot_day_cases_covida=((rate_pos*8044713)/17),
         casos_day = ifelse(date_m == as.Date('2021-01-01'), (casos/17) , (casos/30))
  ) %>% 
  mutate((death_month = deaths_day/tot_day_cases_covida)*100) %>% 
  mutate(cases_covida_month=tot_day_cases_covida*30) %>% 
  mutate(death_month2=(deaths/cases_covida_month)*100) %>% 
  mutate(death_monthsds=(deaths_day/casos_day)*100) 
  

# Aggregated. For all

iceberg_all_month<-iceberg_all_month%>% 
  mutate(covida_av= mean(tot_day_cases_covida[date_m > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot=sum(deaths[date_m > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot_casos=sum(casos[date_m > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot2=mean(deaths_day[date_m > as.Date('2020-05-01')], na.rm = TRUE),
         sds_tot_casos2=mean(casos_day[date_m > as.Date('2020-05-01')], na.rm = TRUE),
         )%>% 
  mutate(covida_tot=covida_av*30*8,
         deaths_agg = (sds_tot/covida_tot)*100,
         deaths_agg_sds=(sds_tot/sds_tot_casos)*100,
         
         deaths_agg2 = (sds_tot2/covida_av)*100,
         deaths_agg_sds2=(sds_tot2/sds_tot_casos2)*100
         )



#---------Extract table-------


iceberg_exp<-iceberg_all_month%>% 
  select(date_m,deaths_day,casos_day,tot_day_cases_covida, death_monthsds, death_month2, ) %>% 
  mutate(date_m=as.character.Date(date_m),
        casos_day=casos_day,
        tot_day_cases_covida=tot_day_cases_covida,
        month=paste(month(date_m, label = TRUE, abbr = FALSE),year(date_m),sep=" ")) %>% 
  filter(date_m>=as.Date("2020-06-01"), date_m<as.Date("2021-03-01"))
  
iceberg_exp

#write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_months.xlsx")


iceberg_exp<-iceberg_all_month%>%
  filter(date_m == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(sds_tot2, sds_tot_casos2, covida_av, deaths_agg_sds2, deaths_agg2) %>% 
  mutate(sds_tot_casos2=sds_tot_casos2,
         covida_av=covida_av)
iceberg_exp
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_agg.xlsx")


iceberg_exp<-iceberg_all_month%>%
  filter(date_m == as.Date('2020-8-01')) %>%  #Could be any date between july-jan. I am just doing this to easily get the output without openeing the df
  select(sds_tot, sds_tot_casos, covida_tot, deaths_agg_sds, deaths_agg) %>% 
  mutate(sds_tot_casos=sds_tot_casos,
         covida_tot=covida_tot)
iceberg_exp
write_xlsx(iceberg_exp,"Iceberg Paper/Results_tables/all_agg.xlsx")



