##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("writexl","xtable","dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','ggsci',"lubridate","Hmisc","broom")
lapply(pkg, require, character.only=T)
rm(pkg)


#setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")
setwd("C:/Users/cdelo/Dropbox/Iceberg Paper/")


# Parameters --------------------------------------------------------------
set.seed(101010) #seed
pop_bogota<-8044713
days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
strat_pob<-read_dta("Data/pob_strat.dta")
# SDS ---------------------------------------------------------------
sds_dta<-read_dta("Data/sds_dta.dta")

sds_dta<- sds_dta %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-")))

table(sds_dta$mes,sds_dta$year)
summary(sds_dta$test_day)

casos<- sds_dta %>% 
  group_by(date_m) %>% 
  dplyr::summarise(casos=sum(casos), .groups="drop") %>%
  mutate(casos=ifelse(date_m>=as.Date("2021-02-01"),casos*28/14,casos)) #sds data are up untinl Feb 14, we project cases until the end of the month



# covida ------------------------------------------------------------------
dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta") 


dta_covida<- dta_covida %>% 
  mutate(date_m=ymd(date_m))


#%>% filter(date_m>=as.Date("2020-06-01"))

rates<-lm(positive~as.factor(date_m)-1,dta_covida,weights = weight_ocup)
rates<-broom::tidy(rates, conf.int = TRUE)
rates<- rates %>% mutate(term=str_remove_all(term,"as.factor(date_m)")) %>% 
  rename(date_m=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  mutate(date_m=ymd(date_m)) %>% 
  select(date_m,rate_pos,q025,q975) %>% 
  mutate(rate_pos= ifelse(date_m < as.Date('2020-06-01'),NA, rate_pos),
   q025= ifelse(date_m < as.Date('2020-06-01'),NA, q025),
   q975= ifelse(date_m < as.Date('2020-06-01'),NA, q975)
   )

rates<- rates %>% full_join(.,casos) %>% 
  mutate(month_days=monthDays(date_m))


rates <- rates %>% 
  mutate(casos_day_sds=ifelse(date_m == as.Date('2021-02-01'), (casos/28) , (casos/30)), # to be consistent with how we did it before (it marginally changes the results for july,december, august and november)   
         casos_day_covida=(rate_pos*pop_bogota)/17,
         q025_casos_day_covida=(q025*pop_bogota)/17,
         q975_casos_day_covida=(q975*pop_bogota)/17,
  )

         
# I'd simply do this....It yields exactly the same result as doing all the other process...

rates_month_all <- rates %>% 
  mutate(detected=casos_day_covida/casos_day_sds,
         detected_q025=q025_casos_day_covida/casos_day_sds,
         detected_q975=q975_casos_day_covida/casos_day_sds,
         month=month(date_m, label = TRUE, abbr = FALSE),
         ) %>% 
select(month,detected,detected_q025,detected_q975) %>% 
  na.omit()


# Now for the average for the period...


rates_agg_all<-rates %>% 
  #mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  filter(date_m > as.Date('2020-06-01')  & date_m < as.Date('2021-02-01'))%>% 
  summarise(covida_av= mean(casos_day_covida, na.rm = TRUE),
         covida_av_q025= mean(q025_casos_day_covida, na.rm = TRUE),
         covida_av_q975= mean(q975_casos_day_covida, na.rm = TRUE),
         sds_tot=sum(casos, na.rm = TRUE))%>% 
  mutate(covida_tot=covida_av*30*8,
         covida_tot_q025=covida_av_q025*30*8,
         covida_tot_q975=covida_av_q975*30*8) %>% 
  mutate(detected_agg = covida_tot/sds_tot,
         detected_agg_q025 = covida_tot_q025/sds_tot,
         detected_agg_q975 = covida_tot_q975/sds_tot
         )%>% 
  select(detected_agg,detected_agg_q025,detected_agg_q975) %>% 
  na.omit()

         
         



#---Now for the Strata/Month.....



rates_anag<- dta_covida %>%
   group_by(date_m,stratum) %>%
   dplyr::summarise(rate_pos_anal=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop",
                     var_anal=wtd.var(positive,weight_ocup,na.rm=TRUE),
#                    var_anal2=rate_pos_anal*(1-rate_pos_anal),
#                    se_anal=sqrt(var_anal),
                    obs=n()) %>%
   na.omit() %>%
  mutate(margin=qnorm(0.975)*sqrt(var_anal/obs),
        q025_anal=rate_pos_anal-margin,
         q975_anal=rate_pos_anal+margin,
         check=((q975_anal+q025_anal))/2-rate_pos_anal,
         )



casos_stratum<- sds_dta %>% 
  group_by(date_m,stratum) %>% 
  dplyr::summarise(casos=sum(casos), .groups="drop") %>%
  mutate(casos=ifelse(date_m>=as.Date("2021-02-01"),casos*28/14,casos)) %>%  #sds data are up untinl Feb 14, we project
  filter(!is.na(stratum))


rates_anag<- rates_anag %>% left_join(.,casos_stratum)
#%>% 
#  mutate(month_days=monthDays(date_m))

rates_anag<- rates_anag %>% left_join(.,strat_pob)



rates_anag <- rates_anag %>% 
  mutate(casos_day_sds=ifelse(date_m == as.Date('2021-02-01'), (casos/28) , (casos/30)), # to be consistent with how we did it before (it marginally changes the results for july,december, august and november)   
         casos_day_covida=(rate_pos_anal*pob_stratum)/17,
         q025_casos_day_covida=(q025_anal*pob_stratum)/17,
         q975_casos_day_covida=(q975_anal*pob_stratum)/17,
  )







# I'd simply do this....It yields exactly the same result as doing all the other process...

rates_strat <- rates_anag %>% 
  mutate(detected=casos_day_covida/casos_day_sds,
         detected_q025=q975_casos_day_covida/casos_day_sds,
         detected_q975=q025_casos_day_covida/casos_day_sds,
         month=month(date_m, label = TRUE, abbr = FALSE)
          )%>% 
  select(month,stratum, detected,detected_q025,detected_q975) %>% 
  na.omit()


# Now for the average for the period...


rates_agg_strat<-rates_anag %>% 
  #mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  group_by(stratum) %>% 
  filter(date_m > as.Date('2020-06-01') & date_m < as.Date('2021-02-01') ) %>%  
  summarise(covida_av= mean(casos_day_covida, na.rm = TRUE),
         covida_av_q025= mean(q025_casos_day_covida, na.rm = TRUE),
         covida_av_q975= mean(q975_casos_day_covida, na.rm = TRUE),
         sds_tot=sum(casos, na.rm = TRUE))%>% 
  mutate(covida_tot=covida_av*30*8,
         covida_tot_q025=covida_av_q025*30*8,
         covida_tot_q975=covida_av_q975*30*8) %>% 
  mutate(detected_agg = covida_tot/sds_tot,
         detected_agg_q025 = covida_tot_q025/sds_tot,
         detected_agg_q975 = covida_tot_q975/sds_tot
  )%>% 
  select(stratum,detected_agg,detected_agg_q025,detected_agg_q975) %>% 
  na.omit() %>% 
  mutate(check=((detected_agg_q025+detected_agg_q975)/2)-detected_agg)



#view----------
xtable(rates_month_all)
write_xlsx(rates_month_all,"Results_tables/rates_month_all.xlsx")

xtable(rates_agg_all)
write_xlsx(rates_agg_all,"Results_tables/rates_agg_all.xlsx")

xtable(rates_strat)
write_xlsx(rates_strat,"Results_tables/rates_strat.xlsx")

xtable(rates_agg_strat)
write_xlsx(rates_agg_strat,"Results_tables/rates_agg_strat.xlsx")





