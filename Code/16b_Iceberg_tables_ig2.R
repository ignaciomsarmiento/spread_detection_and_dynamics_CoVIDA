##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','ggsci',"lubridate","Hmisc","broom")
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
  mutate(detected=casos_day_sds/casos_day_covida,
         detected_q025=casos_day_sds/q025_casos_day_covida,
         detected_q975=casos_day_sds/q975_casos_day_covida,
         month=month(date_m, label = TRUE, abbr = FALSE),
         ) %>% 
select(month,detected,detected_q025,detected_q975) %>% 
  na.omit()


# Now for the average for the period...


rates_agg_all<-rates %>% 
  #mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  mutate(covida_av= mean(casos_day_covida[date_m > as.Date('2020-06-01') ], na.rm = TRUE),
         covida_av_q025= mean(q025_casos_day_covida[date_m > as.Date('2020-06-01') ], na.rm = TRUE),
         covida_av_q975= mean(q975_casos_day_covida[date_m > as.Date('2020-06-01')], na.rm = TRUE),
         sds_tot=sum(casos[date_m > as.Date('2020-06-01')], na.rm = TRUE))%>% 
  mutate(covida_tot=covida_av*30*8,
         covida_tot_q025=covida_av_q025*30*8,
         covida_tot_q975=covida_av_q975*30*8) %>% 
  mutate(detected_agg = sds_tot/covida_tot,
         detected_agg_q025 = sds_tot/covida_tot_q025,
         detected_agg_q975 = sds_tot/covida_tot_q975
         )%>% 
  select(date_m,detected_agg,detected_agg_q025,detected_agg_q975) %>% 
  na.omit()

         
         






#---Now for the Strata/Month.....





rates<-lm(positive~as.factor(date_m):as.factor(stratum)-1,dta_covida ,weights = weight_ocup)
rates<-broom::tidy(rates, conf.int = TRUE)

rates<- rates %>% 
  separate(term, c("date_m", "stratum"), ":") %>% 
  mutate(date_m=str_remove_all(date_m,"as.factor\\(date_m\\)"),
         stratum=str_remove_all(stratum,"as.factor\\(stratum\\)")
  ) %>% 
  mutate(date_m=ymd(date_m),
         stratum=as.numeric(stratum)) %>% 
  mutate(month=month(date_m, label = TRUE, abbr = FALSE)) %>%
  rename(rate_pos=estimate,
         q025=conf.low,
         q975=conf.high) %>% 
  select(date_m,month,stratum,rate_pos,std.error,q025,q975)

#to check, we get the same calculations
rates_anag<- dta_covida %>%
   group_by(date_m,stratum) %>%
   dplyr::summarise(rate_pos_anal=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop",
                     var_anal=wtd.var(positive,weight_ocup,na.rm=TRUE),
                    se_anal=sqrt(var_anal),
                    obs=n()) %>%
   na.omit() %>%
  mutate(q025_anal=rate_pos_anal-qnorm(0.975)*sqrt(var_anal/obs),
         q975_anal=rate_pos_anal+qnorm(0.975)*sqrt(var_anal/obs),
         )
#%>% 
   #left_join(rates)


casos_stratum<- sds_dta %>% 
  group_by(date_m,stratum) %>% 
  dplyr::summarise(casos=sum(casos), .groups="drop") %>%
  mutate(casos=ifelse(date_m>=as.Date("2021-02-01"),casos*28/14,casos)) %>%  #sds data are up untinl Feb 14, we project
  filter(!is.na(stratum))


rates_anag<- rates_anag %>% left_join(.,casos_stratum) %>% 
  mutate(month_days=monthDays(date_m))

rates_anag<- rates_anag %>% left_join(.,strat_pob)





rates_anag <- rates_anag %>% 
  mutate(casos_day_sds=ifelse(date_m == as.Date('2021-02-01'), (casos/28) , (casos/30)), # to be consistent with how we did it before (it marginally changes the results for july,december, august and november)   
         casos_day_covida=(rate_pos_anal*pob_stratum)/17,
         q025_casos_day_covida=(q025_anal*pob_stratum)/17,
         q975_casos_day_covida=(q975_anal*pob_stratum)/17,
  )







# I'd simply do this....It yields exactly the same result as doing all the other process...

rates_strat <- rates_anag %>% 
  mutate(detected=casos_day_sds/casos_day_covida,
         detected_q025=casos_day_sds/q025_casos_day_covida,
         detected_q975=casos_day_sds/q975_casos_day_covida,
         month=month(date_m, label = TRUE, abbr = FALSE),
  )%>% 
  select(month,stratum, detected,detected_q025,detected_q975) %>% 
  na.omit()


# Now for the average for the period...


rates_agg_strat<-rates_anag %>% 
  #mutate(casos2 = ifelse(df_month == as.Date('2021-02-01'), (casos*2) , (casos))) %>% 
  group_by(stratum) %>% 
  mutate(covida_av= mean(casos_day_covida[date_m > as.Date('2020-06-01')  ], na.rm = TRUE),
         covida_av_q025= mean(q025_casos_day_covida[date_m > as.Date('2020-06-01') ], na.rm = TRUE),
         covida_av_q975= mean(q975_casos_day_covida[date_m > as.Date('2020-06-01')], na.rm = TRUE),
         sds_tot=sum(casos[date_m > as.Date('2020-06-01')], na.rm = TRUE))%>% 
  mutate(covida_tot=covida_av*30*8,
         covida_tot_q025=covida_av_q025*30*8,
         covida_tot_q975=covida_av_q975*30*8) %>% 
  mutate(detected_agg = sds_tot/covida_tot,
         detected_agg_q025 = sds_tot/covida_tot_q025,
         detected_agg_q975 = sds_tot/covida_tot_q975
  )%>% 
  select(date_m,detected_agg,detected_agg_q025,detected_agg_q975) %>% 
  na.omit()











# We dont need to do this. Simply divde cases_day/estimated_cases_day




rates <- rates %>% 
  mutate(tot_sds=month_days*casos_day_sds,# we could take this out. it is just the same as casos
         tot_covida=month_days*casos_day_covida,
         q025_tot_covida=month_days*q025_casos_day_covida,
         q975_tot_covida=month_days*q975_casos_day_covida,
         perc_sds=tot_sds/pop_bogota, # why do we need this?
         perc_covida=tot_covida/pop_bogota, # why do we need this?
         q025_perc_covida=q025_tot_covida/pop_bogota,
         q975_perc_covida=q975_tot_covida/pop_bogota,
         covida_100=casos_day_covida*100000/pop_bogota,
         q025_covida_100=q025_casos_day_covida*100000/pop_bogota,
         q975_covida_100=q975_casos_day_covida*100000/pop_bogota,
         sds_100=casos_day_sds*100000/pop_bogota)     




gg_covida<- rates %>% 
  select(date_m,covida_100,q025_covida_100,q975_covida_100)

gg_sds <- rates %>% 
  select(date_m,sds_100)
  


av_month<-left_join(gg_covida,gg_sds)  %>%
        mutate(detected=sds_100/covida_100, # why this transformation?
               q025_detected=sds_100/q025_covida_100,
               q975_detected=sds_100/q975_covida_100,
               month=month(date_m, label = TRUE, abbr = FALSE))%>%
        select(month,detected,q025_detected,q975_detected) %>% 
        na.omit()







# Strata/Month ------------------------------------------------------------


rates<-lm(positive~as.factor(date_m):as.factor(stratum)-1,dta_covida ,weights = weight_ocup)
rates<-broom::tidy(rates, conf.int = TRUE)

rates<- rates %>% 
  separate(term, c("date_m", "stratum"), ":") %>% 
  mutate(date_m=str_remove_all(date_m,"as.factor\\(date_m\\)"),
         stratum=str_remove_all(stratum,"as.factor\\(stratum\\)")
         ) %>% 
  mutate(date_m=ymd(date_m),
         stratum=as.numeric(stratum)) %>% 
  mutate(month=month(date_m, label = TRUE, abbr = FALSE)) %>%
  rename(rate_pos=estimate,
         q025=conf.low,
         q975=conf.high) %>% 
  select(date_m,month,stratum,rate_pos,q025,q975)

#to check, we get the same calculations
# rates_anag<- dta_covida %>% 
#   group_by(date_m,stratum) %>% 
#   dplyr::summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm=TRUE),.groups="drop") %>% 
#   na.omit() %>% 
#   left_join(rates)


casos_stratum<- sds_dta %>% 
  group_by(date_m,stratum) %>% 
  dplyr::summarise(casos=sum(casos), .groups="drop") %>%
  mutate(casos=ifelse(date_m>=as.Date("2021-02-01"),casos*28/14,casos)) %>%  #sds data are up untinl Feb 14, we project
  filter(!is.na(stratum))


rates<- rates %>% left_join(.,casos_stratum) %>% 
  mutate(month_days=monthDays(date_m))


rates <- rates %>% 
  mutate(casos_day_sds=casos/month_days,
         casos_day_covida=(rate_pos*pop_bogota)/17,
         q025_casos_day_covida=(q025*pop_bogota)/17,
         q975_casos_day_covida=(q975*pop_bogota)/17,
  )


#falta pop strato Recalcular esto
rates <- rates %>% 
  mutate(tot_sds=month_days*casos_day_sds,
         tot_covida=month_days*casos_day_covida,
         q025_tot_covida=month_days*q025_casos_day_covida,
         q975_tot_covida=month_days*q975_casos_day_covida,
         perc_sds=tot_sds/pop_bogota,
         perc_covida=tot_covida/pop_bogota,
         q025_perc_covida=q025_tot_covida/pop_bogota,
         q975_perc_covida=q975_tot_covida/pop_bogota,
         covida_100=casos_day_covida*100000/pop_bogota,
         q025_covida_100=q025_casos_day_covida*100000/pop_bogota,
         q975_covida_100=q975_casos_day_covida*100000/pop_bogota,
         sds_100=casos_day_sds*100000/pop_bogota)     
