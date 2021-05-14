##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","haven",'tidyr',"lubridate","broom","here","stringr","Hmisc")
lapply(pkg, require, character.only=T)
rm(pkg)

# Parameters --------------------------------------------------------------
set.seed(101010) #seed
pop_bogota<-8044713
strat_pob<-read_dta("Data/pob_strat.dta")
# SDS ---------------------------------------------------------------
sds_dta<-read_dta(here("Data/sds_dta.dta"))

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
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)


rates<-lm(positive~as.factor(date_m)-1,dta_covida,weights = weight_ocup_month)
rates<-broom::tidy(rates, conf.int = TRUE)
rates<- rates %>% mutate(term=str_remove_all(term,"as.factor(date_m)")) %>% 
  rename(date_m=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  mutate(date_m=ymd(date_m)) %>% 
  select(date_m,rate_pos,q025,q975)

rates<- rates %>% full_join(.,casos) %>% 
  mutate(month_days=30)#monthDays(date_m))


rates <- rates %>% 
  mutate(casos_day_sds=ifelse(date_m == as.Date('2021-02-01'), (casos/28) , (casos/30)), # to be consistent with how we did it before (it marginally changes the results for july,december, august and november)  
         casos_day_covida=(rate_pos*pop_bogota)/17,
         q025_casos_day_covida=(q025*pop_bogota)/17,
         q975_casos_day_covida=(q975*pop_bogota)/17,
  )




rates <- rates %>% 
  mutate(tot_sds=month_days*casos_day_sds,
         tot_covida=month_days*casos_day_covida,
         q025_tot_covida=month_days*q025_casos_day_covida,
         q975_tot_covida=month_days*q975_casos_day_covida)     


rates <- rates %>% 
  mutate(detected=tot_covida/tot_sds,
         detected_q025=q025_tot_covida/tot_sds,
         detected_q975=q975_tot_covida/tot_sds,
         month=month(date_m, label = TRUE, abbr = FALSE),
         detected_q025=ifelse(detected_q025<0,0,detected_q025)
  ) %>% 
  select(date_m,month,detected,detected_q025,detected_q975) %>% 
  na.omit() 

rates_all<-rates%>% 
  mutate(average=formatC(detected, format="f", big.mark=",", digits=1),
         detected_q025=formatC(detected_q025, format="f", big.mark=",", digits=1),
         detected_q975=formatC(detected_q975, format="f", big.mark=",", digits=1),
         ci=paste0("(",detected_q025,",", detected_q975,")")) %>% 
  select(date_m,month,average,ci)
  


rates_all_p<-rates_all %>%
  select(date_m,month,average) %>%  
  mutate(order=1)

rates_all_ci<-rates_all %>%
  select(date_m,month,ci) %>%
  rename(average=ci) %>% 
  mutate(order=2)

rates_all_l<-bind_rows(rates_all_p,rates_all_ci) %>% 
  arrange(date_m,order) %>% 
  select(month,average,order)

rates_all_l

# By month ----------------------------------------------------------------


rates_month<-lm(positive~as.factor(date_m):as.factor(stratum)-1,weights = weight_ocup,dta_covida)
rates_month<-broom::tidy(rates_month, conf.int = TRUE)
rates_month<- rates_month %>% mutate(term=str_remove_all(term,"as.factor\\(date_m\\)"),
                                     term=str_remove_all(term,"as.factor\\(stratum\\)")) %>% 
  separate(term,c('date_m',"stratum"),sep=":") %>% 
  rename(rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  mutate(date_m=ymd(date_m),
         stratum=as.numeric(stratum)) %>% 
  select(date_m,stratum,rate_pos,q025,q975) 



casos_stratum<- sds_dta %>% 
  group_by(date_m,stratum) %>% 
  dplyr::summarise(casos=sum(casos), .groups="drop") %>%
  mutate(casos=ifelse(date_m>=as.Date("2021-02-01"),casos*28/14,casos)) %>%  #sds data are up untinl Feb 14, we project
  filter(!is.na(stratum))



  
rates_month<- rates_month %>% full_join(.,casos_stratum)
rates_month<- rates_month %>% left_join(.,strat_pob)

rates_month <- rates_month %>% 
  mutate(month_days=30) %>% 
  mutate(casos_day_sds=ifelse(date_m == as.Date('2021-02-01'), (casos/28) , (casos/30)), # to be consistent with how we did it before (it marginally changes the results for july,december, august and november)  
         casos_day_covida=(rate_pos*pob_stratum)/17,
         q025_casos_day_covida=(q025*pob_stratum)/17,
         q975_casos_day_covida=(q975*pob_stratum)/17,
  ) %>% na.omit()


rates_month <- rates_month %>% 
            mutate(tot_sds=month_days*casos_day_sds,
                   tot_covida=month_days*casos_day_covida,
                   q025_tot_covida=month_days*q025_casos_day_covida,
                   q975_tot_covida=month_days*q975_casos_day_covida)     



rates_month_f <- rates_month %>% 
  mutate(detected=tot_covida/tot_sds,
         detected_q025=q025_tot_covida/tot_sds,
         detected_q975=q975_tot_covida/tot_sds,
         month=month(date_m, label = TRUE, abbr = FALSE),
         detected_q025=ifelse(detected_q025<0,0,detected_q025)
  ) %>% 
  mutate(detected=formatC(detected, format="f", big.mark=",", digits=1),
         detected_q025=formatC(detected_q025, format="f", big.mark=",", digits=1),
         detected_q975=formatC(detected_q975, format="f", big.mark=",", digits=1),
         ci=paste0("(",detected_q025,",", detected_q975,")")) %>% 
  select(date_m,month,stratum,detected,ci) %>% 
  na.omit()


rates_month_p<-rates_month_f %>%
              select(date_m,month,stratum,detected) %>%  
              spread(.,stratum,detected) %>% 
              mutate(order=1)

rates_month_ci<-rates_month_f %>%
  select(date_m,month,stratum,ci) %>%  
  spread(.,stratum,ci) %>% 
  mutate(order=2)


rates_month_l<-bind_rows(rates_month_p,rates_month_ci) %>% 
              arrange(date_m,order) %>% 
              select(order,month,`1`,`2`,`3`,`4`)

rates_month_l

table_detected<-left_join(rates_month_l,rates_all_l)

table_detected

# By strata ---------------------------------------------------------------

cases_strata<- rates_month %>% 
  mutate(detected=tot_covida/tot_sds,
         detected_q025=q025_tot_covida/tot_sds,
         detected_q975=q975_tot_covida/tot_sds,
         month=month(date_m, label = TRUE, abbr = FALSE),
         detected_q025=ifelse(detected_q025<0,0,detected_q025)
  ) %>% 
  group_by(stratum) %>% 
  dplyr::summarize(detected=mean(detected),
            detected_q025=mean(detected_q025),
            detected_q975=mean(detected_q975),
            .groups="drop")  

cases_strata_f<-cases_strata %>%   
  mutate(detected=formatC(detected, format="f", big.mark=",", digits=1),
         detected_q025=formatC(detected_q025, format="f", big.mark=",", digits=1),
         detected_q975=formatC(detected_q975, format="f", big.mark=",", digits=1),
         ci=paste0("(",detected_q025,",", detected_q975,")")) %>% 
  select(stratum,detected,ci) %>% 
  na.omit()


cases_strata_p<-cases_strata_f %>%
  select(stratum,detected) %>%  
  spread(.,stratum,detected) %>% 
  mutate(order=1)
cases_strata_p
cases_strata_ci<-cases_strata_f %>%
  select(stratum,ci) %>%  
  spread(.,stratum,ci) %>% 
  mutate(order=2)
cases_strata_ci

cases_strata_l<-bind_rows(cases_strata_p,cases_strata_ci) %>% 
  select(order,`1`,`2`,`3`,`4`)

cases_strata_l


# Period Average ----------------------------------------------------------

av_rates <- rates %>% 
  dplyr::summarize(detected=mean(detected),
                   detected_q025=mean(detected_q025),
                   detected_q975=mean(detected_q975)) %>% 
  mutate(detected=formatC(detected, format="f", big.mark=",", digits=1),
         detected_q025=formatC(detected_q025, format="f", big.mark=",", digits=1),
         detected_q975=formatC(detected_q975, format="f", big.mark=",", digits=1),
         ci=paste0("(",detected_q025,",", detected_q975,")"))

av_rates1<-av_rates %>% dplyr::select(detected) %>% rename(average=detected) %>% mutate(order=1)
av_rates2<-av_rates %>% dplyr::select(ci) %>% rename(average=ci) %>% mutate(order=2)

av_rates<-bind_rows(av_rates1,av_rates2)

cases_strata_l<-left_join(cases_strata_l,av_rates) %>% mutate(month="Average")

cases_strata_l
table_detected

# Final Table -------------------------------------------------------------

bind_rows(table_detected,cases_strata_l)
                   
