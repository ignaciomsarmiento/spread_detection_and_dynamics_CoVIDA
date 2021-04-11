##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","haven",'tidyr',"lubridate","broom","here","stringr")
lapply(pkg, require, character.only=T)
rm(pkg)




# Parameters --------------------------------------------------------------
set.seed(101010) #seed
pop_bogota<-8044713
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*10 #days times months (june to march (10 months))
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
#dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) 



#Fix a couple of wronly coded dates
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         #date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))

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
         q975_tot_covida=month_days*q975_casos_day_covida,
         perc_sds=tot_sds/pop_bogota,
         perc_covida=tot_covida/pop_bogota,
         q025_perc_covida=q025_tot_covida/pop_bogota,
         q975_perc_covida=q975_tot_covida/pop_bogota,
         covida_100=casos_day_covida*100000/pop_bogota,
         q025_covida_100=q025_casos_day_covida*100000/pop_bogota,
         q975_covida_100=q975_casos_day_covida*100000/pop_bogota,
         sds_100=casos_day_sds*100000/pop_bogota)     





gg_covida<- rates %>% 
          select(date_m,covida_100,q025_covida_100,q975_covida_100) %>% 
          mutate(grp="Total Cases Estimated")
gg_sds <- rates %>% 
          select(date_m,sds_100)  %>% 
          rename(covida_100=sds_100) %>% 
          mutate(grp="Detected Cases",
                 q025_covida_100=NA,
                 q975_covida_100=NA)




covida<-bind_rows(gg_covida,gg_sds) %>% filter(date_m>=as.Date("2020-06-01"))
covida <- covida %>% mutate(grp=factor(grp,levels=c("Total Cases Estimated","Detected Cases"), ordered=TRUE))


# # -----------------------------------------------------------------------
# Cumulative cases -------------------------------------------------------------
# # -----------------------------------------------------------------------

accum_gg_covida<- rates %>% 
  select(date_m,perc_covida,q025_perc_covida,q975_perc_covida) %>% 
  mutate(grp="Total Cases Estimated") %>% 
  arrange(date_m) %>% 
  mutate(perc_covida=ifelse(is.na(perc_covida),0,perc_covida),
         q025_perc_covida=ifelse(is.na(q025_perc_covida),0,q025_perc_covida),
         q975_perc_covida=ifelse(is.na(q975_perc_covida),0,q975_perc_covida)) %>% 
  mutate(accum_covida=cumsum(perc_covida),
         q025=cumsum(q025_perc_covida),
         q975=cumsum(q975_perc_covida)) %>% 
  select(date_m,accum_covida,q025,q975,grp)
  



accum_gg_sds <- rates %>% 
  select(date_m,perc_sds)  %>% 
  rename(accum_covida=perc_sds) %>% 
  arrange(date_m) %>% 
  mutate(accum_covida=cumsum(accum_covida)) %>% 
  mutate(grp="Detected Cases",
         q025=NA,
         q975=NA) 


accum_covida<-bind_rows(accum_gg_covida,accum_gg_sds)
accum_covida <- accum_covida %>% mutate(grp=factor(grp,levels=c("Total Cases Estimated","Detected Cases"), ordered=TRUE))


  
save.image(here("Data/temp/Fig1_calculations.RData"))

