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
boots_replication<-2000 #Nber bootsrap

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




#FAST TRACK------------

x<-lm(positive~1,dta_all,weights = weight_ocup)
summary(x)
confint(x)

x<-lm(positive~1,dta_nosympt,weights = weight_ocup)
summary(x)
confint(x)

x<-lm(positive~1,dta_justsympt,weights = weight_ocup)
summary(x)
confint(x)

x<-lm(positive~1,dta_camp,weights = weight_ocup)
summary(x)
confint(x)

x<-lm(positive~1,dta_ncamp,weights = weight_ocup)
summary(x)
confint(x)



#Function Boots---------------------------------------------------------------------
boot_covida<-function(db,boots=FALSE){
  
  if(boots==TRUE){db<- analysis(db) } 
  
  rs <- db %>% 
    dplyr::summarise(rate_weight=weighted.mean(positive,weight_ocup,na.rm=TRUE),
                     rate=mean(positive,na.rm=TRUE),
                     obs=sum(obs)) %>% 
                     #.groups="drop") %>% 
    na.omit()
  
#  rs<-bind_rows(rates_oct,rates_jan)
  
  return(rs)
}


#-------------Bootstrapping------------
#All
rs<-boot_covida(dta_all)

covida_bootstraps<-bootstraps(dta_all,boots_replication) 
y<-lapply(covida_bootstraps$splits, boot_covida,boots=TRUE)
y1<-do.call(rbind,y)




y1_all<-y1
y1_all$gr<-1
y1_all$joini <- seq.int(nrow(y1_all)) 





#calculate 95% Boots CIs
y1<- y1 %>% 
  # group_by(localidad,grp) %>% 
  dplyr::summarize(q025_w=quantile(rate_weight,0.025,na.rm = TRUE),
                   q975_w=quantile(rate_weight,0.975,na.rm = TRUE),
                   q025_nw=quantile(rate,0.025,na.rm = TRUE),
                   q975_nw=quantile(rate,0.975,na.rm = TRUE),
                   .groups="drop")




y1$joini<-1
rs$joini<-1

rs<-left_join(rs,y1)
#rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","February 19th"),ordered = TRUE))
#,
#                  stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


#Science pallete
#pal_aaas("default", alpha = 0.7)(9)

rs<- rs %>% mutate(rate_weight=rate_weight*100,
                   rate=rate*100,
                   q025_w=q025_w*100,
                   q975_w=q975_w*100,
                   q025_nw=q025_nw*100,
                   q975_nw=q975_nw*100
)

rs_all<-rs





#no symptoms

rs<-boot_covida(dta_nosympt)

covida_bootstraps<-bootstraps(dta_nosympt,boots_replication) 
y<-lapply(covida_bootstraps$splits, boot_covida,boots=TRUE)
y1<-do.call(rbind,y)


y1_nosymt<-y1
y1_nosymt$gr<-1
y1_nosymt$joini <- seq.int(nrow(y1_nosymt)) 


#calculate 95% Boots CIs
y1<- y1 %>% 
  # group_by(localidad,grp) %>% 
  dplyr::summarize(q025_w=quantile(rate_weight,0.025,na.rm = TRUE),
                   q975_w=quantile(rate_weight,0.975,na.rm = TRUE),
                   q025_nw=quantile(rate,0.025,na.rm = TRUE),
                   q975_nw=quantile(rate,0.975,na.rm = TRUE),
                   .groups="drop")




y1$joini<-1
rs$joini<-1

rs<-left_join(rs,y1)
#rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","February 19th"),ordered = TRUE))
#,
#                  stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


#Science pallete
#pal_aaas("default", alpha = 0.7)(9)

rs<- rs %>% mutate(rate_weight=rate_weight*100,
                   rate=rate*100,
                   q025_w=q025_w*100,
                   q975_w=q975_w*100,
                   q025_nw=q025_nw*100,
                   q975_nw=q975_nw*100
)

rs_nosympt<-rs



#Just Symptoms


rs<-boot_covida(dta_justsympt)

covida_bootstraps<-bootstraps(dta_justsympt,boots_replication) 
y<-lapply(covida_bootstraps$splits, boot_covida,boots=TRUE)
y1<-do.call(rbind,y)

y1_symt<-y1
y1_symt$gr<-1
y1_symt$joini <- seq.int(nrow(y1_symt)) 

#calculate 95% Boots CIs
y1<- y1 %>% 
  # group_by(localidad,grp) %>% 
  dplyr::summarize(q025_w=quantile(rate_weight,0.025,na.rm = TRUE),
                   q975_w=quantile(rate_weight,0.975,na.rm = TRUE),
                   q025_nw=quantile(rate,0.025,na.rm = TRUE),
                   q975_nw=quantile(rate,0.975,na.rm = TRUE),
                   .groups="drop")









y1$joini<-1
rs$joini<-1

rs<-left_join(rs,y1)
#rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","February 19th"),ordered = TRUE))
#,
#                  stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


#Science pallete
#pal_aaas("default", alpha = 0.7)(9)

rs<- rs %>% mutate(rate_weight=rate_weight*100,
                   rate=rate*100,
                   q025_w=q025_w*100,
                   q975_w=q975_w*100,
                   q025_nw=q025_nw*100,
                   q975_nw=q975_nw*100
)

rs_justsympt<-rs




#just public campaigns




rs<-boot_covida(dta_camp)

covida_bootstraps<-bootstraps(dta_camp,boots_replication) 
y<-lapply(covida_bootstraps$splits, boot_covida,boots=TRUE)
y1<-do.call(rbind,y)

y1_camp<-y1
y1_camp$gr<-1
y1_camp$joini <- seq.int(nrow(y1_camp)) 




#calculate 95% Boots CIs
y1<- y1 %>% 
  # group_by(localidad,grp) %>% 
  dplyr::summarize(q025_w=quantile(rate_weight,0.025,na.rm = TRUE),
                   q975_w=quantile(rate_weight,0.975,na.rm = TRUE),
                   q025_nw=quantile(rate,0.025,na.rm = TRUE),
                   q975_nw=quantile(rate,0.975,na.rm = TRUE),
                   .groups="drop")



y1$joini<-1
rs$joini<-1

rs<-left_join(rs,y1)
#rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","February 19th"),ordered = TRUE))
#,
#                  stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


#Science pallete
#pal_aaas("default", alpha = 0.7)(9)

rs<- rs %>% mutate(rate_weight=rate_weight*100,
                   rate=rate*100,
                   q025_w=q025_w*100,
                   q975_w=q975_w*100,
                   q025_nw=q025_nw*100,
                   q975_nw=q975_nw*100
)

rs_camp<-rs



#Just lists



rs<-boot_covida(dta_nocamp)

covida_bootstraps<-bootstraps(dta_nocamp,boots_replication) 
y<-lapply(covida_bootstraps$splits, boot_covida,boots=TRUE)
y1<-do.call(rbind,y)



y1_ncamp<-y1
y1_ncamp$gr<-1
y1_ncamp$joini <- seq.int(nrow(y1_ncamp)) 




#calculate 95% Boots CIs
y1<- y1 %>% 
  # group_by(localidad,grp) %>% 
  dplyr::summarize(q025_w=quantile(rate_weight,0.025,na.rm = TRUE),
                   q975_w=quantile(rate_weight,0.975,na.rm = TRUE),
                   q025_nw=quantile(rate,0.025,na.rm = TRUE),
                   q975_nw=quantile(rate,0.975,na.rm = TRUE),
                   .groups="drop")



y1$joini<-1
rs$joini<-1

rs<-left_join(rs,y1)
#rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","February 19th"),ordered = TRUE))
#,
#                  stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


#Science pallete
#pal_aaas("default", alpha = 0.7)(9)

rs<- rs %>% mutate(rate_weight=rate_weight*100,
                   rate=rate*100,
                   q025_w=q025_w*100,
                   q975_w=q975_w*100,
                   q025_nw=q025_nw*100,
                   q975_nw=q975_nw*100
)

rs_nocamp<-rs




#### view

rs_nocamp<-rs_nocamp %>% 
  select(rate_weight,rate,q025_w,q975_w,q025_nw,q975_nw,obs)
rs_camp<-rs_camp %>% 
  select(rate_weight,rate,q025_w,q975_w,q025_nw,q975_nw,obs)
rs_all<-rs_all %>% 
  select(rate_weight,rate,q025_w,q975_w,q025_nw,q975_nw,obs)
rs_nosympt<-rs_nosympt %>% 
  select(rate_weight,rate,q025_w,q975_w,q025_nw,q975_nw,obs)
rs_justsympt<-rs_justsympt %>% 
  select(rate_weight,rate,q025_w,q975_w,q025_nw,q975_nw,obs)

xtable(rs_all)
xtable(rs_nosympt)
xtable(rs_justsympt)
xtable(rs_nocamp)
xtable(rs_camp)


#----SIMPLE TTEST------

library(stats)

y1_all<- y1_all %>% mutate(rate_weight=rate_weight*100,
                             rate=rate*100)

y1_nosymt<- y1_nosymt %>% mutate(rate_weight=rate_weight*100,
                               rate=rate*100)

y1_symt<- y1_symt %>% mutate(rate_weight=rate_weight*100,
                             rate=rate*100)

y1_ncamp<- y1_ncamp %>% mutate(rate_weight=rate_weight*100,
                               rate=rate*100)

y1_camp<- y1_camp %>% mutate(rate_weight=rate_weight*100,
                             rate=rate*100)

y1_ncamp<- y1_ncamp %>% mutate(rate_weight=rate_weight*100,
                               rate=rate*100)

#write_xlsx(y1_camp,"C:/Users/cdelo/Dropbox/Iceberg Paper/Results_tables/camp.xlsx")

#write_xlsx(y1_ncamp,"C:/Users/cdelo/Dropbox/Iceberg Paper/Results_tables/nocamp.xlsx")


t.test(y1_camp$rate_weight,y1_ncamp$rate_weight,alternative = "two.sided",mu=0)

t.test(y1_all$rate_weight, conf.level = 0.95)
t.test(y1_nosymt$rate_weight, conf.level = 0.95)
t.test(y1_symt$rate_weight, conf.level = 0.95)
t.test(y1_camp$rate_weight, conf.level = 0.95)
t.test(y1_ncamp$rate_weight, conf.level = 0.95)


t.test(y1_all$rate, conf.level = 0.95)
t.test(y1_nosymt$rate, conf.level = 0.95)
t.test(y1_symt$rate, conf.level = 0.95)
t.test(y1_camp$rate, conf.level = 0.95)
t.test(y1_ncamp$rate, conf.level = 0.95)



