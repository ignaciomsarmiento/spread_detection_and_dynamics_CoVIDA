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


setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")




# Parameters --------------------------------------------------------------
set.seed(101010) #seed
pop_bogota<-8044713
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*9
name<-"analytic_crude_no_weights"
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

#dta0<-dta_covida
#dta_covida<-dta0
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))

rates<-lm(positive~as.factor(date_m)-1,dta_covida)
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
  #filter(date_m>as.Date("2020-07-01")) %>% 
  select(date_m,covida_100,q025_covida_100,q975_covida_100) %>% 
  mutate(grp="Total Cases Estimated")
gg_sds <- rates %>% 
  #filter(date_m>as.Date("2020-07-01")) %>% 
  select(date_m,sds_100)  %>% 
  rename(covida_100=sds_100) %>% 
  mutate(grp="Detected Cases",
         q025_covida_100=NA,
         q975_covida_100=NA)




covida<-bind_rows(gg_covida,gg_sds) %>% filter(date_m>=as.Date("2020-06-01"))
covida <- covida %>% mutate(grp=factor(grp,levels=c("Total Cases Estimated","Detected Cases"), ordered=TRUE))

ggplot(data=covida)+
  geom_line(aes(x=date_m, y=covida_100,lty=grp,col=grp),size=1) +
  geom_ribbon(aes(x=date_m,ymin = q025_covida_100,  ymax = q975_covida_100,group=grp), alpha=0.2) +
  scale_color_manual(values=c("#008B45B2" ,"#631879B2" )) +
  scale_linetype_manual(values=c("solid","dashed")) +
  ylab("Daily  SARS-CoV-2 Cases \n per 100,000 Inhabitants") +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  annotate("text",x=as.Date("2020-08-25"), y=350, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.02) +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-06-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.01, 3)) +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=0.5,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(1,1,1,1), "cm"))
ggsave(paste0("views/Fig1_1_",name,".pdf"),height=6,width=9)


# Accum cases -------------------------------------------------------------



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




ins<-tibble(date_m=rep(as.Date("2020-10-15"),3),ins_point=c(0.3*100, NA ,0.04583383*100),ins_q025=c(.27*100,NA,NA),ins_q975=c(.33*100,NA,NA),grp=c("ins","covida","sds"))


#labels
lab_ins<- paste("NIH seroprevalence survey: ", ins$ins_point[1], " (",ins$ins_q025[1],"-",ins$ins_q975[1],")",sep="")
lab_ins
#extrapolate rate for Nov 15 covida
x1<-accum_gg_covida[accum_gg_covida$date_m==as.Date("2020-10-01"),c("accum_covida","q025","q975")]
x2<-accum_gg_covida[accum_gg_covida$date_m==as.Date("2020-11-01"),c("accum_covida","q025","q975")]
covidaNov<-round(x1+((x2-x1)/2),2)*100

lab_covida_nov<-paste("CoVIDA: ", covidaNov[1,1], " (",covidaNov[1,2],"-",covidaNov[1,3],")",sep="")
lab_covida_nov


covidaEnd<-round(accum_gg_covida[accum_gg_covida$date_m==as.Date(max(accum_gg_covida$date_m)),c("accum_covida","q025","q975")],2)*100

lab_covida_end<-paste("CoVIDA: ", covidaEnd$accum_covida, " (",covidaEnd$q025,"-",covidaEnd$q975,")",sep="")
lab_covida_end

#extrapolate rate for Nov 15 SDS
z1<-accum_gg_sds[accum_gg_sds$date_m==as.Date("2020-10-01"),c("accum_covida","q025","q975")]
z2<-accum_gg_sds[accum_gg_sds$date_m==as.Date("2020-11-01"),c("accum_covida","q025","q975")]
sdsNov<-round(z1+((z2-z1)/2),2)*100
sdsNov$accum_covida
lab_sds_nov<-paste0("HSB: ", sdsNov$accum_covida)
lab_sds_nov


sds_end<-accum_gg_sds[accum_gg_sds$date_m==max(accum_gg_sds$date_m),c("accum_covida","q025","q975")]*100
lab_sds_end<-paste("HSB: ", round(sds_end$accum_covida,2))
lab_sds_end


accum_covida<- accum_covida %>% 
  mutate(accum_covida=accum_covida*100,
         q975=q975*100,
         q025=q025*100)

ggplot(data=accum_covida %>% filter(date_m>=as.Date("2020-06-01")))+
  geom_line(aes(x=date_m, y=accum_covida,lty=grp,col=grp),size=1) +
  geom_ribbon(aes(x=date_m,ymin = q025,  ymax = q975,group=grp), alpha=0.2) +
  geom_point(data=ins,aes(x=date_m,y = ins_point)) +
  geom_errorbar(data=ins,aes(x=date_m,ymin = ins_q025,ymax=ins_q975)) +
  #geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  #annotate("text",x=as.Date("2020-09-15"), y=0.6, label="End of quarantine", colour="black", angle=0,size=3) +
  ylab("Accumulated  SARS-CoV-2 Cases \n as % of Population") +
  scale_color_manual(values=c("#008B45B2" ,"#631879B2" )) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_fill_manual(values=c("black","#008B45B2" ,"#631879B2")) +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-06-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.01, 3)) +
  annotate("text", x = as.Date("2020-10-15"), y = 0.30*100, label = lab_ins, size=3, hjust=-0.03)+ #lab INS
  annotate("text", x = as.Date("2020-10-15"), y = 0.36*100, label = lab_covida_nov,size=3)+ #Lab Covida Nov
  annotate("text", x = as.Date("2021-01-15"), y = 0.58*100, label = lab_covida_end,size=3)+ #Lab Covida End
  annotate("text", x = as.Date("2020-10-15"), y = 0.06*100, label = lab_sds_nov,size=3)+ #Lab SDS Nov
  annotate("text", x = as.Date("2021-01-22"), y = 0.11*100, label = lab_sds_end,size=3)+ #Lab SDS End
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=0.5,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(1,1,1,1), "cm"))
ggsave(paste0("views/Fig1_2_",name,".pdf"),height=6,width=9)



