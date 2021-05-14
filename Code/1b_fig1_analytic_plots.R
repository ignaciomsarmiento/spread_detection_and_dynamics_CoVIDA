##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","here")
lapply(pkg, require, character.only=T)
rm(pkg)



# Load data --------------------------------------------------------------
load(here("Data/temp/Fig1_calculations.RData"))


# Figure 1a ---------------------------------------------------------------
ggplot(data=covida)+
  geom_line(aes(x=date_m, y=covida_100,lty=grp,col=grp),size=1) +
  geom_ribbon(aes(x=date_m,ymin = q025_covida_100,  ymax = q975_covida_100,group=grp), alpha=0.2) +
  scale_color_manual(values=c("#008B45B2" ,"#631879B2" )) +
  scale_linetype_manual(values=c("solid","dashed")) +
  ylab("Daily COVID-19 Cases \n per 100,000 Inhabitants") +
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
ggsave(here("views/Fig1_1_analytic.pdf"),height=6,width=9)


# # -----------------------------------------------------------------------
# Cumulative cases  -------------------------------------------------------------
# # ----------------------------------------------------------------------


# Labels ------------------------------------------------------------------
ins<-tibble(date_m=rep(as.Date("2020-10-15"),3),ins_point=c(0.3*100, NA ,0.04583383*100),ins_q025=c(.27*100,NA,NA),ins_q975=c(.33*100,NA,NA),grp=c("ins","covida","sds"))

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


# Figure 1b ---------------------------------------------------------------
ggplot(data=accum_covida %>% filter(date_m>=as.Date("2020-06-01")))+
  geom_line(aes(x=date_m, y=accum_covida,lty=grp,col=grp),size=1) +
  geom_ribbon(aes(x=date_m,ymin = q025,  ymax = q975,group=grp), alpha=0.2) +
  geom_point(data=ins,aes(x=date_m,y = ins_point)) +
  geom_errorbar(data=ins,aes(x=date_m,ymin = ins_q025,ymax=ins_q975)) +
  #geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  #annotate("text",x=as.Date("2020-09-15"), y=0.6, label="End of quarantine", colour="black", angle=0,size=3) +
  ylab("Accumulated COVID-19 Cases \n as % of Population") +
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
ggsave(here("views/Fig1_2_analytic.pdf"),height=6,width=9)

