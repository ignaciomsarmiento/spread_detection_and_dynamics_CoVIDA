##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","openxlsx","haven","ggsci","tidyr","here")
lapply(pkg, require, character.only=T)
rm(pkg)



# Read data covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Datos_Salesforce_treated_feb19_clean.dta"))
#dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) 

#Fix a couple of wronly coded dates
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))


dta_sum<-dta_covida %>% 
  group_by(test_day, stratum)%>%
  filter(!is.na(stratum)) %>% 
  filter(date_m>as.Date("2020-05-01"))


reg<-lm(positive~as.factor(stratum):as.factor(date_m)-1,dta_covida ,weights = weight_ocup)

rates <-broom::tidy(reg, conf.int = TRUE)
rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(stratum\\)"),
                           term=str_remove_all(term,"as.factor\\(date_m\\)"))  %>% 
  separate(col=term,into=c("stratum","date_m"),sep=":") %>% 
  mutate(rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100)  %>%
  select(stratum,date_m,rate_pos,q025,q975)


obs<-  dta_sum %>% 
  group_by(stratum,date_m) %>% 
  dplyr::summarise(mean=mean(positive),
                   Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop") %>% 
  mutate(stratum=as.character(stratum))



rates<- rates %>% 
  mutate(date_m=ymd(date_m)) %>% 
  left_join(.,obs) %>% 
  mutate(rate_pos=ifelse(is.na(rate_pos),0,rate_pos))


rates<- rates %>% mutate(q025=ifelse(q025<0,0,q025),
                         q025=ifelse(rate_pos==0,NA,q025),
                         q975=ifelse(q975>100,100,q975),
                         q975=ifelse(rate_pos==0,NA,q975),
                         Obs=ifelse(is.na(Obs),0,Obs))

rates<- rates %>% mutate(stratum=factor(stratum,levels=c("1","2","3","4"),
                                           labels=c("1&2","3","4","5&6"),ordered=TRUE))

rates <- rates %>% complete(stratum,date_m = seq.Date(ymd("2020-04-01"), ymd("2021-02-01"), by="month"))%>%
  filter(!is.na(stratum))

p<-ggplot(rates) +
  geom_line(aes(x=date_m, y=rate_pos,linetype=stratum,col=stratum),size=1, position=position_dodge(width =4)) +
  geom_point(aes(x=date_m, y=rate_pos,shape=stratum,col=stratum),size=2, position=position_dodge(width =4),alpha=0.6) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  #ylim(c(-0.015,.2)) +
  scale_y_continuous('Positivity Rate',breaks = c(0,2.5,5,7.5,10,12.5),limits=c(0,14)) +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-03-01"),
                            as.Date("2021-04-01"), "1 month"),
               expand = c(0.01, 10)) +
  theme_bw() +
  theme(legend.title= element_text(size=14) ,
        legend.position="bottom",
        legend.text=element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=0.5,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) +
  scale_color_manual(values=c("#a6611a","#dfc27d", "#80cdc1", "#018571"))+
  guides(lty=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5),
         shape=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5),
         col=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5))+
  annotate("text",x=as.Date("2020-08-25"), y=12, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.04)
p
ggsave(here(paste0("views/Fig3b.pdf")),height=6,width=10)


# With Conf Intervals -------------------------------------------------------
p+geom_errorbar(aes(x=date_m,ymin=q025, ymax=q975,col=stratum), width=.1, position=position_dodge(width = 4)) 
ggsave(here(paste0("views/Fig3b_CI.pdf")),height=6,width=10)
