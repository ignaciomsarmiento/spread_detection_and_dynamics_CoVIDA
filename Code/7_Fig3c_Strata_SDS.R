##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','ggsci','purrr',"lubridate","here")
lapply(pkg, require, character.only=T)
rm(pkg)



# Smoothing parameter -----------------------------------------------------
smoothing<- 0.7
set.seed(101010)
# - -----------------------------------------------------------------------


sds_dta<-read_dta(here("Data/sds_dta.dta"))

sds_dta<- sds_dta %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-")))



poblaciones<-read_dta("Data/pob_strat.dta")


db_sum<-sds_dta %>% 
  group_by(test_day,stratum)%>%
  filter(!is.na(stratum)) %>% 
  summarise(positive=sum(casos,na.rm = TRUE),
            .groups="drop") %>%
  left_join(.,poblaciones) %>% 
  mutate(rate_pos=positive*100000/pob_stratum) %>% dplyr::select(test_day,stratum,rate_pos)




db_sum<- db_sum %>% mutate(stratum=factor(stratum,levels=c("1","2","3","4"),
                                            labels=c("1&2","3","4","5&6"),ordered=TRUE),
                             test_day=lubridate::ymd(test_day))


#completes missing days
db_sum<- db_sum %>% complete(stratum,nesting(test_day))

#db<-dta_sum
smoother<-function(db){
  db[,"rate_pos"]<-zoo::na.fill(db[,"rate_pos"],"extend")
  x<-predict(loess(rate_pos ~ as.numeric(test_day) , data = db, span=smoothing),se=TRUE)
  tibble(rate_pos=x$fit,rate_pos_se=x$se.fit)
}


db_sum_smoothed<- db_sum %>% 
  group_by(stratum) %>% 
  arrange(test_day) %>% 
  do(rate_pos = smoother(.)) %>% 
  unnest(rate_pos) %>% 
  mutate(test_day=db_sum$test_day) %>% 
  filter(test_day>as.Date("2020-04-01"))


#db_sum_smoothed<- db_sum_smoothed %>% mutate(rate_pos=ifelse(rate_pos<0,0,rate_pos))

# geom_ribbon -------------------------------------------------------------

ggplot(db_sum_smoothed ) +
  geom_line(aes(x=test_day, y=rate_pos,lty=stratum,col=stratum),size=.7) +
  geom_ribbon(aes(x=test_day,ymin = rate_pos - qnorm(0.975)*rate_pos_se,  ymax = rate_pos + qnorm(0.975)*rate_pos_se,group=stratum), alpha=0.2) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred")  +
  ylab('Daily SARS-CoV-2 Cases \n per 100k Inhabitants') +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-03-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.01, 1.5)) +
  theme_bw() +
  theme(legend.title= element_text(size=14) ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=1,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) +
  scale_color_aaas()  +
  guides(col=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5),
         lty=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5))+
  annotate("text",x=as.Date("2020-08-25"), y=40, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.02) 
  
ggsave(paste0("views/Fig3_c_",smoothing,".pdf"),height=6,width=10)




# no CI -------------------------------------------------------------


ggplot(db_sum_smoothed) +
  geom_line(aes(x=test_day, y=rate_pos,lty=stratum,col=stratum),size=.7) +
  #geom_ribbon(aes(x=test_day,ymin = rate_pos - qnorm(0.975)*rate_pos_se,  ymax = rate_pos + qnorm(0.975)*rate_pos_se,group=stratum), alpha=0.2) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  ylab('Daily SARS-CoV-2 Cases \n per 100k Inhabitants') +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-03-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.01, 1.5)) +
  theme_bw() +
  theme(legend.title= element_text(size=14) ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=0.5,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) +
  scale_color_aaas()  +
  guides(col=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5),
         lty=guide_legend(title='Socioeconomic Strata',nrow = 1,title.position = "top",title.hjust =0.5))+
  annotate("text",x=as.Date("2020-08-25"), y=40, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.02) 
ggsave(paste0("views/Fig3_c_",smoothing,"_no_CI.pdf"),height=6,width=10)
