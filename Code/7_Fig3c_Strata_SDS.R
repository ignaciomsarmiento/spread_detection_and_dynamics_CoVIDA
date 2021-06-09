##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","haven",'tidyr',"lubridate","here")
lapply(pkg, require, character.only=T)
rm(pkg)




# Read SDS data -----------------------------------------------------------
sds_dta<-read_dta(here("Data/sds_dta.dta"))

sds_dta<- sds_dta %>%
  filter(!is.na(test_day)) %>% 
  mutate(mes=month(test_day),
         year=year(test_day),
         date_m=dmy(paste("01",mes,year,sep="-"))) %>% 
          filter(test_day>as.Date("2020-04-01"))



poblaciones<-read_dta(here("Data/pob_strat.dta"))


db_sum<-sds_dta %>% 
  group_by(date_m,stratum)%>%
  filter(!is.na(stratum)) %>% 
  summarise(positive=sum(casos,na.rm = TRUE),
            .groups="drop") %>%
  left_join(.,poblaciones) %>% 
  mutate(rate_pos=positive*100000/pob_stratum) %>% dplyr::select(date_m,stratum,rate_pos)


db_sum<- db_sum %>% mutate(stratum=factor(stratum,levels=c("1","2","3","4"),
                                        labels=c("1&2","3","4","5&6"),ordered=TRUE))




ggplot(db_sum) +
  geom_line(aes(x=date_m, y=rate_pos,linetype=stratum,col=stratum),size=1, position=position_dodge(width =4)) +
  geom_point(aes(x=date_m, y=rate_pos,shape=stratum,col=stratum),size=2, position=position_dodge(width =4),alpha=0.6) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  ylab('Monthly COVID-19 Cases \n per 100k Inhabitants') +
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
  annotate("text",x=as.Date("2020-08-25"), y=1500, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.04)
ggsave(here(paste0("views/Fig3c.pdf")),height=6,width=10)


