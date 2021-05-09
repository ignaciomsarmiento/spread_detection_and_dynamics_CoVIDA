##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","haven",'tidyr',"lubridate","broom","here","stringr", "ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)

here()



# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Datos_Salesforce_treated_feb19_clean.dta"))

#Fix a couple of wronly coded dates
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))



reg<-lm(positive~as.factor(date_m)-1,dta_covida)

rates <-broom::tidy(reg, conf.int = TRUE)
rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(date_m\\)"))  %>% 
  separate(col=term,into=c("date_m"),sep=":") %>% 
  mutate(rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100) %>%
  select(date_m,rate_pos,q025,q975)

obs<-  dta_covida %>% 
  group_by(date_m) %>%
  dplyr::summarise(mean=mean(positive),
                   Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop")



rates<- rates %>% 
  mutate(date_m=ymd(date_m)) %>% 
  left_join(.,obs) %>% 
  mutate(rate_pos=ifelse(is.na(rate_pos),0,rate_pos)) 

p<-ggplot(rates, aes(x=date_m, y=rate_pos)) +
  geom_line(aes(x=date_m, y=rate_pos),size=0.5, position=position_dodge(width =0.5)) +
  geom_point(aes(x=date_m, y=rate_pos),size=2, position=position_dodge(width =4),alpha=0.6) +
  geom_errorbar(aes(x=date_m,ymin=q025, ymax=q975), width=.1, position=position_dodge(width = 4),alpha=0.6) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  scale_y_continuous('Positivity Rate',breaks = c(0,2,5,7),limits=c(0,7)) +
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
        axis.text.x =element_text( angle=0,hjust=0.5,size=9),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) +
  annotate("text",x=as.Date("2020-08-25"), y=6.5, label="End of quarantine", colour="black", angle=0,size=4,hjust=-0.04) +
  geom_text(aes(label=Obs), y = 7, size= 4, nudge_x = 0.9, check_overlap = T, color="black")+
  annotate("text", label = "Obs:", size = 4, x = ymd("2020-05-19"), y = 7)
p
ggsave("views/raw_positivity_months.pdf",height=6,width=8)
