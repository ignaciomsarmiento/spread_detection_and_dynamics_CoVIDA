##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
#Load Packages
pkg<-list("dplyr","ggplot2","stringr","broom","here","haven")
lapply(pkg, require, character.only=T)
rm(pkg)




# Parameters --------------------------------------------------------------
set.seed(101010)
name<-"analytic"
#days_oct<-as.numeric(dmy("30-11-2020")-dmy("01-06-2020"))
days_oct<-30*5
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*9

# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)


poblacion<-read_dta(here("Data/pob_strat.dta"))
poblacion <- poblacion %>% 
  rename(poblacion_agregada=pob_stratum)



#db<-dta_covida



# Calculate rates by strata --------------------------------------------
#June November
rates_oct <-broom::tidy(lm(positive~as.factor(stratum)-1,dta_covida %>%   filter(mes>4 & mes<12) ,weights = weight_ocup), conf.int = TRUE)

rates_oct<- rates_oct %>% 
  mutate(term=str_remove_all(term,"as.factor\\(stratum\\)"),
         term=as.numeric(term)) %>% 
  rename(stratum=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  select(stratum,rate_pos,q025,q975) %>%
  left_join(.,poblacion) %>%
  mutate(tot_day_cases_covida=((rate_pos*poblacion_agregada)/17)*days_oct,
         q025_tot_day_cases_covida=((q025*poblacion_agregada)/17)*days_oct,
         q975_tot_day_cases_covida=((q975*poblacion_agregada)/17)*days_oct) %>%
  mutate(acumm_covid_covida=tot_day_cases_covida/poblacion_agregada,
         q025_acumm_covid_covida=q025_tot_day_cases_covida/poblacion_agregada,
         q975_acumm_covid_covida=q975_tot_day_cases_covida/poblacion_agregada,
         grp=1) %>%
  na.omit()


#June March
rates_jan <-broom::tidy(lm(positive~as.factor(stratum)-1,dta_covida ,weights = weight_ocup), conf.int = TRUE)

rates_jan<- rates_jan %>% 
  mutate(term=str_remove_all(term,"as.factor\\(stratum\\)"),
         term=as.numeric(term)) %>% 
  rename(stratum=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  select(stratum,rate_pos,q025,q975) %>%
  left_join(.,poblacion)%>%
  mutate(tot_day_cases_covida=((rate_pos*poblacion_agregada)/17)*days_fin,
         q025_tot_day_cases_covida=((q025*poblacion_agregada)/17)*days_fin,
         q975_tot_day_cases_covida=((q975*poblacion_agregada)/17)*days_fin) %>%
  mutate(acumm_covid_covida=tot_day_cases_covida/poblacion_agregada,
         q025_acumm_covid_covida=q025_tot_day_cases_covida/poblacion_agregada,
         q975_acumm_covid_covida=q975_tot_day_cases_covida/poblacion_agregada,
         grp=2) %>%
  na.omit()


rs<-bind_rows(rates_oct,rates_jan)
rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","March 3rd"),ordered = TRUE),
                   stratum=factor(stratum,levels=c(1,2,3,4), labels=c("1&2","3","4","5&6"),ordered = TRUE))


rs<- rs %>% mutate(acumm_covid_covida=acumm_covid_covida*100,
                   q025=q025_acumm_covid_covida*100,
                   q975=q975_acumm_covid_covida*100) %>% 
  mutate(q975=ifelse(q975>100,100,q975),
         q025=ifelse(q025<0,0,q025),
  )




ggplot(data=rs, aes(x=stratum, y=acumm_covid_covida, group=grp, col=grp))+
  geom_point(aes(shape=grp),size=2, position=position_dodge(width = .4))+
  geom_errorbar(aes(ymin=q025, ymax=q975,lty=grp), width=.2, position=position_dodge(width = .4)) +
  xlab("Socioeconomic Strata") +
  theme_bw() +
  scale_y_continuous("Accumulated SARS-COV-2 Cases \n as Percentage of Population",breaks =seq(0,100,20),limits=c(0,102)) +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=1,size=12),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) + scale_color_manual(values=c("#d8b365","#5ab4ac"))
ggsave(here(paste0("views/Fig2b_",name,".pdf")),height=5,width=7)
