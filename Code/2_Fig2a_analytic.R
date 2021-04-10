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
#setwd("C:/Users/cdelo/Dropbox")



# Parameters --------------------------------------------------------------
set.seed(101010) #seed
name<-"analytic"
#days_oct<-as.numeric(dmy("30-11-2020")-dmy("01-06-2020"))
days_oct<-30*5
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*9

#db<-dta_covida
# Helper Function --------------------------------------------------------------
transf_ocup<-function(db){
  db <- db %>%
    mutate(ocup_cat = case_when(
      ocup_cat %in% c("personal ingenieria mecanica", "ingeniero y servicios informaticos") ~ "Architects and Engineers",
      ocup_cat %in% c("psicologos, sociologos y afines", "quimicos", "biologo y afines") ~ "Professional, Scientific \n and Technical Services"    ,
      ocup_cat %in% c("abogados")~"Lawyers",
      ocup_cat %in% c("trabajadores de la salud", "personal servicio comunitario","personal ingresos hospitalarios") ~ "Health Care and Social Assistance",
      ocup_cat %in% c("periodistas y escritores", "artistas y actividades culturales") ~ "Arts Entertainment and Recreation",
      ocup_cat %in% c("pensionado") ~ "Retired",
      ocup_cat %in% c("desempleado") ~ "Unemployed",
      ocup_cat %in% c("estudiante","profesores") ~ "Educational Services"  ,
      ocup_cat %in% c("obreros de construccion", "carpinteros y afines")~"Construction",
      ocup_cat %in% c("personal servicios financieros", "directores y gerentes de empresas")~"Finance, Management, \n and Insurance",
      ocup_cat %in% c("mensajero")~"Delivery Workers",
      ocup_cat %in% c("militares y fuerza publica")~"Military, Police, \n and Firefighters",
      ocup_cat %in% c("personal limpieza", "cuidador de niños")~"Nannies, Maids, \n and Housekeeping Cleaners",
      ocup_cat %in% c("ama de casa")~"Stay at home mothers",
      ocup_cat %in% c("personal secretaria","servicios apoyo produccion")~"Administrative and Support",
      ocup_cat %in% c("taxistas","personal transporte","personal de servicio a bordo")~"Taxi Drivers and Transportation",
      ocup_cat %in% c("vendedor tienda","peluqueros y afines","personal de restaurantes","vendedor ambulante")~"Retail Trade, Accommodation, \n and Food Services",
      ocup_cat %in% c("guardias seguridad")~ "Security Guards",
      ocup_cat %in% c("personal aseo")~ "Janitors and Cleaners",
      TRUE                                                          ~ NA_character_
    ))
  
  return(db)
  
}


# covida ------------------------------------------------------------------
dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")
#dta<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")
#dta_covida<-dta
dta_covida<- dta_covida %>% filter(!(ocup_cat%in%c("agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")))


dta_covida<-transf_ocup(dta_covida)
table(dta_covida$ocup_cat)

poblacion<- read_dta("Data/pob_cats.dta")
poblacion<-transf_ocup(poblacion)
poblacion<- poblacion %>% 
            group_by(ocup_cat) %>% 
            dplyr::summarize(poblacion_agregada=sum(poblacion_agregada),.groups="drop")

#db<-dta_covida

# Calculate rates by ocupation --------------------------------------------


#June November
rates_oct <-broom::tidy(lm(positive~as.factor(ocup_cat)-1,dta_covida %>%   filter(mes>4 & mes<12) ,weights = weight_ocup), conf.int = TRUE)

rates_oct<- rates_oct %>% 
            mutate(term=str_remove_all(term,"as.factor\\(ocup_cat\\)")) %>% 
            rename(ocup_cat=term,
                   rate_pos=estimate,
                   q025=conf.low,
                   q975=conf.high)  %>% 
            select(ocup_cat,rate_pos,q025,q975) %>%
            left_join(.,poblacion)%>%
            mutate(tot_day_cases_covida=((rate_pos*poblacion_agregada)/17)*days_oct,
                   q025_tot_day_cases_covida=((q025*poblacion_agregada)/17)*days_oct,
                   q975_tot_day_cases_covida=((q975*poblacion_agregada)/17)*days_oct) %>%
            mutate(acumm_covid_covida=tot_day_cases_covida/poblacion_agregada,
                   q025_acumm_covid_covida=q025_tot_day_cases_covida/poblacion_agregada,
                   q975_acumm_covid_covida=q975_tot_day_cases_covida/poblacion_agregada,
                             grp=1) %>%
                      na.omit()


#June March
rates_jan <-broom::tidy(lm(positive~as.factor(ocup_cat)-1,dta_covida ,weights = weight_ocup), conf.int = TRUE)

rates_jan<- rates_jan %>% 
  mutate(term=str_remove_all(term,"as.factor\\(ocup_cat\\)")) %>% 
  rename(ocup_cat=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  select(ocup_cat,rate_pos,q025,q975)  %>%
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
                   ocup_cat=factor(ocup_cat))
rs<- rs %>% mutate(ocup_cat=forcats::fct_reorder2(ocup_cat,grp,-acumm_covid_covida))


rs<- rs %>% mutate(acumm_covid_covida=acumm_covid_covida*100,
                   q025=q025_acumm_covid_covida*100,
                   q975=q975_acumm_covid_covida*100) %>% 
            mutate(q975=ifelse(q975>100,100,q975),
                   q025=ifelse(q025<0,0,q025),
                   )


ggplot(data=rs, aes(x=ocup_cat, y=acumm_covid_covida, group=grp, col=grp))+
  geom_point(size=1, position=position_dodge(width = .2))+
  geom_errorbar(aes(ymin=q025, ymax=q975), width=.1, position=position_dodge(width = .2)) +
  xlab("Occupations") +
  theme_bw() +
  scale_y_continuous("Accumulated SARS-COV-2 Cases \n as Percentage of Population",breaks =seq(0,100,20),limits=c(0,102)) +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=60,hjust=1,size=12),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) + scale_color_manual(values=c("#3B4992B2","#EE0000B2"))
ggsave(paste0("views/Fig2_a_",name,".pdf"),height=7,width=9)








