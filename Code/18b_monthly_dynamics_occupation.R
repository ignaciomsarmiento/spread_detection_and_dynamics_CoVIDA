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





# Smoothing parameter -----------------------------------------------------
smoothing<- "raw"
set.seed(101010)
# - -----------------------------------------------------------------------

transf_ocup<-function(db){
  db <- db %>%
    mutate(ocup_cat = case_when(
      ocup_cat %in% c("personal ingenieria mecanica", "ingeniero y servicios informaticos") ~ "Architects and Engineers",
      ocup_cat %in% c("psicologos, sociologos y afines", "quimicos", "biologo y afines") ~ "Professional, Scientific and Technical Services"    ,
      ocup_cat %in% c("abogados")~"Lawyers",
      ocup_cat %in% c("trabajadores de la salud", "personal servicio comunitario","personal ingresos hospitalarios") ~ "Health Care and Social Assistance",
      ocup_cat %in% c("periodistas y escritores", "artistas y actividades culturales") ~ "Arts Entertainment and Recreation",
      ocup_cat %in% c("pensionado") ~ "Retired",
      ocup_cat %in% c("desempleado") ~ "Unemployed",
      ocup_cat %in% c("estudiante","profesores") ~ "Educational Services"  ,
      ocup_cat %in% c("obreros de construccion", "carpinteros y afines")~"Construction",
      ocup_cat %in% c("personal servicios financieros", "directores y gerentes de empresas")~"Finance, Management, and Insurance",
      ocup_cat %in% c("mensajero")~"Delivery Workers",
      ocup_cat %in% c("militares y fuerza publica")~"Military, Police, and Firefighters",
      ocup_cat %in% c("personal limpieza", "cuidador de niños")~"Nannies, Maids, and Housekeeping Cleaners",
      ocup_cat %in% c("ama de casa")~"Stay at home mothers",
      ocup_cat %in% c("personal secretaria","servicios apoyo produccion")~"Administrative and Support",
      ocup_cat %in% c("taxistas","personal transporte","personal de servicio a bordo")~"Taxi Drivers and Transportation",
      ocup_cat %in% c("vendedor tienda","peluqueros y afines","personal de restaurantes","vendedor ambulante")~"Retail Trade, Accommodation, and Food Services",
      ocup_cat %in% c("guardias seguridad")~ "Security Guards",
      ocup_cat %in% c("personal aseo")~ "Janitors and Cleaners",
      TRUE                                                          ~ NA_character_
    ))
  
  return(db)
  
}


transf_ocup_agg<-function(db){
  db<- db %>% 
    mutate(ocup_cat_agg = case_when(
      ocup_cat_agg %in% c("Architects and Engineers","Professional, Scientific and Technical Services","Lawyers") ~ "Professional, Lawyers, Architects, Engineers, \n Scientific and Technical Services",
      ocup_cat_agg %in% c("Security Guards")~"Security Guards",
      #ocup_cat_agg %in% c("Unemployed","Stay at home mothers")~"Unemployed and Stay at home mothers",
      ocup_cat_agg %in% c("Delivery Workers","Retail Trade, Accommodation, and Food Services","Janitors and Cleaners")~"Retail Trade,Food Services, \n Accommodation, Delivery, and Cleaners ",
      ocup_cat_agg %in% c("Finance, Management, and Insurance")~"Finance, Management, and Insurance",
      #ocup_cat_agg %in% c("Health Care and Social Assistance") ~ "Health Care and Social Assistance",
      ocup_cat_agg %in% c("taxistas","personal transporte","personal de servicio a bordo","Taxi Drivers and Transportation")~"Taxi Drivers and Transportation",
      TRUE                                                          ~ NA_character_
    ))
}
# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Datos_Salesforce_treated_feb19_clean.dta"))
#dta_covida<-read_dta(here("Data/Data_CoVIDA.dta"))

dta_covida<- dta_covida %>% filter(!(ocup_cat%in%c("agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")))


dta_covida<-transf_ocup(dta_covida)


#Fix a couple of wronly coded dates
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))



reg<-lm(positive~as.factor(ocup_cat):as.factor(date_m)-1,dta_covida ,weights = weight_ocup)
rates <-broom::tidy(reg, conf.int = TRUE)


rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(ocup_cat\\)"),
                           term=str_remove_all(term,"as.factor\\(date_m\\)"))  %>% 
                    separate(col=term,into=c("ocup_cat","date_m"),sep=":") %>% 
    mutate(rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100)  %>%
  select(ocup_cat,date_m,rate_pos,q025,q975)


obs<-  dta_covida %>% 
  group_by(ocup_cat,date_m) %>% 
  dplyr::summarise(mean=mean(positive),
                   Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop")



rates<- rates %>% 
  mutate(date_m=ymd(date_m)) %>% 
  left_join(.,obs) %>% 
  mutate(rate_pos=ifelse(is.na(rate_pos),0,rate_pos))


write.xlsx(rates,here("Results_tables/Dynamic_Ocup_Raw.xlsx"))


#
rates<- rates %>% mutate(q025=ifelse(q025<0,0,q025),
                 q025=ifelse(rate_pos==0,NA,q025),
                 q975=ifelse(q975>100,100,q975),
                 q975=ifelse(rate_pos==0,NA,q975),
                 Obs=ifelse(is.na(Obs),0,Obs))


#plot_db<-rates

#plot_db<- rates %>% filter(ocup_cat=="Health Care and Social Assistance")

plot_list<-list()

for(i in 1:length(table(rates$ocup_cat))){
  plot_db<- rates %>% filter(ocup_cat==names(table(rates$ocup_cat)[i]))
  plot_label_N<- paste0("N = ",format(plot_db$Obs,big.mark=","))

  plot_list[[i]]<-ggplot(plot_db, aes(x=date_m, y=rate_pos))+
                    geom_point(size=2)+
                    geom_errorbar(aes(ymin=q025, ymax=q975), width=.2, position=position_dodge(width = .4)) +
                    scale_x_date("", date_labels = "%b %Y",
                                 breaks = seq(as.Date("2020-06-01"),
                                              as.Date("2021-03-01"), "1 month"),
                                 expand = c(0.01, 3)) +
                    #ylab() +
                    scale_y_continuous("Positivity (%)",limits=c(0,20)) +
                    annotate("text", x = plot_db$date_m, y = 18, 
                             label = as.character(paste0(plot_label_N)),  
                             size= 5, angle=0) +
                    theme_bw()  +
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
  #facet_wrap(. ~ ocup_cat, ncol = 3)
  plot_list[[i]]
  ggsave(here(paste0("views/Fig_Occupations_",names(table(rates$ocup_cat)[i]),".pdf")),height=5,width=7)
}

# require(ggpubr)
# egg::ggarrange(plots=plot_list)
# ggsave(here(paste0("views/Fig_Occupations_","egg",".pdf")),height=20,width=18)  

rates<- rates %>% 
      mutate(q975=ifelse(q975>22,22,q975))

ggplot(rates, aes(x=date_m, y=rate_pos, label=Obs))+
  geom_point(size=2)+
  geom_line() +
  geom_errorbar(aes(ymin=q025, ymax=q975), width=.2, alpha=0.6, position=position_dodge(width = .4)) +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-06-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.05, 3)) +
  #ylab() +
  scale_y_continuous("Positivity (%)",limits=c(0,22)) +
  geom_text(aes(label = Obs, y=21, x=date_m),
            size = 4, 
            nudge_x = 0.9,
            check_overlap = T) +
  annotate("text", label = "Obs:", size = 4, x = ymd("2020-05-20"), y = 21) +
  theme_bw()  +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        rect = element_rect(colour = "transparent", fill = "white"),
        axis.text.x =element_text(size=12, angle=60,hjust=1),
        axis.text.y =element_text( size=12),
        strip.text = element_text(size=14)) +
  facet_wrap(. ~ ocup_cat, ncol = 3)
ggsave(here(paste0("views/Fig_Occupations.pdf")),height=20,width=18)
