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


# Helper Function -----------------------------------------------------

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
      ocup_cat_agg %in% c("taxistas","personal transporte","personal de servicio a bordo","Taxi Drivers and Transportation")~"Taxi Drivers \n and Transportation",
      TRUE                                                          ~ NA_character_
    ))
}

# Read data covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) %>%  filter(exclude_symptomatic==1)

dta_covida<- dta_covida %>% filter(!(ocup_cat%in%c("agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")))

dta_covida<-transf_ocup(dta_covida)

dta_covida<-dta_covida %>% 
  mutate(ocup_cat_agg=ocup_cat) 


dta_covida<-transf_ocup_agg(dta_covida)


dta_sum<-dta_covida %>% 
  filter(date_m>as.Date("2020-05-01")) 

reg<-lm(positive~as.factor(ocup_cat_agg):as.factor(date_m)-1,dta_covida ,weights = weight_ocup)

rates <-broom::tidy(reg, conf.int = TRUE)
rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(ocup_cat_agg\\)"),
                           term=str_remove_all(term,"as.factor\\(date_m\\)"))  %>% 
  separate(col=term,into=c("ocup_cat_agg","date_m"),sep=":") %>% 
  mutate(rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100)  %>%
  select(ocup_cat_agg,date_m,rate_pos,q025,q975)


obs<-  dta_sum %>% 
  group_by(ocup_cat_agg,date_m) %>% 
  dplyr::summarise(mean=mean(positive),
                   Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop")



rates<- rates %>% 
  mutate(date_m=ymd(date_m)) %>% 
  left_join(.,obs) %>% 
  mutate(rate_pos=ifelse(is.na(rate_pos),0,rate_pos))


rates<- rates %>% mutate(q025=ifelse(q025<0,0,q025),
                         q025=ifelse(rate_pos==0,NA,q025),
                         q975=ifelse(q975>100,100,q975),
                         q975=ifelse(rate_pos==0,NA,q975),
                         Obs=ifelse(is.na(Obs),0,Obs))

rates <- rates %>% complete(ocup_cat_agg,date_m = seq.Date(ymd("2020-04-01"), ymd("2021-02-01"), by="month"))%>%
  filter(!is.na(ocup_cat_agg))



# No Conf Intervals -------------------------------------------------------
p<-ggplot(rates) +
  geom_line(aes(x=date_m, y=rate_pos,linetype=ocup_cat_agg,col=ocup_cat_agg),size=1, position=position_dodge(width =4)) +
  geom_point(aes(x=date_m, y=rate_pos,shape=ocup_cat_agg,col=ocup_cat_agg),size=2, position=position_dodge(width =4),alpha=0.6) +
  #geom_errorbar(aes(x=date_m,ymin=q025, ymax=q975,col=ocup_cat_agg), width=.1, position=position_dodge(width = 4),alpha=0.6) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
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
        axis.text.x =element_text( angle=0,hjust=0.5,size=12),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) +
  scale_color_manual(values=c("#5DC863FF",  "#FDE725FF","#3B528BFF","#21908CFF",  "#440154FF"))+
  scale_linetype_manual(values=c(2,3,4,1,5)) +
  guides(col=guide_legend(title='Occupations',nrow = 2,title.position = "top",title.hjust =0.5),
         shape=guide_legend(title='Occupations',nrow = 2,title.position = "top",title.hjust =0.5),
         lty=guide_legend(title='Occupations',nrow = 2,title.position = "top",title.hjust =0.5)) + 
  annotate("text",x=as.Date("2020-08-25"), y=12, label="End of quarantine", colour="black", angle=0,size=5,hjust=-0.04)
p + scale_y_continuous('Positivity Rate',breaks = c(0,2.5,5,7.5,10,12.5),limits=c(0,14)) 
ggsave(here(paste0("views/Fig3a.pdf")),height=6.5,width=10)




# With Conf Intervals -------------------------------------------------------
p+ geom_errorbar(aes(x=date_m,ymin=q025, ymax=q975,col=ocup_cat_agg), width=.1, position=position_dodge(width = 4),alpha=0.6) +
   scale_y_continuous('Positivity Rate',breaks = c(0,2.5,5,7.5,10,12.5,15,17.5),limits=c(0,20)) 
ggsave(here(paste0("views/Fig3a_CI.pdf")),height=6,width=10)
