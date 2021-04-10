##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","openxlsx","haven","ggsci","tidyr")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")


# Smoothing parameter -----------------------------------------------------
smoothing<- 0.7
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
dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")

dta_covida<- dta_covida %>% filter(!(ocup_cat%in%c("agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")))


dta_covida<-transf_ocup(dta_covida)





dta_sum<-dta_covida %>% 
  group_by(test_day, ocup_cat)%>%
  filter(date_m>as.Date("2020-05-01")) %>% 
  summarise(rate_pos=weighted.mean(positive,weight_ocup,na.rm = TRUE),
            n=n(),.groups="drop") %>% 
  filter(!is.na(ocup_cat))


dta_sum<- dta_sum %>% mutate(test_day=lubridate::ymd(test_day))


#completes missing days
dta_sum<- dta_sum %>% complete(ocup_cat,nesting(test_day))

#db<-dta_sum
smoother<-function(db){
  db[,"rate_pos"]<-zoo::na.fill(db[,"rate_pos"],"extend")
  x<-predict(loess(rate_pos ~ as.numeric(test_day) , data = db, span=smoothing),se=TRUE)
  tibble(rate_pos=x$fit,rate_pos_se=x$se.fit)
}


db_sum_smoothed<- dta_sum %>% 
  group_by(ocup_cat) %>% 
  arrange(test_day) %>% 
  do(rate_pos = smoother(.)) %>% 
  unnest(rate_pos) %>% 
  mutate(test_day=dta_sum$test_day)








# geom_ribbon -------------------------------------------------------------

p<-ggplot(db_sum_smoothed) +
  geom_line(aes(x=test_day, y=rate_pos,group=ocup_cat),size=.7) +
  geom_ribbon(aes(x=test_day,ymin = rate_pos - qnorm(0.975)*rate_pos_se,  ymax = rate_pos + qnorm(0.975)*rate_pos_se,group=ocup_cat), alpha=0.2) +
  geom_vline(aes(xintercept=as.Date("2020-08-25")), linetype = "longdash",col="darkred") +
  ylab('Positivity Rate') +
  #ylim(c(-0.015,.2)) +
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-03-01"),
                            as.Date("2021-03-01"), "1 month"),
               expand = c(0.01, 0.01)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        rect = element_rect(colour = "transparent", fill = "white"),
        axis.text.x =element_text( angle=60,hjust=1),
        strip.text = element_text(size=14)) +
  guides(col=guide_legend(title='',nrow = 2),
         lty=guide_legend(title='',nrow = 2)
  )  + scale_color_aaas()
p + facet_wrap(. ~ ocup_cat, ncol = 3)

#p + annotate("text",x=as.Date("2020-09-06"), y=0.18, label="Selective isolation", colour="black", angle=0,size=3) 
ggsave(paste0("views/Fig_Occupations_",smoothing,".pdf"),height=20,width=18)



