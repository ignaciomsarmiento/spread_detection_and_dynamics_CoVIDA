##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr", "haven","openxlsx","stringr","broom","here")
lapply(pkg, require, character.only=T)
rm(pkg)


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
dta_covida<-read_dta(here("Data/Datos_Salesforce_treated_feb19_clean.dta"))
#dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) 


dta_covida<- dta_covida %>% filter(!(ocup_cat%in%c("agricultores y afines","personal de servicio a bordo","personal servicio comunitario","servicios apoyo produccion","entrenadores actividades deportivas")))


dta_covida<-transf_ocup(dta_covida)
dta_covida<- dta_covida %>% mutate(ocup_cat=ifelse(is.na(ocup_cat),"Not Classified",ocup_cat))




translate<-read.xlsx(here("Data/translate.xlsx"))
translate$ocupacion_desagregada<-trimws(translate$ocupacion_desagregada)
translate$ocup_desag_en<-str_to_title(translate$ocup_desag_en)

ocup_desag_en<- dta_covida %>% 
  group_by(ocup_cat, ocupacion_desagregada) %>% 
  dplyr::summarise(Obs=n(),
                   .groups="drop")%>% 
  left_join(.,translate)  %>% 
  select(ocup_cat,ocup_desag_en,Obs) %>% 
  mutate(ocup_n=paste0(ocup_desag_en, " (n= ",formatC(Obs, format="f", big.mark=",", digits=0),");")) %>% 
  arrange(ocup_cat,-Obs) %>%
  group_by(ocup_cat) %>%
  summarise(ocup_n=paste(ocup_n,collapse=' '), .groups = 'drop')



rates <-broom::tidy(lm(positive~as.factor(ocup_cat)-1,dta_covida ,weights = weight_ocup), conf.int = TRUE) 
rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(ocup_cat\\)")) %>% 
  mutate(ocup_cat=term,
         rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100)  %>% 
  select(ocup_cat,rate_pos,q025,q975) 



rates<-rates %>% 
        mutate(rate_pos=formatC(rate_pos, format="f", big.mark=",", digits=2),
               q025=formatC(q025, format="f", big.mark=",", digits=2),
               q975=formatC(q975, format="f", big.mark=",", digits=2),
               ci=paste0("(",q025,",", q975,")")) %>% 
        select(ocup_cat,rate_pos,ci)

obs<-  dta_covida %>% 
  group_by(ocup_cat) %>% 
  dplyr::summarise(Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop")


rates<- rates %>% 
          left_join(.,ocup_desag_en) %>% 
          left_join(.,obs) %>% 
          select(ocup_cat,ocup_n,rate_pos,ci,Obs)

write.xlsx(rates,here("Results_tables/Epid_week.xlsx"))
