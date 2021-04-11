##########################################################
# author: CLDR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("rgdal","sf","sp","dplyr","ggplot2","stringr","openxlsx","haven",'tidyr','rsample','purrr',"lubridate","ggpubr","here")
lapply(pkg, require, character.only=T)
rm(pkg)




# Parameters --------------------------------------------------------------
set.seed(101010) #seed
name<-"analytic"
#days_oct<-as.numeric(dmy("30-11-2020")-dmy("01-06-2020"))
days_oct<-30*5
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*10

# covida ------------------------------------------------------------------
#dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta")) 



poblacion<-read_dta(here("Data/pob_loc.dta")) %>% 
              rename(poblacion_agregada=pob_loc)




# Calculate rates by strata --------------------------------------------

#June November
rates_oct <-broom::tidy(lm(positive~as.factor(localidad)-1,dta_covida %>%   filter(mes>4 & mes<12) ,weights = weight_ocup), conf.int = TRUE)

rates_oct<- rates_oct %>% 
  mutate(term=str_remove_all(term,"as.factor\\(localidad\\)"))  %>% 
  rename(localidad=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  select(localidad,rate_pos,q025,q975) %>%
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
rates_jan <-broom::tidy(lm(positive~as.factor(localidad)-1,dta_covida ,weights = weight_ocup), conf.int = TRUE)

rates_jan<- rates_jan %>% 
  mutate(term=str_remove_all(term,"as.factor\\(localidad\\)")) %>% 
  rename(localidad=term,
         rate_pos=estimate,
         q025=conf.low,
         q975=conf.high)  %>% 
  select(localidad,rate_pos,q025,q975) %>%
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
rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","March 30th"),ordered = TRUE))


rs<- rs %>% mutate(acumm_covid_covida=acumm_covid_covida*100,
                   q025=q025_acumm_covid_covida*100,
                   q975=q975_acumm_covid_covida*100) %>% 
  mutate(acumm_covid_covida=ifelse(acumm_covid_covida>100,100,acumm_covid_covida),
        q975=ifelse(q975>100,100,q975),
         q025=ifelse(q025<0,0,q025),
  )




dta<-read_dta(here("Data/localidad_formaps.dta"))
#dta$NOMBRE[dta$NOMBRE=="ANTONIO NARI?O"]<-"ANTONIO NARINO"
#dta$NOMBRE[dta$NOMBRE=="ANTONIO NARI?'O"]<-"ANTONIO NARINO"
#dta$NOMBRE[dta$NOMBRE=="LA CANDELARIA"]<-"CANDELARIA"
loc_data<-merge(rs,dta, by = "localidad")

ggplot(data=loc_data, aes( y=acumm_covid_covida, group=grp, col=grp, x = reorder(localidad, estrato_prom)))+
  geom_point(size=1, position=position_dodge(width = .2))+
  geom_errorbar(aes(ymin=q025, ymax=q975), width=.1, position=position_dodge(width = .2)) +
  xlab("Localities") +
  theme_bw() +
  scale_y_continuous("Accumulated SARS-COV-2 Cases \n as Percentage of Population",breaks =seq(0,100,20),limits=c(0,102)) +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=60,hjust=1,size=14),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white")
  ) + scale_color_manual(values=c("#3B4992B2","#EE0000B2"))
ggsave(paste0("views/Fig2_c_",name,".pdf"),height=7,width=9)


#Maps-------------------------------------------------------------------------------

#PATHS 
main = "C:/Users/cdelo/Dropbox/COVIDA"
localidades = paste(main,"/localidades", sep="") 
iceberg = paste(main, "/iceberg", sep="") 
loc_fig = ("C:/Users/cdelo/Dropbox/Iceberg Paper/views/Loclaidades_Maps")

# I like this projection better
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # ASUM? QUE ESTA ERA LA PROYECCI?N DE LOS COLEGIOS



#--------------IMPORTING FILES----------------------------------------

setwd(localidades)

#load and prepare the map 

localidades<- readOGR(dsn=".", layer="localidades_nsmpr") 
localidades<-clgeo_Clean(localidades)
table(localidades$NOMBRE)
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"Antonio Nari?o"
localidades$NOMBRE[localidades$NOMBRE=="Antonio Nari?o"]<-"Antonio Nari?o"
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"Antonio Nari?o"
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"Antonio Nari?o"
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"Antonio Nari?o"
localidades$NOMBRE[localidades$NOMBRE=="BARRIOS UNIDOS"]<-"Barrios Unidos"
localidades$NOMBRE[localidades$NOMBRE=="BOSA"]<-"Bosa"
localidades$NOMBRE[localidades$NOMBRE=="CANDELARIA"]<-"La Candelaria"
localidades$NOMBRE[localidades$NOMBRE=="CHAPINERO"]<-"Chapinero"
localidades$NOMBRE[localidades$NOMBRE=="CIUDAD BOLIVAR"]<-"Ciudad Bolivar"
localidades$NOMBRE[localidades$NOMBRE=="ENGATIVA"]<-"Engativa"
localidades$NOMBRE[localidades$NOMBRE=="FONTIBON"]<-"Fontibon"
localidades$NOMBRE[localidades$NOMBRE=="KENNEDY"]<-"Kennedy"
localidades$NOMBRE[localidades$NOMBRE=="LOS MARTIRES"]<-"Martires"
localidades$NOMBRE[localidades$NOMBRE=="PUENTE ARANDA"]<-"Puente Aranda"
localidades$NOMBRE[localidades$NOMBRE=="RAFAEL URIBE URIBE"]<-"Rafael Uribe Uribe"
localidades$NOMBRE[localidades$NOMBRE=="SAN CRISTOBAL"]<-"San Cristobal"
localidades$NOMBRE[localidades$NOMBRE=="SANTA FE"]<-"Santa Fe"
localidades$NOMBRE[localidades$NOMBRE=="SUBA"]<-"Suba"
localidades$NOMBRE[localidades$NOMBRE=="TEUSAQUILLO"]<-"Teusaquillo"
localidades$NOMBRE[localidades$NOMBRE=="TUNJUELITO"]<-"Tunjuelito"
localidades$NOMBRE[localidades$NOMBRE=="USAQUEN"]<-"Usaquen"
localidades$NOMBRE[localidades$NOMBRE=="USME"]<-"Usme"

#localidades<-spTransform(localidades,CRSobj = wgs.84)

# load the data. I created the vars in stata (its easier as I had the code ready)


localidades$localidad<-localidades$NOMBRE
table(localidades$localidad)
# Merge data 
rscam2<-rs %>% 
  select(localidad, acumm_covid_covida,grp) %>% 
  filter(grp == "March 3th")

rscam1<-rs %>% 
  select(localidad, acumm_covid_covida,grp) %>% 
  filter(grp == "November 30th")

loc_data1<-sp::merge(localidades, rscam1, by = 'localidad')
loc_data2<-sp::merge(localidades, rscam2, by = 'localidad')

sf_poly1 <- st_as_sf(loc_data1)
sf_poly2 <- st_as_sf(loc_data2)


setwd(loc_fig)

b<-max(sf_poly2$acumm_covid_covida)
b[b > 100] <- 100


p1<-ggplot(sf_poly1) +
  geom_sf(aes(fill = acumm_covid_covida), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="November 30th")
#    caption = "July - January ; CoVIDA Data")


table(sf_poly1$localidad)
table(sf_poly2$localidad)

#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly2) +
  geom_sf(aes(fill = acumm_covid_covida), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="March 30th")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("covida_all_new.pdf")
