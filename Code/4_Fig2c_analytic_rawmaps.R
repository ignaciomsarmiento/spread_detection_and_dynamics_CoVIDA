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

setwd('C:/Users/cdelo/Dropbox/Iceberg Paper/')


# Parameters --------------------------------------------------------------
set.seed(101010) #seed
name<-"analytic"
#days_oct<-as.numeric(dmy("30-11-2020")-dmy("01-06-2020"))
days_oct<-30*5
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*10

# covida ------------------------------------------------------------------
dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")
dta_covida<- dta_covida %>%
  mutate(localidad=localidadderesidencianombredeloc)# este paso no debería existir pero me dí cuenta que el dta covida tiene los datos actualizados. Imagino
# que ignacio los actualizó




poblacion<-read_dta("Data/pob_loc.dta") %>% 
  rename(poblacion_agregada=pob_loc)




# Calculate rates by Localidad --------------------------------------------

#June November


rates_oct <-dta_covida %>% 
  filter(mes>4 & mes<12) %>% 
  group_by(localidad) %>% 
  summarise(w_positive = weighted.mean(positive,weight_ocup),
            positive = mean(positive)) %>% 
  mutate(w_positive=w_positive*100,
         positive=positive*100,
         grp=1)

rates_jan <-dta_covida %>% 
  group_by(localidad) %>%
  summarise(w_positive = weighted.mean(positive,weight_ocup),
            positive = mean(positive)) %>% 
  mutate(w_positive=w_positive*100,
         positive=positive*100,
         grp=2)


rs<-bind_rows(rates_oct,rates_jan)
rs<- rs %>% mutate(grp=factor(grp, levels=c(1,2),  labels=c("November 30th","March 3th"),ordered = TRUE))


#Maps-------------------------------------------------------------------------------

#PATHS 
#setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")
#setwd("C:/Users/cdelo/Dropbox/Iceberg Paper/")
main = "C:/Users/cdelo/Dropbox/Iceberg Paper/Data"
localidades = paste(main,"/localidades", sep="") 
#iceberg = paste(main, "/iceberg", sep="") 
loc_fig = ("C:/Users/cdelo/Dropbox/Iceberg Paper/views/")

# I like this projection better
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # ASUM? QUE ESTA ERA LA PROYECCI?N DE LOS COLEGIOS



#--------------IMPORTING FILES----------------------------------------

setwd(localidades)

#load and prepare the map 

localidades<- readOGR(dsn=".", layer="localidades_nsmpr") 
localidades<-clgeo_Clean(localidades)
table(localidades$NOMBRE)
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARIÃ'O"]<-"Antonio Nariño"
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
  select(localidad, positive, w_positive,grp) %>% 
  filter(grp == "March 3th")

rscam1<-rs %>% 
  select(localidad,  positive, w_positive,grp) %>% 
  filter(grp == "November 30th")

loc_data1<-sp::merge(localidades, rscam1, by = 'localidad')
loc_data2<-sp::merge(localidades, rscam2, by = 'localidad')

sf_poly1 <- st_as_sf(loc_data1)
sf_poly2 <- st_as_sf(loc_data2)


setwd(loc_fig)



p1<-ggplot(sf_poly1) +
  geom_sf(aes(fill = positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="November 30th")
#    caption = "July - January ; CoVIDA Data")


#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly2) +
  geom_sf(aes(fill = positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="March 3th")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("raw_positivity_map.pdf")

p1<-ggplot(sf_poly1) +
  geom_sf(aes(fill = w_positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="November 30th")
#    caption = "July - January ; CoVIDA Data")


#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly2) +
  geom_sf(aes(fill = w_positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="March 3th")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("raw_weighted_positivity_map.pdf")