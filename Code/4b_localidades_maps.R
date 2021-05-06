#load packages
z<-c("ggpubr","cleangeo","ggplot2","rgdal","haven","dplyr","broom","sf")
#ggplot2 or tidyverse
#z<-c("cleangeo'","ggplot2","ggmap","dplyr", "sf", "sp","rgdal","maptools","rgeos","raster","sf","raster","stars","maps","geosphere","foreign","tidyverse","xlsx","plyr","cleangeo","hablar","haven","dplyr","maps","viridis","cartogram","tidyverse","broom")
lapply(z, library, character.only = TRUE) 



#PATHS 
#main = "C:/Users/cdelo/Dropbox/COVIDA"
main = "C:/Users/cdelo/Dropbox/Iceberg Paper/Data"
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

localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARIÃ'O"]<-"ANTONIO NARINO"
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARIÃ'O"]<-"ANTONIO NARINO"
localidades$NOMBRE[localidades$NOMBRE=="LOS MARTIRES"]<-"MARTIRES"

#localidades<-spTransform(localidades,CRSobj = wgs.84)

# load the data. I created the vars in stata (its easier as I had the code ready)

#setwd(iceberg)

dta<-read_dta("localidad_formaps.dta")
dta$NOMBRE[dta$NOMBRE=="ANTONIO NARIñO"]<-"ANTONIO NARINO"
dta$NOMBRE[dta$NOMBRE=="ANTONIO NARIÃ'O"]<-"ANTONIO NARINO"
dta$NOMBRE[dta$NOMBRE=="LA CANDELARIA"]<-"CANDELARIA"


# Merge data 

loc_data<-merge(localidades,dta, by = "NOMBRE")

#loc_data_st<-fortify(loc_data, region="NOMBRE")
#names(loc_data_st)[names(loc_data_st) == "id"] <- "NOMBRE"
#loc_data_st<-merge(loc_data_st,dta, by= "NOMBRE")

sf_poly <- as(loc_data, "sf")

setwd(loc_fig)

# First only the mean socioeconomic stratum


ggplot(sf_poly) +
  geom_sf(aes(fill = estrato_prom), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="")+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
#  theme(plot.title=element_text(size=10, hjust=0.5, face="bold", colour="black",vjust=-1)) +
 # theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #labs(title="Mean Socioeconomic Stratum",
   #    subtitle="by Population")

ggsave("stratum.pdf")

b<-max(sf_poly$acumm_covid_covida_jul_jan)
b[b > 1] <- 1


p1<-ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_covida_jul_oct), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="July - October")
#    caption = "July - January ; CoVIDA Data")



#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_covida_jul_jan), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="July - January")
   #    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("covida_all.pdf")


b<-max(sf_poly$acumm_covid_sds_jul_jan_noadj)
b[b > 1] <- 1


p1<-ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_oct_noadj), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="July - October")
#    caption = "July - January ; CoVIDA Data")



#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_jan_noadj), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="July - January")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("sds_all.pdf")



#--DEPRECTED-----
b<-max(sf_poly$acumm_covid_sds_jul_jan)
b[b > 1] <- 1


ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_oct)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
  #     caption = "July - October ; SDS Data ; % detected by locality to extrapolate")

ggsave("sds_jul_oct_1.png")


ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_jan)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
  #     caption = "July - January ; SDS Data ; % detected by locality to extrapolate")

ggsave("sds_jul_jan_1.png")

b<-max(sf_poly$acumm_covid_sds_jul_jan_tot)
b[b > 1] <- 1

ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_oct_tot)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
  #     caption = "July - October ; SDS Data ; % detected aggregated to extrapolate")

ggsave("sds_jul_oct_2.png")



ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_jan_tot)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
  #     caption = "July - January ; SDS Data ; % detected aggregated to extrapolate")

ggsave("sds_jul_jan_2.png")

b<-max(sf_poly$acumm_covid_sds_jul_jan2)
b[b > 1] <- 1

ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_oct2)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
   #    caption = "July - October ; SDS Data ; % detected adjusted by stratum in each locality to extrapolate")

ggsave("sds_jul_oct_3.png")

ggplot(sf_poly) +
  geom_sf(aes(fill = acumm_covid_sds_jul_jan2)) +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="",limits=c(0,b), 
                        oob = scales::squish)+
  theme_void()
#+
 # theme(plot.title = element_text(hjust = 0.5))+
  
  #theme(plot.title=element_text(size=10, hjust=0.6, face="bold", colour="black",vjust=-1)) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  #labs(title="Accumulated Cases as % of Population ",
   #    caption = "July - January ; SDS Data ; % detected adjusted by stratum in each locality to extrapolate")

ggsave("sds_jul_jan_3.png")


