##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("ggpubr","ggplot2","haven","dplyr","broom","sf")
lapply(pkg, require, character.only=T)
rm(pkg)



# I like this projection better
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # ASUM? QUE ESTA ERA LA PROYECCI?N DE LOS COLEGIOS


#--------------IMPORTING FILES----------------------------------------

setwd(localidades)

#load and prepare the map 

localidades<- read_sf(here("Data/localidades/localidades_nsmp.shp"))
#localidades<-clgeo_Clean(localidades)

localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"ANTONIO NARINO"
localidades$NOMBRE[localidades$NOMBRE=="ANTONIO NARI?'O"]<-"ANTONIO NARINO"
localidades$NOMBRE[localidades$NOMBRE=="LOS MARTIRES"]<-"MARTIRES"


dta<-read_dta(here("Data/localidades/localidad_formaps.dta")) 
dta$NOMBRE[dta$NOMBRE=="ANTONIO NARI?O"]<-"ANTONIO NARINO"
dta$NOMBRE[dta$NOMBRE=="ANTONIO NARI?'O"]<-"ANTONIO NARINO"
dta$NOMBRE[dta$NOMBRE=="LA CANDELARIA"]<-"CANDELARIA"
dta<- dta %>% select(NOMBRE,localidad,estrato_prom)

# Merge data 
loc_data<-merge(localidades,dta, by = "NOMBRE")




# Mean socioeconomic stratum ----------------------------------------------
ggplot(loc_data) +
  geom_sf(aes(fill = estrato_prom), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="")+
  theme_void()
ggsave(here("views/stratum.pdf"))

# Accumulated Cases -------------------------------------------------------

loc_calc<-readRDS(here("Data/temp/calculations_locality.rds"))


loc_data<-left_join(loc_data,loc_calc) %>%
          filter(!is.na(acumm_covid_covida))


ggplot(loc_data ) +
  geom_sf(aes(fill = acumm_covid_covida), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  facet_wrap(. ~ grp) +
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black"),
        panel.spacing = unit(8, "lines"))  
ggsave(here("views/raw_positivity_map.pdf"))
