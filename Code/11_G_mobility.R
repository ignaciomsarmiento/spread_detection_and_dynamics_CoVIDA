##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","stringr","openxlsx","lubridate",'tidyr','rsample','purrr')
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Research/Covid_los_andes/Iceberg Paper/")




google<-read.csv("Data/Region_Mobility_Report_CSVs/2020_CO_Region_Mobility_Report.csv")
google<- google %>% filter(sub_region_1=="Bogota")
google<- google %>% mutate(date=ymd(date),
                           month=month(date))

google_retail<- google %>% dplyr::select(date,
                                         retail_and_recreation_percent_change_from_baseline,
                                         grocery_and_pharmacy_percent_change_from_baseline, 
                                         parks_percent_change_from_baseline,                
                                         transit_stations_percent_change_from_baseline,     
                                         workplaces_percent_change_from_baseline,           
                                         residential_percent_change_from_baseline)




google_long<- google_retail %>% gather(grp, percent_change_from_baseline, retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline, factor_key=TRUE)

google_long<- google_long %>% mutate(grp=factor(grp, levels=c("retail_and_recreation_percent_change_from_baseline",
                                          "grocery_and_pharmacy_percent_change_from_baseline", 
                                          "parks_percent_change_from_baseline",                
                                          "transit_stations_percent_change_from_baseline",     
                                          'workplaces_percent_change_from_baseline',           
                                          "residential_percent_change_from_baseline"),
                                          labels=c("Retail and Recreation",
                                                   "Grocery and Pharmacy",
                                                   "Parks",
                                                   "Transit Stations",
                                                   "Workplaces",
                                                   "Residential")))



#
ggplot(google_long) +
  geom_line(aes(x=date,y=percent_change_from_baseline,group=grp,col=grp)) +
  ylab("Percent Change from Baseline") +
  xlab("") +
  facet_wrap(.~grp,ncol=2) +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="none",
        #legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0),
        rect = element_rect(colour = "transparent", fill = "white"),
        axis.title = element_text(), plot.margin = unit(c(2,2,1,1), "lines"))
ggsave(paste0("views/google_mobility.pdf"),height=9,width=8)




  