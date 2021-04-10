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



# Load data --------------------------------------------------------------
load("Data/temp/Fig1_calculations.RData")


#monthly cases
covida_table<-rates %>% 
              select(date_m,sds_100)  %>% 
              left_join(.,gg_covida) %>% 
              select(-grp) %>% 
              na.omit()

detected_month<- covida_table %>% 
                  mutate(detected=covida_100/sds_100,
                         q025_detected=q025_covida_100/sds_100,
                         q975_detected=q975_covida_100/sds_100,
                         month=month(date_m, label = TRUE, abbr = FALSE)
                         ) %>% 
                  select(month,detected,q025_detected,q975_detected)


openxlsx::write.xlsx(detected_month,"Results_tables/monthly_Fig1a.xlsx")
# # -----------------------------------------------------------------------
# Cumulative cases -------------------------------------------------------------
# # -----------------------------------------------------------------------

accum_gg_sds<-accum_gg_sds %>% 
              select(-grp,-q025,-q975) %>% 
              rename(accum_sds=accum_covida)
accum_detected_month<- accum_gg_covida %>% 
                       filter(date_m>=as.Date("2020-06-01")) %>% 
                       select(-grp) %>% 
                       left_join(accum_gg_sds) %>% 
                      mutate(detected=accum_covida/accum_sds,
                             q025_detected=q025/accum_sds,
                             q975_detected=q975/accum_sds,
                             month=month(date_m, label = TRUE, abbr = FALSE)
                              ) %>% 
                              select(month,detected,q025_detected,q975_detected)

openxlsx::write.xlsx(accum_detected_month,"Results_tables/accumulated_Fig1b.xlsx")
