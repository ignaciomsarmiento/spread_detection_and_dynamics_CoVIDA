##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo
#Load Packages
pkg<-list("dplyr","haven",'tidyr',"lubridate","broom","here","stringr","openxlsx")
lapply(pkg, require, character.only=T)
rm(pkg)

# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta"))  %>% filter(exclude_symptomatic==1)

dta_covida<-  dta_covida %>% 
  mutate(public_campaign=ifelse(invite_sample=="CAMPAÑA PUBLICA","public","private"))


dta_pub<-dta_covida %>%
        group_by(date_m,public_campaign) %>%
        tally() 


dta_pub_wide<-dta_pub %>%
          pivot_wider(names_from=public_campaign,values_from=n,values_fill=0) %>% 
          mutate(date_m=paste(month(date_m, label = TRUE, abbr = FALSE),year(date_m),sep=" "),
                 date_m=as.character(date_m))


dta_pub_t<-dta_covida %>%
  group_by(public_campaign) %>%
  tally()  %>%
  pivot_wider(names_from=public_campaign,values_from=n,values_fill=0)  %>% 
  mutate(date_m="Total")

dta_pub_wide<-bind_rows(dta_pub_wide,dta_pub_t)

dta_pub_wide<-dta_pub_wide %>%
  mutate(total=public+private,
         perc_public=public/total,
         perc_private=private/total,
         ) %>% 
  dplyr::select(date_m,private,public,total) %>% 
  mutate(
    private=formatC(private, format="f", big.mark=",", digits=0),
    public=formatC(public, format="f", big.mark=",", digits=0),
    total=formatC(total, format="f", big.mark=",", digits=0),
  )
dta_pub_wide

write.xlsx(dta_pub_wide,here("Results_tables/sample_evolution.xlsx"))
