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
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta"))

dta_covida<- dta_covida %>% mutate(gender=ifelse(is.na(gender) | gender=="No deseo responder" | gender=="","no_info",gender))

rates <-broom::tidy(lm(positive~as.factor(gender)-1,dta_covida ,weights = weight_ocup), conf.int = TRUE) 

rates <-rates %>%   mutate(term=str_remove_all(term,"as.factor\\(gender\\)")) %>% 
  mutate(gender=term,
         rate_pos=estimate*100,
         q025=conf.low*100,
         q975=conf.high*100)  %>% 
  select(gender,rate_pos,q025,q975) 

rates <-rates %>%  
  mutate(rate_pos=formatC(rate_pos, format="f", big.mark=",", digits=2),
         q025=formatC(q025, format="f", big.mark=",", digits=2),
         q975=formatC(q975, format="f", big.mark=",", digits=2),
         ci=paste0("(",q025,",", q975,")")) %>% 
  select(gender,rate_pos,ci)

obs<-  dta_covida %>% 
  group_by(gender) %>% 
  dplyr::summarise(Obs=formatC(n(), format="f", big.mark=",", digits=0),
                   .groups="drop") %>% 
  mutate(gender=ifelse(is.na(gender) | gender=="","no_info",gender))


rates<- rates %>% 
  left_join(.,obs) %>% 
  select(gender,rate_pos,ci,Obs)

rates

write.xlsx(rates,here("Results_tables/gender.xlsx"))



