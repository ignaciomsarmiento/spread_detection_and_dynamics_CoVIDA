##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","openxlsx","haven","tidyr","here","stargazer")
lapply(pkg, require, character.only=T)
rm(pkg)



# covida ------------------------------------------------------------------
dta_covida<-read_dta(here("Data/Data_CoVIDA.dta"))

dta_covida<- dta_covida %>% mutate(positive=positive*100)

#all observations------------
no_weight<-lm(positive~1,dta_covida)
weight<-lm(positive~1,dta_covida,weights = weight_ocup)


stargazer(weight,no_weight,align = TRUE, 
          no.space = TRUE, ci = TRUE, digits=2,
          report = ('vcs'), type="text",
          column.labels = c("Weighted", "Non-Weighted"),
          covariate.labels = c("Full Sample"))


#No symptomatic------------
no_weight_no_sym<-lm(positive~1,dta_covida %>% 
                    filter(exclude_symptomatic==1))
weight_no_sym<-lm(positive~1,dta_covida %>% 
                 filter(exclude_symptomatic==1),weights = weight_ocup)



#Only symptomatic observations------------



#No symptomatic------------
no_weight_sym<-lm(positive~1,dta_covida %>% 
                       filter(exclude_symptomatic==0))
weight_sym<-lm(positive~1,dta_covida %>% 
                    filter(exclude_symptomatic==0),weights = weight_ocup)



#No symptomatic Invited Lists------------
no_weight_no_sym_inv<-lm(positive~1,dta_covida %>% 
                       filter(exclude_symptomatic==1, invite_sample!="CAMPAÑA PUBLICA"))
weight_no_sym_inv<-lm(positive~1,dta_covida %>% 
                    filter(exclude_symptomatic==1, invite_sample!="CAMPAÑA PUBLICA"),weights = weight_ocup)





#No symptomatic Public Campaign ------------
no_weight_no_sym_public<-lm(positive~1,dta_covida %>% 
                           filter(exclude_symptomatic==1, invite_sample=="CAMPAÑA PUBLICA"))
weight_no_sym_public<-lm(positive~1,dta_covida %>% 
                        filter(exclude_symptomatic==1, invite_sample=="CAMPAÑA PUBLICA"),weights = weight_ocup)






stargazer(weight_no_sym,no_weight_no_sym,align = TRUE, 
          no.space = TRUE, ci = TRUE, digits=2,
          report = ('vcs'), type="text",
          column.labels = c("Weighted", "Non-Weighted"),
          covariate.labels = c("Excluding symptomatic and/or known contact"))



stargazer(weight_sym,no_weight_sym,align = TRUE, 
          no.space = TRUE, ci = TRUE, digits=2,
          report = ('vcs'), type="text",
          column.labels = c("Weighted", "Non-Weighted"),
          covariate.labels = c("Only symptomatic and/or known contact"))


stargazer(weight_no_sym_inv,no_weight_no_sym_inv,align = TRUE, 
          no.space = TRUE, ci = TRUE, digits=2,
          report = ('vcs'), type="text",
          column.labels = c("Weighted", "Non-Weighted"),
          covariate.labels = c("Participants that we invited (from lists)"))


stargazer(weight_no_sym_public,no_weight_no_sym_public,align = TRUE, 
          no.space = TRUE, ci = TRUE, digits=2,
          report = ('vcs'), type="text",
          column.labels = c("Weighted", "Non-Weighted"),
          covariate.labels = c("Participants from public campaign"))
