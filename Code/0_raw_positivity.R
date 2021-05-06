##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","haven",'tidyr',"lubridate","broom","here","stringr")
lapply(pkg, require, character.only=T)
rm(pkg)


data = "C:/Users/cdelo/Dropbox/Iceberg Paper/"
setwd(data)

# Parameters --------------------------------------------------------------
set.seed(101010) #seed
pop_bogota<-8044713
#days_fin<-as.numeric(dmy("03-03-2021")-dmy("01-06-2020"))
days_fin<-30*10 #days times months (june to march (10 months))
# SDS ---------------------------------------------------------------


# covida ------------------------------------------------------------------
dta_covida<-read_dta("Data/Datos_Salesforce_treated_feb19_clean.dta")

#dta_covida<-read_dta("Data/Data_CoVIDA.dta")



#Fix a couple of wronly coded dates
dta_covida<- dta_covida %>% 
  mutate(date_m=as.character(date_m),
         date_m_orig=date_m,
         date_m=ifelse(date_m=="2020-04-01","2020-06-01",date_m),
         date_m=ifelse(date_m=="2020-05-01","2020-06-01",date_m),
         #date_m=ifelse(date_m=="2021-03-01","2021-02-01",date_m),
         date_m=ymd(date_m))


dta_covida<- dta_covida %>% 
  mutate(obs=1) %>% 
  group_by(date_m) %>% 
  summarise(positive=sum(positive),
            obs=sum(obs)) %>% 
  mutate(percent=(positive/obs)*100)


dta_covida2<-dta_covida %>% 
  filter(date_m<"2021-03-01") 

ggplot(dta_covida2, aes(x = date_m, y = percent)) + geom_col(fill="grey")+
geom_text(aes(label=obs), size = 3, position=position_dodge(width=0.9), vjust=-0.25)+
  scale_x_date("", date_labels = "%b %Y",
               breaks = seq(as.Date("2020-06-01"),
                            as.Date("2021-02-01"), "1 month"),
               expand = c(0.01, 3))+
  ylab("Raw Positivy (Percent)")+
  theme_bw()+
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.text=element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=0.5,size=9),
        axis.text.y =element_text( size=12),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(1,1,1,1), "cm"))



ggsave("views/raw_positivity_months.pdf",height=5,width=7)














