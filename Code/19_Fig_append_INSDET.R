##########################################################
# author: CDLR
##########################################################


#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","ggplot2","here")
lapply(pkg, require, character.only=T)
rm(pkg)



INS<-read.xlsx(here("Data/INS_detected.xlsx"))

INS<-INS %>% select(upper, lower, pib, city, fecha, detected, seropositiva_ajustada, lwb, upb)

INS$daten<-str_sub(INS$fecha, start=3)
INS$limit<-31000


ggplot(data=INS, aes(x=pib, y=detected, group=city, col=city))+
  geom_point(size=1, position=position_dodge(width = .2))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, position=position_dodge(width = .2)) +
  ylab("% Detected") +
  xlab("GDP Per Capita (Thousands COP)") +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="none",
        axis.title = element_text(size=10),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=1),
        rect = element_rect(colour = "transparent", fill = "white")
  ) + scale_color_manual(values=c("#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2"))+
  geom_text(aes(label=city),hjust=-0.1, vjust=-0.8, size= 4, color="black")+
  geom_text(aes(label=daten),hjust=-0.1, vjust=0.4, size =3, color="#3B4992B2")+
  expand_limits(x = 31000)
ggsave(here("views/INS_detected2.pdf"),height=7,width=10)

INS$serop<-(INS$seropositiva_ajustada)*100
INS$lb<-(INS$lwb)*100
INS$ub<-(INS$upb)*100


ggplot(data=INS, aes(x=pib, y=serop, group=city, col=city))+
  geom_point(size=1, position=position_dodge(width = .2))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1, position=position_dodge(width = .2)) +
  ylab("Seropositivity in %") +
  xlab("GDP Per Capita (Thousands COP)") +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="none",
        axis.title = element_text(size=10),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0,hjust=1),
        rect = element_rect(colour = "transparent", fill = "white")
  ) + scale_color_manual(values=c("#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2","#EE0000B2"))+
  geom_text(aes(label=city),hjust=-0.1, vjust=-0.8, size= 4, color="black")+
  geom_text(aes(label=daten),hjust=-0.1, vjust=0.4, size =3, color="#3B4992B2")+
  expand_limits(x = 31000)
  
ggsave(here("views/INS_seropositive.pdf"),height=7,width=10)
