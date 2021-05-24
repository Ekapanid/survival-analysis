

library(asaur)
library(survival)
library(survminer)

data("gastricXelox")

help("gastricXelox")



theme <- theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = "grey90"),
               panel.grid.minor = element_line(colour = "grey90"),
               panel.border = element_blank(),
               panel.background = element_blank()) 

str(gastricXelox)

kaplan_meier_fit<-survfit(Surv(timeWeeks,delta) ~ 1,data=gastricXelox,conf.type="plain")

summary(kaplan_meier_fit)


kap_plot<-ggsurvplot(kaplan_meier_fit,color="black",conf.int=TRUE,conf.int.style="step",ggtheme=theme)

kap_plot


nelson<-basehaz(coxph(Surv(timeWeeks,delta)~1,data=gastricXelox))



plot(nelson$time,nelson$hazard)


nelson_fit<- survfit(Surv(timeWeeks,delta) ~ 1, data = gastricXelox, 
               type = "fleming-harrington")

plot(nelson_fit$time,-log(nelson_fit$surv),type="s",ylab="Cumulative Hazard",xlab="time")


nelson_plot<-ggsurvplot(nelson_fit,color="red",conf.int=FALSE,ggtheme=theme)




fit=list(c11=kap_plot,c22=nelson_plot)
fit





