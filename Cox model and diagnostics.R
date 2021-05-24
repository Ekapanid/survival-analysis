


install.packages("fastDummies")


library(fastDummies)

library(survminer)

library(survival)

library(KMsurv)


library(dplyr)

library(survMisc)

data("pharmacoSmoking")

str(pharmacoSmoking)


data(larynx)

fit<-survfit(Surv(time,delta)~1,data=larynx)
ggsurvplot(fit)


str(larynx)

larynx$stage<-as.factor(larynx$stage)


# simple backward selection of variables 

datalar<-dummy_cols(larynx, select_columns = "stage")

cox_mod3<-coxph(Surv(time,delta)~stage_1+stage_2+stage_3+age,data=datalar)

#testing for the proportionality of hazards assumptions
test.ph <- cox.zph(cox_mod3)
test.ph

ggcoxzph(test.ph)
#No problem regarding the assumptions (p-value above 0.05) 

summary(cox_mod3)

#Age seems to be insignificant

cox_mod4<-coxph(Surv(time,delta)~stage_1+stage_2+stage_3,data=datalar)

test.ph <- cox.zph(cox_mod4)
test.ph
#Again, no problem with the assumption 

summary(cox_mod4)


 

################ Martingale Examples ################################ 
#We present two examples of how to use martingale residuals to find the correct functional form of a covariate. The method
#applied in the following examples is based on Therneau's book "Modeling survival data: Extending the Cox model"

# Visualization of martingale residuals for the larynx data. Considering that the "age" variable is kept in the model, what is
# its best functional form ?

cox_mod<-coxph(Surv(time,delta)~1,data=larynx)


larynx$res<- residuals(cox_mod, type="martingale",data=larynx)


plot(larynx$age,larynx$res, xlab="Age",ylab="Martingale Residuals",ylim=c(-1,1),xlim=c(45,90))
lines(lowess(larynx$age,larynx$res))

#The plot suggests the age squared is a better functional form. 

#For the sake of another interesting example, consider that we have the same dataset but the patients are only in stage 1

lar_strat<-larynx%>%
  filter(stage==1)

cox_mod_str_null<-coxph(Surv(time,delta)~1,data=lar_strat)
lar_strat$res<- residuals(cox_mod_str_null, type="martingale",data=lar_strat)

plot(lar_strat$age,lar_strat$res, xlab="Age",ylim=c(-1,1),xlim=c(45,90))
lines(lowess(lar_strat$age,lar_strat$res))


#In this case it seems that the best functional form of the age variable is dichotomizing the variable. This is implied by
#the martingale plot. It is however not implied that the optimal cutpoint is the point just below 70.


#In order to find the optimal cutpoint O' Quigley's method is applied.
cox_mod_str<-coxph(Surv(time,delta)~age,data=lar_strat)

print(cutp(cox_mod_str))

#Interpreting this table shows that a cutpoint of 75 is the most suitable.


#### Cox-Snell residuals ###############################################3

#After fitting a model on data, Cox-Snell residuals help assess the goodness of fit of the model. Using R this test can be visualized and
#applied as below

#Model 3
cox_mod3$coefficients
#Extracting martingale residuals of the cox model 3
larynx$res3<- residuals(cox_mod3, type="martingale",data=datalar)

#This is a well known theoretical formula for Cox-Snell Residuals
larynx$csres<-larynx$delta-larynx$res3

#Computing the Cox-Snell plot 
surv3<-survfit(Surv(csres,delta)~1,type="fleming-harrington",data=larynx)

summary(surv3)

plot(surv3$time,-log(surv3$surv),type="s",ylab="Cumulative Hazard of CS-Residuals",xlab="CS-Residuals")
abline(a=0,b=1)


cox_mod4$coefficients


#Comparing the previous plot with another plot when age (Wald test has shown that it is insignificant) is kept in the model
larynx$res<- residuals(cox_mod4, type="martingale",data=datalar)


larynx$csres<-larynx$delta-larynx$res

surv<-survfit(Surv(csres,delta)~1,type="fleming-harrington",data=larynx)

summary(surv)

plot(surv$time,-log(surv$surv),type="s",ylab="Cumulative Hazard of CS-Residuals",xlab="CS-Residuals")
abline(a=0,b=1)

#It seems that this plot indicates a better fit even though age was insignificant
