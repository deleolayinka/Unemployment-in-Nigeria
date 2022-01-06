#Survival Analysis in R
#Copyright 2018 by Oyedele Moses Olayinka

install.packages("survival")
library(survival)

mydata<-read.csv("C:/McCOY/Mccoy/Projects/Masters/MScProject.csv")
attach(mydata)

#Define Variables
time<-Spell
event<-Event
X<-cbind(Age, Gender, MaritalStatus, HLEC, COD, Wage, PRVJOB, PRVJOBW)
group<-Gender

#Descriptive Statistics
summary(time)
summary(event)
summary(X)
summary(group)


#kaplan-Meier Non-Parametric Analysis 
kmsurvival<-survfit(Surv(time, event)~1)
summary(kmsurvival)
plot(kmsurvival, main="Kaplan-Meier Estimate", xlab="Time", ylab="Survival Probability")
windows()

#kaplan-Meier Non-Parametric Analysis by group
kmsurvival1<-survfit(Surv(time, event)~group)
summary(kmsurvival1)
plot(kmsurvival1, main="Kaplan-Meier Estimate by Gender", xlab="Time", ylab="Survival Probability", lty=1:2, col=c(2,4))
legend("topright", legend=c("Male", "Female"), lty=1:2, col=c(2,4))
fit<-survdiff(Surv(time, group)~event)
fit
windows()

#Nelson-Aalen Non-Parametric Analysis 
nasurvival<-survfit(coxph(Surv(time, event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, main="Nelson-Aalen Estimate", xlab="Time", ylab="Survival Probability")
windows()

#Nelson-Aalen Non-Parametric Analysis by group
nasurvival1<-survfit(coxph(Surv(time, event)~group), type="aalen")
summary(nasurvival1)
plot(nasurvival1, main="Nelson-Aalen Estimate by Gender", xlab="Time", ylab="Survival Probability", lty=1:2, col=c(2,4))
legend("topright", legend=c("Male", "Female"), lty=1:2, col=c(2,4))
fit<-survdiff(Surv(time, group)~event)
fit
windows()


#Cox Proportional Hazard Model - Coefficients and Hazard Rates
coxph<-coxph(Surv(time, event)~X, method="breslow")
summary(coxph)


#Exponential, Weibull, and Log-logistic Parametric Model Coefficients
exponential<-survreg(Surv(time, event)~X, dist="exponential")
summary(exponential)

weibull<-survreg(Surv(time, event)~X, dist="weibull")
summary(weibull)

loglogistic<-survreg(Surv(time, event)~X, dist="loglogistic")
summary(loglogistic)
