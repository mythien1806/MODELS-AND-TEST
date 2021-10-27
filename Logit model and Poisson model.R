## ----setup, include=TRUE, message=FALSE, warning=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(kableExtra)
library(readr)
library(readxl)
library(lmtest)
library(stargazer)
library(car)
library(broom)
library(ivreg)
library(systemfit)
library(mfx)
library(AER)


## -----------------------------------------------------------------------------------
organic<-read_excel("OrganicDecision.xlsx")
organic<-rename(organic,"Purchaser"="Organic Food Purchaser","Subscriber"="Online Health Wellness e-Newsletters Subscriber")
organic


## -----------------------------------------------------------------------------------
correlation.matrix<-cor(organic)
kable(correlation.matrix)


## -----------------------------------------------------------------------------------
organic_glm<-glm(Purchaser~Age+Subscriber,family=binomial(link="logit"),data=organic)
summary(organic_glm)


## -----------------------------------------------------------------------------------
logitmfx(Purchaser~Age+Subscriber,data=organic,atmean=TRUE)


## -----------------------------------------------------------------------------------
logitmfx(Purchaser~Age+Subscriber,data=organic,atmean=FALSE)


## -----------------------------------------------------------------------------------
lrtest(organic_glm)


## -----------------------------------------------------------------------------------
stargazer(organic_glm,type='text')


## -----------------------------------------------------------------------------------
table<-data.frame(table(true=organic$Purchaser, 
              predicted=round(fitted(organic_glm))))
kable(table, align='c', caption="Logit Confusion Matrix")


## -----------------------------------------------------------------------------------
awards<-read_excel("Awards.xlsx")
awards_dummy<- fastDummies::dummy_cols(awards,select_columns='prog')


## -----------------------------------------------------------------------------------
awards.OLS <-lm(num_awards~math+prog_Arts+prog_Business,data=awards_dummy)
ehat<-awards.OLS$residuals
plot(awards_dummy$num_awards,ehat,xlab="Awards",ylab="Residuals",col=4)


## -----------------------------------------------------------------------------------
poisson1<-glm(num_awards~math+prog_Arts+prog_Business,family="poisson",na.action=na.omit,data=awards_dummy)
kable(tidy(poisson1),digits=4,caption="Poisson model 1")


## -----------------------------------------------------------------------------------
dispersiontest(poisson1)


## -----------------------------------------------------------------------------------
poisson2<-glm(num_awards~math+prog_Arts+prog_Business,family="quasipoisson",na.action=na.omit,data=awards_dummy)
kable(tidy(poisson2),digits=4, align='c',caption="Poisson model 2")


## -----------------------------------------------------------------------------------
stargazer(awards.OLS,poisson1,poisson2,type="text",keep.stat="n",intercept.bottom=FALSE)

