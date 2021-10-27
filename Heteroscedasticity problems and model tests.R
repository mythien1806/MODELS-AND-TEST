## ----setup, include=TRUE, warning=FALSE,message=FALSE----------------------------
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


## --------------------------------------------------------------------------------
birthwt1<-read_excel("BirthWt.xlsx")



## --------------------------------------------------------------------------------
birthwt<-select(birthwt1,'LogBW','Mother_CigPerDay','Order','Male','FatherEducation','MotherEducation','FamilyIncome')



## --------------------------------------------------------------------------------
correlation.matrix<-cor(birthwt)
correlation.matrix


## --------------------------------------------------------------------------------
OLS<-lm(LogBW~Mother_CigPerDay+Order+Male,data=birthwt)

stargazer(OLS,digits=3, type="text")


## --------------------------------------------------------------------------------
BW_iv<-ivreg(LogBW~Mother_CigPerDay+Order+Male|MotherEducation+Order+Male,data=birthwt)
stargazer(BW_iv,OLS,title="OLS vs IV", header=F, digits=3,type="text",
          dep.var.caption = "Dependent variable: BirthWeight")


## --------------------------------------------------------------------------------
summary(BW_iv,df=Inf,diagnostics=T)


## --------------------------------------------------------------------------------
BW_iv2<-ivreg(LogBW~Mother_CigPerDay+Order+Male|MotherEducation+FatherEducation+FamilyIncome+Order+Male,data=birthwt)
stargazer(BW_iv2,BW_iv,OLS,title="OLS vs IV", header=F, digits=3,type="text",
          dep.var.caption = "Dependent variable: BirthWeight")


## --------------------------------------------------------------------------------
summary(BW_iv2,df=Inf,diagnostics=T)


## --------------------------------------------------------------------------------
summary(BW_iv2,df=Inf,diagnostics=T)


## --------------------------------------------------------------------------------
tradeopen<-read_excel("OpenInflPCInc.xlsx")
correlation.matrix<-cor(tradeopen)
kable(correlation.matrix)


## --------------------------------------------------------------------------------
pcinc_iv <- ivreg(lpcinc~open|oil+land,data=tradeopen)
summary(pcinc_iv,df=Inf,diagnostics=T)


## --------------------------------------------------------------------------------
pcinc_OLS<-lm(lpcinc~open,data=tradeopen)
stargazer(pcinc_iv,pcinc_OLS,digits=3,type="text") 


## --------------------------------------------------------------------------------
inf_iv <-ivreg(linf~open|oil+land,data=tradeopen)
summary(inf_iv,df=Inf,diagnostics=T)


## --------------------------------------------------------------------------------
inf_OLS<-lm(linf~open,data=tradeopen)
stargazer(inf_iv,inf_OLS,digits=3,type="text") 


## --------------------------------------------------------------------------------
pcinc<-lpcinc~open
inf<-linf~open
open<-open~lpcinc+linf+oil+land
open_eqs<-list(pcinc,inf,open)
open_ivs <- ~oil+land
open2SLS <-systemfit(open_eqs,method="2SLS",inst=open_ivs,data=tradeopen)
summary(open2SLS)


## --------------------------------------------------------------------------------
open3SLS<-systemfit(open_eqs,method="3SLS",inst=open_ivs,data=tradeopen)
summary(open3SLS)

