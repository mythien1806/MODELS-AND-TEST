## ----setup, include=TRUE, message=FALSE, warning=FALSE----------------------------
knitr::opts_chunk$set(echo = TRUE)
library(PoEdata); library(xtable); library(knitr); library(lmtest); library(zoo);library(carData);library(sandwich);library(ggplot2);library(gridExtra);library(lmtest);library(readxl);library(car);library(stargazer);library(lattice);library(survival);library(Formula);library(nortest);library(foreign);library(nnet);library(reshape2);library(readxl); library(kableExtra); library(xtable); library(knitr); library("tidyr");library(tidyverse);library(broom);library(AER); library(systemfit); library(nnet); library(censReg); library(truncreg); library(dynlm);library(orcutt); library(nlWaldTest);library(pdfetch); library(forecast); library(TTR); library(tseries); library(plm)


## ---------------------------------------------------------------------------------
accidents<-read_excel("AccidentsTS.xls")


## ---------------------------------------------------------------------------------
total<-ts(accidents$Tot_Acc,start=c(1981,1),frequency=12)
plot.ts(total,main="Total accidents",ylab="Total accidents",xlab="Time")
totalcomponents<-decompose(total)
plot(totalcomponents)


## ---------------------------------------------------------------------------------
total.HW<- HoltWinters(total)
plot(total.HW)


## ---------------------------------------------------------------------------------
total.HW.forecast<-forecast(total.HW,3)
plot(total.HW.forecast)


## ---------------------------------------------------------------------------------
acf(total.HW.forecast$residuals, lag.max=20, na.action = na.contiguous)
plot.ts(total.HW.forecast$residuals, main="Residual after HoltWinters Forecasting for Total Accidents")
abline(h=0,lty=2)

Box.test(total.HW.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
acf(total, lag.max=20)
pacf(total, lag.max=20)
auto.arima(total)
total.ARIMA<-arima(total,order=c(2,1,0))
total.ARIMA.forecast<-forecast(total.ARIMA,3)
plot(total.ARIMA.forecast)
Box.test(total.ARIMA.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
total.ARIMA2<-arima(total,order=c(2,1,1))
total.ARIMA.forecast2<-forecast(total.ARIMA2,3)
plot(total.ARIMA.forecast2)
Box.test(total.ARIMA.forecast2$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
accuracy(total.HW.forecast)
accuracy(total.ARIMA.forecast)
accuracy(total.ARIMA.forecast2)


## ---------------------------------------------------------------------------------
total.HW.forecast


## ---------------------------------------------------------------------------------
fatal<-ts(accidents$Fat_Acc,start=c(1981,1),frequency=12)
plot.ts(fatal,main="Fatal Accidents",ylab="Fatal accidents",xlab="Time")


## ---------------------------------------------------------------------------------
fatalcomponents<-decompose(fatal)
plot(fatalcomponents)


## ---------------------------------------------------------------------------------
fatal.HW<- HoltWinters(fatal)
plot(fatal.HW)


## ---------------------------------------------------------------------------------
fatal.HW.forecast<-forecast(fatal.HW,3)
plot(fatal.HW.forecast)


## ---------------------------------------------------------------------------------
acf(fatal.HW.forecast$residuals, lag.max=20, na.action = na.contiguous)
plot.ts(fatal.HW.forecast$residuals, main="Residual after HoltWinters Forecasting For Fatal Accidents")
abline(h=0,lty=2)

Box.test(fatal.HW.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
acf(fatal, lag.max=20)
pacf(fatal, lag.max=20)
auto.arima(fatal)


## ---------------------------------------------------------------------------------
fatal.ARIMA<-arima(fatal,order=c(1,0,1))
fatal.ARIMA.forecast<-forecast(fatal.ARIMA,3)
plot(fatal.ARIMA.forecast)
Box.test(fatal.ARIMA.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
fatal.ARIMA2<-arima(fatal,order=c(2,1,0))
fatal.ARIMA.forecast2<-forecast(fatal.ARIMA2,3)
plot(fatal.ARIMA.forecast2)
Box.test(fatal.ARIMA.forecast2$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
accuracy(fatal.HW.forecast)
accuracy(fatal.ARIMA.forecast)
accuracy(fatal.ARIMA.forecast2)


## ---------------------------------------------------------------------------------
fatal.HW.forecast


## ---------------------------------------------------------------------------------
injury<-ts(accidents$Injury_Acc,start=c(1981,1),frequency=12)
plot.ts(injury,main="Injury Accidents",ylab="Injury accidents",xlab="Time")


## ---------------------------------------------------------------------------------
injurycomponents<-decompose(injury)
plot(injurycomponents)


## ---------------------------------------------------------------------------------
injury.HW<- HoltWinters(injury)
plot(injury.HW)


## ---------------------------------------------------------------------------------
injury.HW.forecast<-forecast(injury.HW,3)
plot(injury.HW.forecast)


## ---------------------------------------------------------------------------------
acf(injury.HW.forecast$residuals, lag.max=20, na.action = na.contiguous)
plot.ts(injury.HW.forecast$residuals, main="Residual after HoltWinters Forecasting For Injury Accidents")
abline(h=0,lty=2)

Box.test(injury.HW.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
acf(injury, lag.max=20)
pacf(injury, lag.max=20)
auto.arima(injury)


## ---------------------------------------------------------------------------------
injury.ARIMA<-arima(injury,order=c(3,0,0))
injury.ARIMA.forecast<-forecast(injury.ARIMA,3)
plot(injury.ARIMA.forecast)
Box.test(injury.ARIMA.forecast$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
injury.ARIMA2<-arima(injury,order=c(2,1,0))
injury.ARIMA.forecast2<-forecast(injury.ARIMA2,3)
plot(injury.ARIMA.forecast2)
Box.test(injury.ARIMA.forecast2$residuals,lag=20,type="Ljung-Box")


## ---------------------------------------------------------------------------------
accuracy(injury.HW.forecast)
accuracy(injury.ARIMA.forecast)
accuracy(injury.ARIMA.forecast2)


## ---------------------------------------------------------------------------------
injury.HW.forecast


## ---------------------------------------------------------------------------------
Dtotal<- diff(total)
Dfatal<- diff(fatal)
Dinjury<- diff(injury)

rate<-ts(accidents$UnEmploymentRate,start=c(1981,1),frequency=12)
Drate<-diff(rate)

speed<-ts(accidents$SpeedLaw,start=c(1981,1),frequency=12)
Dspeed<-diff(speed)

belt<-ts(accidents$BeltLaw,start=c(1981,1),frequency=12)
Dbelt<-diff(belt)

accidents.ts<-ts.union(total,Dtotal,fatal,Dfatal,injury,Dinjury,rate,Drate,speed,Dspeed,belt,Dbelt,dframe=T)

head(accidents.ts)


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(total,main="Total Accidents")
plot(Dtotal,main="1st Diff Total Accidents")


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(fatal,main="Fatal Accidents")
plot(Dfatal,main="1st Diff Fatal Accidents")


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(injury,main="Injury Accidents")
plot(Dinjury,main="1st Diff Injury Accidents")


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(rate,main="Unemployment rate")
plot(Drate,main="1st Diff Unemployment rate")


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(speed,main="Speed Law")
plot(Dspeed,main="1st Diff Speed Law")


## ---------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(belt,main="Belt Law")
plot(Dbelt,main="1st Diff Belt Law")


## ---------------------------------------------------------------------------------
adf.test(total)
adf.test(Dtotal)


## ---------------------------------------------------------------------------------
adf.test(fatal)
adf.test(Dfatal)


## ---------------------------------------------------------------------------------
adf.test(injury)
adf.test(Dinjury)


## ---------------------------------------------------------------------------------
adf.test(rate)
adf.test(Drate)


## ---------------------------------------------------------------------------------
adf.test(speed)
adf.test(Dspeed)


## ---------------------------------------------------------------------------------
adf.test(belt)
adf.test(Dbelt)


## ---------------------------------------------------------------------------------
total.model<-dynlm(Dtotal~Drate+Dspeed+Dbelt-1,data=accidents.ts)
summary(total.model)


## ---------------------------------------------------------------------------------
fatal.model<-dynlm(fatal~Drate+Dspeed+Dbelt,data=accidents.ts)
summary(fatal.model)


## ---------------------------------------------------------------------------------
injury.model<-dynlm(injury~Drate+Dspeed+Dbelt,data=accidents.ts)
summary(injury.model)


## ---------------------------------------------------------------------------------
minwage<-read_excel("MINWAGE.xlsx")


## ---------------------------------------------------------------------------------
lwage232<-ts(minwage$lwage232)
Dlwage232<- diff(lwage232)

par(mfrow=c(2,1))
plot(lwage232,main="lwage232")
plot(Dlwage232,main="1st Diff lwage232")

adf.test(lwage232)
adf.test(Dlwage232)


## ---------------------------------------------------------------------------------
lemp232<-ts(minwage$lemp232)
Dlemp232<- diff(lemp232)

par(mfrow=c(2,1))
plot(lemp232,main="lemp232")
plot(Dlemp232,main="1st Diff lemp232")

adf.test(lemp232)
adf.test(Dlemp232)


## ---------------------------------------------------------------------------------
reg1<-dynlm(Dlemp232~Dlwage232-1)
summary(reg1)

reg1.bfx<-as.matrix(cbind(lemp232,lwage232),demean=F)
po.test(reg1.bfx)


## ---------------------------------------------------------------------------------
t <- ts(minwage$t)

reg2<-dynlm(Dlemp232~Dlwage232+t)
summary(reg2)

reg2.bfx<-as.matrix(cbind(lemp232,lwage232,t),demean=F)
po.test(reg2.bfx)


## ---------------------------------------------------------------------------------
lrwage232<-ts(minwage$lwage232-minwage$lcpi)
plot.ts(lrwage232)


## ---------------------------------------------------------------------------------
adf.test(lrwage232)


## ---------------------------------------------------------------------------------
Dlrwage232<-diff(lrwage232)
plot.ts(Dlrwage232)

adf.test(Dlrwage232)


## ---------------------------------------------------------------------------------
bfx<-as.matrix(cbind(lemp232,lrwage232,t),demean=F)
po.test(bfx)


## ---------------------------------------------------------------------------------
reg3<-dynlm(Dlemp232~Dlrwage232+t)
summary(reg3)


## ---------------------------------------------------------------------------------
airfarepanel<-read_excel("AirfarePanel.xls")
airpanel <-pdata.frame(airfarepanel,index=c("id", "year"))
pdim(airpanel)


## ---------------------------------------------------------------------------------
mod1.pooled<-plm(lfare~concen+ldist+ldistsq+y98+y99+y00,model="pooling",data=airpanel)
summary(mod1.pooled)


## ---------------------------------------------------------------------------------
concen<-coef(mod1.pooled)[2]
concen


## ---------------------------------------------------------------------------------
mod1.pooled.RSE <- tidy(coeftest(mod1.pooled, vcov=vcovHC(mod1.pooled,type="HC0", cluster="group")))
kable(mod1.pooled.RSE, digits=3, caption= "Pooled Air Panel model with robust standard errors") 



## ---------------------------------------------------------------------------------
lowlimit<-mod1.pooled.RSE[2,2]-1.96*mod1.pooled.RSE[2,3]
uplimit <- mod1.pooled.RSE[2,2]+1.96*mod1.pooled.RSE[2,3]
paste('The confidence interval at 95% level of confidence is:',lowlimit,'and', uplimit)



## ---------------------------------------------------------------------------------
compare_r_p <- plmtest(mod1.pooled, effect="individual")
kable(tidy(compare_r_p ))


## ---------------------------------------------------------------------------------
mod1.random <- plm(lfare~concen+ldist+ldistsq+y98+y99+y00,model="random",random.method="swar",data=airpanel)
kable(tidy(mod1.random), digits=4)



## ---------------------------------------------------------------------------------
mod1.within <- plm(lfare~concen+ldist+ldistsq,data=airpanel,model="within")
kable(tidy(mod1.within), digits=4)


## ---------------------------------------------------------------------------------
kable(tidy(phtest(mod1.within, mod1.random)), caption= "Hausman endogeneity test for the random effects wage model")


## ---------------------------------------------------------------------------------
kable(tidy(pFtest(mod1.within, mod1.pooled)), digits=2,caption="Fixed effects test: Ho:'No fixed effects'")


## ---------------------------------------------------------------------------------
summary(mod1.within)

