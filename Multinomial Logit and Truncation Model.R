## ----setup, include=TRUE,message=FALSE,warning=FALSE--------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(PoEdata); library(xtable); library(knitr); library(lmtest); library(zoo);library(carData);library(sandwich);library(ggplot2);library(gridExtra);library(lmtest);library(readxl);library(car);library(stargazer);library(lattice);library(survival);library(Formula);library(nortest);library(foreign);library(nnet);library(reshape2);library(readxl); library(kableExtra); library(xtable); library(knitr); library("tidyr");library(tidyverse);library(broom);library(AER); library(systemfit); library(nnet); library(censReg); library(truncreg); library(dynlm);library(orcutt); library(nlWaldTest);library(pdfetch); library(forecast); library(TTR); library(tseries); library(censReg)



## -----------------------------------------------------------------------------------
heating<-read_csv("Heating.csv")


## -----------------------------------------------------------------------------------
heat_multi<-multinom(depvar~income+rooms+agehed,data=heating)
summary(heat_multi)


## -----------------------------------------------------------------------------------
z<-summary(heat_multi)$coefficients/summary(heat_multi)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
paste("The following are the p-values for the multinomial regression coefficients")
p



## -----------------------------------------------------------------------------------
medincome<-quantile(heating$income,.5)
medroom<-quantile(heating$rooms,.5)
medagehed<-quantile(heating$agehed,.5)

newdat<-data.frame(income=c(medincome),rooms=c(medroom),agehed=c(medagehed))
pred<-predict(heat_multi,newdat,"probs")
pred


## -----------------------------------------------------------------------------------
htv<-read_excel("HTV.xlsx")


## -----------------------------------------------------------------------------------
htv.ols<-lm(lwage~educ+abil+exper+nc+west+south+urban,data = htv)
summary(htv.ols)


## -----------------------------------------------------------------------------------
updatehtv<-mutate(htv, newwage=ifelse(wage>=20,20,wage))
updatehtv


## -----------------------------------------------------------------------------------
htv.ols2<-lm(log(newwage)~educ+abil+exper+nc+west+south+urban,data = updatehtv)
summary(htv.ols2)


## -----------------------------------------------------------------------------------
htv.censored<-censReg(log(wage)~educ+abil+exper+nc+west+south+urban,right=log(20),data=htv)
summary(htv.censored)


## -----------------------------------------------------------------------------------
newhtv<-filter(htv,wage<20)
newhtv

htv.ols3<-lm(lwage~educ+abil+exper+nc+west+south+urban,data = newhtv)
summary(htv.ols3)


## -----------------------------------------------------------------------------------
htv.truncated<-truncreg(lwage~educ+abil+exper+nc+west+south+urban,data=newhtv,point=log(20),direction="right")
summary(htv.truncated)


## -----------------------------------------------------------------------------------
metalprice<-read_excel("MetalPrice.xlsx")


## -----------------------------------------------------------------------------------
price<-metalprice[,2]
price.ts<-ts(price,frequency=4, start=c(2004,1))
plot.ts(price.ts,main="Metal Price in city",xlab="Time",ylab="Price")



## -----------------------------------------------------------------------------------
pricecomponents<-decompose(price.ts)
plot(pricecomponents)


## -----------------------------------------------------------------------------------
priceforecasts_HW<-HoltWinters(price.ts)
priceforecasts_HW

## -----------------------------------------------------------------------------------
plot(priceforecasts_HW)

## -----------------------------------------------------------------------------------
priceforecasts_HW2<-forecast(priceforecasts_HW,3)
plot(priceforecasts_HW2)


## -----------------------------------------------------------------------------------
par(mfrow=c(1,2))
acf(priceforecasts_HW2$residuals, lag.max=20, na.action = na.contiguous)
pacf(priceforecasts_HW2$residuals, lag.max=20, na.action = na.contiguous)


## -----------------------------------------------------------------------------------
Box.test(priceforecasts_HW2$residuals, lag=20, type="Ljung-Box")

## -----------------------------------------------------------------------------------
plot.ts(priceforecasts_HW2$residuals, main="Residual after Price Forecasting",ylab="Residual")
abline(h=0,lty=2)


## ----message=FALSE,warning=FALSE----------------------------------------------------
plotForecastErrors <- function(forecasterrors)
{
# make a histogram of the forecast errors:
mybinsize <- IQR(forecasterrors)/4
mysd <- sd(forecasterrors)
mymin <- min(forecasterrors) - mysd*5
mymax <- max(forecasterrors) + mysd*3
# generate normally distributed data with mean 0 and standard deviation mysd
mynorm <- rnorm(10000, mean=0, sd=mysd)
mymin2 <- min(mynorm)
mymax2 <- max(mynorm)
if (mymin2 < mymin) { mymin <- mymin2 }
if (mymax2 > mymax) { mymax <- mymax2 }
# make a red histogram of the forecast errors, with the normally distributed data overlaid:
mybins <- seq(mymin, mymax, mybinsize)
hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
# freq=FALSE ensures the area under the histogram = 1
# generate normally distributed data with mean 0 and standard deviation mysd
myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
# plot the normal curve as a blue line on top of the histogram of forecast errors:
points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

completerecords<-na.omit(priceforecasts_HW2$residuals)
plotForecastErrors(completerecords)


## -----------------------------------------------------------------------------------
par(mfrow=c(1,2))
acf(price.ts,lag.max=20)
pacf(price.ts,lag.max=20)


## -----------------------------------------------------------------------------------
auto.arima(price.ts)

## -----------------------------------------------------------------------------------
price.arima<-arima(price.ts,order=c(0,1,1))
price.arimaforecast<-forecast(price.arima,3)
plot(price.arimaforecast)


## -----------------------------------------------------------------------------------
accuracy(priceforecasts_HW2)
accuracy(price.arimaforecast)


## -----------------------------------------------------------------------------------
price.arimaforecast

