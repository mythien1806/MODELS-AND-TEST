## ----setup, include=TRUE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(kableExtra)
library(PoEdata)


## 1. Simple Linear Model: Using the fultonfish database from the PoE package, where we have the natural log of the dollar price of fish represented as lprice and the quantity of fish sold at that price represented as quan (use R help command ?fultonfish to check for the other variable definitions)
data("fultonfish",package="PoEdata")
fultonfish$price = exp(fultonfish$lprice)


## A. plot scatterplots to understand the best functional form for regressing quantity sold on price
plot(fultonfish$price,fultonfish$quan,
     main="Quantity sold on price",
     xlab="Price of fish",
     ylab="Quantity of fish sold",
     pch=20,col=4)
abline(lm(fultonfish$quan~fultonfish$price),col=2,lwd=2)




## The scatterplot shows the negative relationship between the price and the quantity of fish sold.
##B. Run the following models for the regressing of quantity of whiting sold on price, and compare for each of these models, the value of the change of quantity sold for a marginal rise of price from
###i.	Linear model
###ii.	Log-log model
###iii.	Quadratic model
###iv.	Log-level model
###v.	Level-log model

par(mfrow=c(3,2))

# I. LINEAR MODEL
mod1<-lm(fultonfish$quan~fultonfish$price)
smod1<-summary(mod1)
plot(fultonfish$price,fultonfish$quan,
     xlab="Price of fish",
     ylab="Quantity of fish sold",
     sub="Linear Fit", col=4,pch=20)
abline(lm(fultonfish$quan~fultonfish$price), col=2)
yhat1<-predict(mod1)
rg1<-cor(fultonfish$quan, yhat1)^2
text(1.6,20000,paste("Rg^2=", round(rg1,3)))
text(1.6,16000,paste("Radj^2=", round(smod1$adj.r.squared,3)))
text(1.6,12000,paste("R^2=", round(smod1$r.squared,3)))
##############################################################

##############################################################
# II. LOG-LOG MODEL
ordat <- fultonfish[order(fultonfish$price),]
mod2<-lm(log(quan)~log(price),data = ordat)
smod2<-summary(mod2)
plot(fultonfish$price,fultonfish$quan,xlab="Price of fish",
     ylab="Quantity of fish sold",
     sub="Log-Log Fit", col=4,pch=20)
b1 <- coef(mod2)[[1]]
b2 <- coef(mod2)[[2]]
yhat2 <- exp(b1+b2*log(ordat$price)) 
lines(yhat2~ordat$price, col=2)
rg2 <- cor(ordat$quan, yhat2)^2
text(1.6,20000,paste("Rg^2=", round(rg2,3)))
text(1.6,16000,paste("Radj^2=", round(smod2$adj.r.squared,3)))
text(1.6,12000,paste("R^2=", round(smod2$r.squared,3)))
##############################################################

##############################################################
# III. QUADRATIC MODEL
ordat <- fultonfish[order(fultonfish$price),]
mod3 <- lm(quan~I(price^2), data=ordat)
smod3<-summary(mod3)
plot(fultonfish$price,fultonfish$quan,xlab="Price of fish",
     ylab="Quantity of fish sold",
     sub="Quandratic Fit", col=4,pch=20)
lines(fitted(mod3)~ordat$price, col=2)
yhat3 <- predict(mod3)
rg3 <- cor(ordat$quan, yhat3)^2
text(1.6,20000,paste("Rg^2=", round(rg3,3)))
text(1.6,16000,paste("Radj^2=", round(smod3$adj.r.squared,3)))
text(1.6,12000,paste("R^2=", round(smod3$r.squared,3)))
##############################################################

##############################################################
# IV. LOG-LEVEL MODEL
ordat <- fultonfish[order(fultonfish$price),]
mod4 <- lm(log(quan)~price, data=ordat)
smod4<-summary(mod4)
plot(fultonfish$price,fultonfish$quan,xlab="Price of fish",
     ylab="Quantity of fish sold",
     sub="Log-Level Fit", col=4,pch=20)
lines(exp(fitted(mod4))~ordat$price, col=2)
yhat4 <- exp(predict(mod4))
rg4 <- cor(ordat$quan, yhat4)^2
text(1.6,20000,paste("Rg^2=", round(rg4,3)))
text(1.6,16000,paste("Radj^2=", round(smod4$adj.r.squared,3)))
text(1.6,12000,paste("R^2=", round(smod4$r.squared,3)))
##############################################################

##############################################################
# V. LEVEL-LOG MODEL
ordat <- fultonfish[order(fultonfish$price),]
mod5 <- lm(quan~log(price), data=ordat)
smod5<-summary(mod5)
plot(fultonfish$price,fultonfish$quan,xlab="Price of fish",
     ylab="Quantity of fish sold",
     sub="Level-Log Fit", col=4,pch=20)
lines(fitted(mod5)~ordat$price,col=2)
yhat5 <- predict(mod5)
rg5 <- cor(ordat$quan, yhat5)^2
text(1.6,20000,paste("Rg^2=", round(rg5,3)))
text(1.6,16000,paste("Radj^2=", round(smod5$adj.r.squared,3)))
text(1.6,12000,paste("R^2=", round(smod5$r.squared,3)))
##############################################################

# C. Which of the above models do you think is most suited for the prediction of quantity sold changes for a given change in price? 
## Among 5 above models, because the Rg^2 of the Log-Level Fit is the highest, the Log-Level model is most suited for the prediction of quantity sold changes for a given change in price.


#D. Are all L.I.N.E. assumptions met for the above model?
par(mfrow=c(2,2))

plot(fultonfish$price,fultonfish$quan,
     main="Quantity sold on price",
     xlab="Price of fish",
     ylab="Quantity of fish sold",
     type="p",
     pch=20,col=4)
mod1<-lm(fultonfish$quan~fultonfish$price)
abline(lm(fultonfish$quan~fultonfish$price),col=2,lwd=2)

ehat <- mod1$residuals
plot(fultonfish$price, ehat, xlab="price", ylab="residuals")

hist(mod1$residuals, col="grey", freq=FALSE)
ebar <- mean(ehat)
sde <- sd(ehat)
curve(dnorm(x, ebar, sde), col=2, add=TRUE,
      ylab="density", xlab="ehat")

qqnorm(mod1$residuals)
qqline(mod1$residuals)

###We can see the spread of the residuals seems to be higher at lower price, which means the assumption equal variance (homoscedasticity) is violated.

# 2) Using the same database as the above problem, summarize all the variables. 
## A. Descriptive Statistics and Summaries of Data
###i.For the numerical variables report in a table the mean, median, max, min, and standard deviation.  
###ii.Also, for these variables create the five number summaries using box-plots.


price<-c(mean=mean(fultonfish$price),median=median(fultonfish$price),max=max(fultonfish$price), min=min(fultonfish$price),sd=sd(fultonfish$price))

lprice<-c(mean=mean(fultonfish$lprice),median=median(fultonfish$lprice),max=max(fultonfish$lprice), min=min(fultonfish$lprice),sd=sd(fultonfish$lprice))

quan<-c(mean=mean(fultonfish$quan),median=median(fultonfish$quan),max=max(fultonfish$quan), min=min(fultonfish$quan),sd=sd(fultonfish$quan))

lquan<-c(mean=mean(fultonfish$lquan),median=median(fultonfish$lquan),max=max(fultonfish$lquan), min=min(fultonfish$lquan),sd=sd(fultonfish$lquan))

totr<-c(mean=mean(fultonfish$totr),median=median(fultonfish$totr),max=max(fultonfish$totr), min=min(fultonfish$totr),sd=sd(fultonfish$totr))

diff<-c(mean=mean(fultonfish$diff),median=median(fultonfish$diff),max=max(fultonfish$diff), min=min(fultonfish$diff),sd=sd(fultonfish$diff))


Des.stat<-data.frame(price,lprice,quan,lquan,totr,diff)
Des.stat%>%
  knitr::kable(caption = "Descriptive Statistics",digits = 2) %>%
  kableExtra::kable_styling(full_width = T)


## ---------------------------------------------------------------------------------
par(mfrow=c(2,3))

boxplot(fultonfish$price,main="Boxplot of price",xlab="Price",col=4)

boxplot(fultonfish$lprice,main="Boxplot of log price",xlab="Log price",col=4)

boxplot(fultonfish$quan,main="Boxplot of Quantity of fish sold",xlab="Quantity",col=4)

boxplot(fultonfish$lquan,main="Boxplot of Log Quantity of fish sold",xlab="Log Quantity",col=4)

boxplot(fultonfish$totr,main="Boxplot of Total Received",xlab="Total received",col=4)

boxplot(fultonfish$diff,main="Boxplot of Inventory change",xlab="Inventory change",col=4)

##B. Report the correlation matrix and the scatter plots of quantity on all numerical variables. Which variables do you think explain the variations of quantity sold of fish best.

data<-select(fultonfish,lprice,totr,diff)
correlation.matrix<-cor(fultonfish$quan,data)
correlation.matrix

par(mfrow = c(2,2))

plot(fultonfish$lprice~fultonfish$quan,
     main = "Log Price vs Quantity",
     xlab="Quantity", 
     ylab="Log Price", 
     type = "p",
     pch=20,col=4)

plot(fultonfish$totr~fultonfish$quan,
     main = "Total received vs Quantity",
     xlab="Quantity", 
     ylab="Total received", 
     type = "p",
     pch=20,col=4)

plot(fultonfish$diff~fultonfish$quan,
     main = "Inventory change vs Quantity",
     xlab="Quantity", 
     ylab="Inventory change", 
     type = "p",
     pch=20,col=4)



### Because correlation coefficient r=0.795, which indicates the strong positive linear relationship between total received and quantity. So the variable total received explain the variations of quantity sold of fish best.



##C. Now using the dataset, regress log of quantity sold on the log of prices, day of week, whether it was rainy, stormy or had mixed winds and waves. 
###i.Interpret the coefficients for each of these variables.
###ii.Do you need all these variables in your model to explain fish sales? 
library(broom)
data1<-lm(lquan~lprice+mon+tue+wed+thu+stormy+mixed+rainy+cold, data = fultonfish)
kable(tidy(data1), caption = "The dummy model",digits=2) %>%
  kable_styling(full_width = T)

###No we do not need all variables to explain fish sales since some of p-value are pretty large. We can consider some variables such as : lprice, tuesday, and wednesday as best to explain the fish sales.

##E. Run the level-level regression model of quantity sold on prices and rain day.  But now including their interactions. Interpret what the dummy and interaction terms tell you. 
interaction<- lm(quan~price*rainy, data=fultonfish)
summary(interaction)


###The dummy coefficient is 5631, which means controlling for price of fish, the effect of a rainy day raises the quantity of fish sales by 5631 pound.

###The coefficient of price is -3052 and the interactions coefficient is -5952, which means when it is not a rainy day, one unit increase in price, the quantity of fish sold is decrease 3052 pounds. By contrast, when it is a rainy day, one unit increase in price, the quantity decreases 9004 pounds.
