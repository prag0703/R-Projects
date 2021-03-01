# File Name: ALY6020_Fall_TermA2020_Pragati_Koladiya_Assignment2_Part2_Time_series
# Assignment 2 
# Part 2 Time Series Forecasting
# Dataset Information: Data set is chosen from kaggle and store with file name "Electric_Production.csv"

#--------------------------------------Importing the packages-------------------

#install.packages('caTools')
library(caTools)
library(ggplot2)
library(stats)
library(base)
#install.packages("dplyr")
library(dplyr)
#install.packages("gridExtra")
library(stats)
library(gridExtra)
#install.packages("GGally")
library("GGally")
library(readr)
library(seriation) 
#install.packages("moderndive")
library(moderndive)
#install.packages("corrgram")
library(corrgram)
library(RColorBrewer)
library(lattice)
#install.packages("caret")
library(caret)
#install.packages("fpp2")
library(fpp2)
#install.packages("seasonal")
library(astsa)
library(DT)
library(dygraphs)
library(seasonal)
#install.packages("ggseas")
library(ggseas)


#reading the data file name Electric_Production.csv
ep <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/Electric_Production.csv")
View(ep) 
str(ep)
head(ep)

#Converting data frame into time series object.
ep.ts <- ts(ep,frequency=12,start = c(1985,1),end=c(2018,1))

#Monthly data can be resampled as quarterly data(4 months) and Yearly 
#data(12 months), helps to see different seasonality pattern.

ep.ts.qtr <- aggregate(ep.ts,nfrequency=4)
ep.ts.yr <- aggregate(ep.ts,nfrequency=1)

### ------------------------------------ PLOTING TIME SERIES 
plot.ts(ep.ts[,2],main="Monthly Electricity consumption",xlab="Year",ylab="Usage",col=c(rep("deeppink4")))

df <- cbind(Monthly = ep.ts[,2],DailyAverage = ep.ts[,2]/monthdays(ep.ts[,2]))
autoplot(df,facet=TRUE) + xlab("Years")+ylab("Usage")+ggtitle("Electricity production per month")

#quartely plot 
plot.ts(ep.ts.qtr[,2],main="Quarterly Electricity Consumption",xlab="Year",ylab="Usage", col=c(rep("hotpink4")))

#Annual plot
plot.ts(ep.ts.yr[,2],main="Annual Electricity Consumption",xlab="Year",ylab="Usage",col=c(rep("darkorchid4")))

#seasonal plot 
ggseasonplot(ep.ts[,2], year.labels=TRUE, year.labels.left=TRUE,   continuous=TRUE ) +
  ylab("Electricity usage") +
  ggtitle("Electricity Consumption- Seasonal plot")

### -------------------------------------Time series decomposition
# Decomposing time series means :
#   Decomposing a time series means separating it into it’s constituent components,
# which are often a trend component and a random component, and if the data is seasonal, a seasonal component.

#Addtitive decomposition
x<-decompose(ep.ts[,2],type = "additive")
plot(x,col=c("deeppink1"))

#Multiplicative decomposition
x<-decompose(ep.ts[,2],type = "multiplicative")
plot(x,col=c("cadetblue2"))

#seasonality adjusted 
ep_adj <- ep.ts[,2] - x$seasonal
plot.ts(ep_adj,col=c(rep("olivedrab1"))) + title('Seasonally adjusted series')

#Moving average to estimate the trand cicyle.

par(mfrow = c(2,2))
plot(ep.ts[,2], col="gray", main = "1st Year Moving Average Smoothing")
lines(ma(ep.ts[,2], order = 12), col = "deeppink", lwd=3)
plot(ep.ts[,2], col="gray", main = "3rd Year Moving Average Smoothing")
lines(ma(ep.ts[,2], order = 36), col = "darkslategray2", lwd=3)
plot(ep.ts[,2], col="gray", main = "5th Year Moving Average Smoothing")
lines(ma(ep.ts[,2], order = 60), col = "goldenrod1", lwd=3)
plot(ep.ts[,2], col="gray", main = "10th Year Moving Average Smoothing")
lines(ma(ep.ts[,2], order = 120), col = "darkorchid1", lwd=3)

par(mfrow=c(1,1))

#Winter holt's additive method 
ep.ts3 <- window(ep.ts, start = 1985, end = 2000)
ep.ts.qtr <- aggregate(ep.ts3, nfrequency=4)
ep.fit.hw <- hw(ep.ts.qtr[,2], h = 20, seasonal = "additive")


plot(ep.fit.hw, type="o", fcol="white", main = "Electricity Consumption using holt winter's additive", xlab = "Year", ylab = "Usage")
lines(ep.fit.hw$fitted, col = "darkturquoise", lty=2)
lines(ep.fit.hw$mean, col = "darkturquoise", type="o")
legend("topleft", lty = 1, pch = 1, col = c("deeppink1", "darkturquoise"),
       c("Data", "Holt Winters Additive"))


#forecasting using exponential smooting model
ep.fit.ets <- ets(ep.ts.qtr[,2])
plot(forecast::forecast((ep.fit.ets), h = 8), xlab = "Year", ylab = "Usage", col=c("maroon3"), main="Forcasts using Exponential Smoorthing Model")


#summary
summary(ep.fit.ets)


### ------------------------------------- ARIMA MODEL -------------

ggAcf(ep.ts[,2], main="Lag Vs ACF")

whitenoise<-rnorm(100,0,1)
plot(whitenoise,type='l',xlab='Time',ylab=expression('x'[t]),main='White noise process',col=c("lightcoral"))

#Differencing 

diffep <- diff(log(ep.ts.qtr[,2]))
 
diff_dif_ep <- diff(diffep, lag = 4)
plot(diff_dif_ep, main = "Differencing the difference logged eLectricity consumption", col=c("hotpink4"))

acf2(diffep)

acf2(diff_dif_ep)
#We can see a decaying sinusodial pattern in ACF and there is no significant lag/spike after 
#lag 3 in PACF. This implies, we have to choose  𝐴𝑅𝐼𝑀𝐴(3,1,0)  from the plot.

#Seasonal Differenced plot
sea_diff_ep <- diff(log(ep.ts.qtr[,2]),lag=4)
plot(sea_diff_ep, main = "Seasonally differenced logged electricity qonsumption by quarter",col=c("midnightblue"))

acf2(sea_diff_ep)
#From the above plot, we can see that there is a sinusodial pattern/exponential decay 
#in ACF and after each lags, there is a statistical significant spike in PACF.  𝐴𝑅𝐼𝑀𝐴(3,1,0)(0,1,1)

sarima(elec_prod.ts.qtr[,2],3,1,0,0,1,1,4)
#=> By looking at the residual diagnostics, it looks like we have a workable model here since the residuals seem normally distributed, 
#ACF of the residuals are within 95% confidence interval. Therefore, we can now use this ARIMA model to forecast.


#Reference:
#  https://rpubs.com/ryankelly/tsa6
# https://pkg.robjhyndman.com/forecast/reference/seasonplot.html