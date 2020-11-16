#Author:SUNTAOTAO
#Date:20201114
#I got inspired by reading section9
#https://www.cnblogs.com/xuancaoyy/p/5535909.html
#https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/

library(zoo)
library(forecast)
library(dplyr)
library(lubridate)
setwd("C:/Workspace/ESE5023_Assignments/Assignment 04")

# Read the csv file
Airport_Data <- read.csv(file = "2281305.csv", header = T)
BaoAn_data_tbl <- as_tibble(Airport_Data)

# Get the plot data
Baoan_data <- BaoAn_data_tbl %>%
  select(DATE,TMP) %>%
  mutate(
    Tvalue = as.numeric(substr(TMP,2,5)),
    Tflag = as.logical(as.numeric(substr(TMP,7,7))),
    TMPBaoan = Tvalue * 0.1,
    DATEBaoan = substr(DATE,1,7)) %>%
  filter(TMPBaoan!= 999.9 | Tvalue==TRUE)
#Time = as.Date(DATEBaoan,"%Y-%m")

Plot_Data <- Baoan_data %>%
  select(DATEBaoan,TMPBaoan) %>%
  group_by(DATEBaoan) %>%
  summarise(TMPBaoan_M = mean(TMPBaoan)) 
head(Plot_Data)

# Apply the ts() function
# 2.1Construct a time series of monthly-averaged temperature from 2010 Jan. to 2020 Aug.
Tmp <- ts(Plot_Data$TMPBaoan_M, start=c(2010,1), end=c(2020,8),frequency=12)
# Quick plot
plot(Tmp, type="l")

#2.2 Decompose the time series into trend, seasonality, and error parts. 
Tmp_components <- decompose(Tmp)
plot(Tmp_components)

# Plot hist
hist(Tmp_components$random, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(Tmp_components$random,na.rm=T),
            sd=sd(Tmp_components$random,na.rm=T)),
      add=TRUE, col="red")

#2.3 Fit an ARIMA(p,d,q) model to the time series. 
trModel <- lm(Tmp ~ c(1:length(Tmp)))
plot(resid(trModel), type="l")

#run acf() and pacf() function
acf(resid(trModel))
pacf(resid(trModel))
acf(resid(trModel),lag.max=30,plot=FALSE)
pacf(resid(trModel),lag.max=30,plot=FALSE)

#run auto.arima
auto.arima(Tmp,trace=T)

#Test which is better
airarima1 <- arima(Tmp,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12),method="ML")
airarima2 <- arima(Tmp,order=c(1,0,0),seasonal=list(order=c(2,1,0),period=12),method="ML")
airarima1
airarima2

#Predict monthly-averaged temperatures in 2020 Sep. and Oct
Tmpforecast <- forecast(airarima2,h=2,level=c(99.5))
Tmpforecast
forecast:::plot.forecast(Tmpforecast)
