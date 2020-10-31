#Author:SUNTAOTAO
#Date:20201030
#I got inspired by reading section6, section7 and lab3.

setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

# Load data
library(MASS);library(leaps)
data(cpus)

# Check col names
head(cpus)

cpus <- cpus%>%
  mutate(mean_perf=mean(perf))

# Split into two subsets
sample_index <- sample(nrow(cpus),nrow(cpus)*0.8)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]

#6.1For the train set, fit the best subset regression between predictor variable 
#perf and response variables including syct, mmin, mmax, cach, chmin, and chmax.
cpus_train_result <- regsubsets(perf ~ syct+ mmin + mmax + cach +
                              chmin + chmax, data=cpus_train, nbest=2, nvmax = 6)
plot(cpus_train_result, scale="bic")

# Build a linear model
fullmodel=lm(perf ~ syct+ mmin + mmax + cach +
               chmin + chmax, data=cpus_train)
model_step_b <- step(fullmodel,direction='backward')

# Get estimates
summary(fullmodel)

#6.2Apply the best regression model to the test set, and compare your predicted 
#perf values with the actual values that provided in the test set. 
# Apply the model model_log to the test subset
perf_predict <- predict(fullmodel,cpus_test)

# Compare predicted values with actual values 
plot(cpus_test$mean_perf, perf_predict)

# Correlation coefficients
cor(cpus_test$mean_perf, perf_predict)

# Mean predicted value
mean(perf_predict)

# Mean actual value
mean(cpus_test$mean_perf)

# Relative mean bias
(mean(perf_predict) - mean(cpus_test$mean_perf))/mean(cpus_test$mean_perf)




