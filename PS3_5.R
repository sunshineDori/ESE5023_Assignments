#Author:SUNTAOTAO
#Date:20201024
#I got inspired by reading section6 https://zhu-group.github.io/ese5023/Section_06.html#Boxplots


setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Create data tibble
Distance_data <- read.csv("The Big Bang Theory.csv",sep = ",")
Distance_data_tbl <- as_tibble(Distance_data)

#Use pull() to get a the data vector from the tibble
x <- Velocity <- Distance_data_tbl %>%
  filter((Velocity != 'NA') & (Distance !='NA')) %>%
  pull(Velocity)

y <- Velocity <- Distance_data_tbl %>%
  filter((Velocity != 'NA') & (Distance !='NA')) %>%
  pull(Distance)

#Make the linear regression
fit <- lm(y ~ x)
summary(fit)
summary(fit)$coefficients

#Make the plot
plot(y ~ x,
     xlab = "Velocity(km/s)",
     ylab = "Distance(megaparsecs)",
     main = "Distance vs Velocity",
     pch = 20,
     cex = 2,
     col = "grey")
abline(fit, lwd = 2, col = "red")
points(mean(x), mean(y), pch = "+", cex = 1)