#Author:SUNTAOTAO
#Date:20201024
#I got inspired by reading section6 https://zhu-group.github.io/ese5023/Section_06.html#Boxplots


setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Create data tibble
Lapserate_data <- read.csv("Atmospheric lapse rate.csv",sep = ",")
Lapserate_data_tbl <- as_tibble(Lapserate_data)

#Use pull() to get a the data vector from the tibble
x <- Elevation <- Lapserate_data_tbl %>%
  filter((Elevation != 'NA') & (Temperature !='NA')) %>%
  pull(Elevation)

y <- Temperature <- Lapserate_data_tbl %>%
  filter((Elevation != 'NA') & (Temperature !='NA')) %>%
  pull(Temperature)

#Make the linear regression
fit <- lm(y ~ x)
summary(fit)
summary(fit)$coefficients

#Make the plot
plot(y ~ x,
     xlab = "Elevation(m)",
     ylab = "Temperature(¡æ)",
     main = "Temperature vs Elevation",
     pch = 20,
     cex = 2,
     col = "grey")
abline(fit, lwd = 2, col = "red")
points(mean(x), mean(y), pch = "+", cex = 1)