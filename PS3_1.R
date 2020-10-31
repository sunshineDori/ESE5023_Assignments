#Author:SUNTAOTAO
#Date:20201024
#I got inspired by reading section6 and section7.
#Liyuan explained to me the objective of the question,and why should we use factor.

setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Create data tibble
Rainfall_data_tbl <- tibble(rainfall=c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8,95.0, 87.0, 81.2, 68.5, 
                   47.3, 41.1, 36.6, 29.0, 28.6, 26.3,26.0, 24.4, 21.4, 17.3,11.5, 4.9, 4.9, 1.0,
                   2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1,302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 
                   198.6, 129.6, 119.0,118.3, 115.3, 92.4, 40.6, 32.7, 31.4,17.5, 7.7, 4.1),
                   seed=rep(0:1,each=26))

# Change to factor type
Rainfall_data_tbl <- Rainfall_data_tbl %>%
  mutate(seed = factor(seed, ordered = TRUE))

# Quick check
glimpse(Rainfall_data_tbl)
levels(Rainfall_data_tbl$seed)

#Compute the mean and standard deviation
Rainfall_data_tbl %>%
  group_by(seed) %>%
  summarise(
    count = n(),
    mean_rainfall = mean(rainfall, na.rm = TRUE),
    sd_rainfall = sd(rainfall, na.rm = TRUE)
  )

#Plot two box plots side-by-side of the data. Discribe the distributions.
Rainfall_Plot <- ggplot(Rainfall_data_tbl, aes(x = seed, y = rainfall, fill = seed)) +
  geom_boxplot() +
  theme_classic()
ggsave("Rainfall_plot.png",Rainfall_Plot,width = 15,height = 6,units = "cm")

#Run the one-way ANOVA test
anova_one_way <- aov(rainfall ~ seed, data = Rainfall_data_tbl)
summary(anova_one_way)





