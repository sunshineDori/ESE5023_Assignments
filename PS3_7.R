#Author:SUNTAOTAO
#Date:20201031
#I got inspired by reading section6 and section7.

setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Read csv file
Monitoring_Data <- read.csv("MonitoringData.csv",header = TRUE,sep = ",")

#Convert data to tibble
Monitoring_data_tbl <- as_tibble(Monitoring_Data)
head(Monitoring_data_tbl)

#7.1 t-test
Monitoring_Aug <- Monitoring_data_tbl %>%
  filter(Month=="8")

Monitoring_Sep <- Monitoring_data_tbl %>%
  filter(Month=="9")

t.test(Monitoring_Aug$PH, Monitoring_Sep$PH)


#7.2 one-way ANOVA test
#Plot box plots side-by-side of the data. Discribe the distributions.
Monitoring_data_tbl_new<-Monitoring_data_tbl %>%
  mutate(Month=factor(Month,ordered = TRUE)) 

Monitoring_data_tbl_new %>%
  ggplot(aes(x = Month, y = PH, fill = Month)) +
  geom_boxplot() +
  theme_classic()
ggsave("Monitoring_plot.png",width = 18,height = 12,units = "cm")

#Run the one-way ANOVA test
anova_one_way <- aov(PH ~ Month, data = Monitoring_data_tbl_new)
summary(anova_one_way)

#7.3 linear model test
# Split into two subsets
sample_index <- sample(nrow(Monitoring_data_tbl_new),nrow(Monitoring_data_tbl_new)*0.85)
Monitoring_train <- Monitoring_data_tbl_new[sample_index,]
Monitoring_test  <- Monitoring_data_tbl_new[-sample_index,]

# Build a linear model
model_log <- lm( PH ~ Watertemprature, data=Monitoring_train)

# Add regression line
plot(PH ~ Watertemprature, data=Monitoring_train)
abline(model_log,col="red")

# Get estimates
summary(model_log)


