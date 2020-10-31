#Author:SUNTAOTAO
#Date:20201030
#I got inspired by reading section6 and section7.

setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Read csv file
Zinc_Data <- read.csv("Vegetarians and Zinc.csv",header = TRUE,sep = ",")

#Convert data to tibble
Zinc_data_tbl <- as_tibble(Zinc_Data)
Zinc_pregnant_veg <- Zinc_Data$Pregnant.vegetarians
Zinc_pregnant_nonveg <- Zinc_Data$Pregnant.nonvegetarians
t.test(Zinc_pregnant_veg,na.omit(Zinc_pregnant_nonveg))


#Convert to long data and change to facor type
Zinc_data_tblnew<-Zinc_data_tbl %>%
  gather(Zinc_flag,Zinc_value,na.rm = TRUE) %>%
  mutate(Zinc_flag = factor(Zinc_flag, ordered = TRUE)) 


#Plot two box plots side-by-side of the data. Discribe the distributions.
Zinc_Plot <- ggplot(Zinc_data_tblnew, aes(x = Zinc_flag, y = Zinc_value, fill = Zinc_flag)) +
  geom_boxplot() +
  theme_classic()
ggsave("Zinc_plot.png",Zinc_Plot,width = 30,height = 9,units = "cm")

#Run the one-way ANOVA test
anova_one_way <- aov(Zinc_value ~ Zinc_flag, data = Zinc_data_tblnew)
summary(anova_one_way)




