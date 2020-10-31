#Author:SUNTAOTAO
#Date:20201024
#I got inspired by reading section6 and section7.

setwd("C:/Workspace/ESE5023_Assignments")
library(tidyr)
library(dplyr)
library(ggplot2)

#Read csv file
Bone_Data <- read.csv("Tyrannosaurus.csv",header = TRUE,sep = ",")

#Convert data to tibble
Bone_data_tbl <- as_tibble(Bone_Data)

#Convert to long data and change to facor type
Bone_data_tblnew<-Bone_data_tbl %>%
  gather(Bone_amount,Temp_value,-Bone,na.rm = TRUE) %>%
  mutate(Bone = factor(Bone, ordered = TRUE)) 


#Plot two box plots side-by-side of the data. Discribe the distributions.
Bone_Plot <- ggplot(Bone_data_tblnew, aes(x = Bone, y = Temp_value, fill = Bone)) +
  geom_boxplot() +
  theme_classic()
ggsave("Bone_plot.png",Bone_Plot,width = 35,height = 10,units = "cm")

#Run the one-way ANOVA test
anova_one_way <- aov(Temp_value ~ Bone, data = Bone_data_tblnew)
summary(anova_one_way)




