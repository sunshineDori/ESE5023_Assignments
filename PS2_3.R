#Author:SUNTAOTAO
#Date:20201016
#Description:Use functions from tidyr, dplyr, and ggplot2 packages to
#Reproduce the same time series in PS1_7.R 

library(tidyr)
library(dplyr)
library(ggplot2)

# Read the Precipitation csv file 
Precipitation_Data <- read.csv(file = "VQC00670480.csv", header = T) 
names(Precipitation_Data) 

#Covert to a tibble object
Precipitation_tbl <- as_tibble(Precipitation_Data)
Precipitation_tbl

#Get the date-precipitation value and plot
Precipitation_tbl %>%
  select(DATE,DlySum,DlySumQF) %>%
  filter(DATE>"2016-12-31") %>%
  mutate(Year=substr(DATE,1,4)) %>%
  group_by(Year) %>%
  summarise(sum_cal=sum(DlySum),mean_cal=mean(DlySum),
            median_cal=median(DlySum),min_cal=min(DlySum),
            max_cal=max(DlySum),range_cal=range(DlySum)) 

# Make the plot 
Pre_Plot <- Precipitation_tbl %>%
  select(DATE,DlySum,DlySumQF) %>%
  filter(DATE>"2016-12-31") %>%
  mutate(Year=substr(DATE,1,4),日期=as.Date(DATE,"%Y-%m-%d")) %>%
  ggplot(aes(x=日期)) +
  scale_x_date(breaks="20 days",date_labels="%m/%d")+
  geom_line(aes(y=DlySum),color = "#69b3a2",size=0.5)
ggsave("Pre_Plot2017.png",Pre_Plot,width = 15,height = 6,units = "cm")



