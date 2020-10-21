#Author:SUNTAOTAO
#Date:20201016
#Description:Use functions from tidyr, dplyr, and ggplot2 packages to
#Plot monthly averaged wind speed as a function of the observation time. 
#Liyuan explained to me how to get the monthly mean value by using "group_by(Time,Month)"

library(tidyr)
library(dplyr)
library(ggplot2)

# Read the csv file
Airport_Data <- read.csv(file = "2281305.csv", header = T)
names(Airport_Data)

#Covert to a tibble object
Shenzhen_Wnd <- as_tibble(Airport_Data)
Shenzhen_Wnd

#Get the date-wnd value and plot
Shenzhen_Wnd_Plot <- Shenzhen_Wnd %>%
  select(DATE,WND) %>%
  mutate(Year=substr(DATE,1,4),Month=substr(DATE,6,7),
         Time = as.Date(Year,"%Y"),
         Wind_rate_o=substr(WND,9,12),
         Wind_rate=as.numeric(Wind_rate_o)*0.1,
         Wind_flag=substr(WND,14,14)) %>%
  filter(Wind_rate_o!=9999 & Wind_flag==1) %>%
  group_by(Time,Month) %>%
  #需要根据年、月求均值
  summarise(Wind_rate_mon=mean(Wind_rate,na.rm = TRUE)) %>%
  # Make the plot
  ggplot(aes(x=Time, y=Wind_rate_mon, color=Month)) +
  scale_x_date(breaks="3 years",date_labels="%Y")+
  geom_line()+
  facet_wrap(~ Month)
ggsave("Shenzhen_Wnd_Plot.png",Shenzhen_Wnd_Plot,height = 9,width = 15,units = "cm")

  

