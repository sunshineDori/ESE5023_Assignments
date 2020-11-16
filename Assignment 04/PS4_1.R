#Author:SUNTAOTAO
#Date:20201113
#I got inspired by reading section8.

setwd("C:/Workspace/ESE5023_Assignments/Assignment 04")
library(tidyr)
library(dplyr)
library(ggplot2)
library(fields); library(maps); library(RNetCDF)
#Read csv file
Monitoring_Data <- read.csv("MonitoringData.csv",header = TRUE,sep = ",")

#Convert data to tibble
Monitoring_data_tbl <- as_tibble(Monitoring_Data)
head(Monitoring_data_tbl)

#Plot data setting
Plot_Data <- Monitoring_data_tbl 
Plot_Data$Time <- as.Date(Plot_Data$Time)
Plot_Data$Month = factor(Plot_Data$Month, levels=c('Aug','Sep','Oct'))
class(Plot_Data$Time)

#4.1Boxplot
Pbox <- ggplot(data = Plot_Data, 
            mapping = aes(x = Month, y = PH ))
Mboxplot <- Pbox + geom_boxplot(aes(color = factor(Flag))) +
  geom_jitter(aes(color = factor(Flag)),
              position = position_jitterdodge()) +
  labs(
    title = "A监测站PH变化图",
    subtitle = "2020年8-10月",
    caption = "数据来源：监测站",
    x = "月份",
    y = "PH值")
ggsave("Mbox_plot.png",Mboxplot,width = 18,height = 12,units = "cm")

#4.2Time series
Pts <- ggplot(data = Plot_Data, 
                     mapping = aes(x = Time, y = PH ))
Mtsplot <- Pts + geom_point(aes(color=Month)) + 
  geom_line(aes(color=Month)) +
  scale_x_date(
    date_breaks = "10 days",
    date_labels = "%m-%d",
    #date_minor_breaks = "5 days",
    expand = c(0, 0)) +
  labs(
    title = "A监测站PH变化图",
    subtitle = "2020年8-10月",
    caption = "数据来源：监测站",
    x = "时间",
    y = "PH值")
ggsave("Mts_plot.png",Mtsplot,width = 18,height = 12,units = "cm")

#4.3Histogram
Phist <- ggplot(data = Plot_Data, 
              mapping = aes(x = PH))
Mhistplot <- Phist + geom_histogram(aes(fill=Month)) +
  facet_wrap(~Month)+  
  labs(
    title = "A监测站PH变化图",
    subtitle = "2020年8-10月",
    caption = "数据来源：监测站",
    x = "PH值",
    y = "数量")
ggsave("Mhist_plot.png",Mhistplot,width = 18,height = 12,units = "cm")

#4.4Scatter plot
Pscatter <- ggplot(data = Plot_Data, 
                mapping = aes(x = Time, y = PH))

Mscatterplot <- Pscatter + geom_point(aes(color=Month,size=PH)) + 
  scale_x_date(
    date_breaks = "10 days",
    date_labels = "%m-%d") +
  labs(
    title = "A监测站PH变化图",
    subtitle = "2020年8-10月",
    caption = "数据来源：监测站",
    x = "时间",
    y = "PH值")
ggsave("Mscatter_plot.png",Mscatterplot,width = 18,height = 12,units = "cm")

#4.5Image plot
ex.nc     <- open.nc("air.mon.ltm.nc")
# Read the variables
Lat       <- var.get.nc(ex.nc, "lat")
Lon       <- var.get.nc(ex.nc, "lon")
# Monthly long term mean, surface temperature [degC]
Air_T     <- var.get.nc(ex.nc, "air") 
# Close the NetCDF file
close.nc(ex.nc)
# Original Lat is in decreasing order, we need to reverse it
Lat <- rev(Lat)

# Data transformation of Air_T_Jan
Air_T_Jan <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  Air_T_Jan[,row] <- Air_T[, (length(Lat)+1-row),1 ]
}
# Set the png format
png("Image_plot.png", width=18, height=12, units="cm", res=400) 

# Set margins on bottom, left, top, right
par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon, Lat, Air_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [degC]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long term (1800-2020) mean surface temperature in Jan."),
      cex.main=1,font.main=2)

# Add map
map('world',add=T,lwd=0.75,col="black")
# Add a box
box(lwd=2)
# Close the png file to save the file
dev.off()
