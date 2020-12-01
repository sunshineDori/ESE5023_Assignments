#Author:SUNTAOTAO
#Date:20201201
#I got inspired by reading section10 https://zhu-group.github.io/ese5023/Section_10.html
#https://zhuanlan.zhihu.com/p/280968987
#https://cengel.github.io/rspatial/4_Mapping.nb.html

setwd("C:/Workspace/ESE5023_Assignments/Assignment 05")

library("sp")
library("raster")
library("sf")
library("rgdal")


#1.1 Read tiff files
#get the 12 layers with different month
dir("wc2.1_2.5m_srad",full.names = T) %>% 
  stack() -> worldclimsrad
dir("wc2.1_2.5m_prec",full.names = T) %>% 
  stack() -> worldclimprec
dir("wc2.1_2.5m_wind",full.names = T) %>% 
  stack() -> worldclimwind
#create a new layer with mean values
Srad_mean <- stackApply(worldclimsrad,indices=c(1),fun=mean,na.rm = TRUE)
Prec_mean <- stackApply(worldclimprec,indices=c(1),fun=mean,na.rm = TRUE)
Wind_mean <- stackApply(worldclimwind,indices=c(1),fun=mean,na.rm = TRUE)
# Look at the raster attributes
Srad_mean
Prec_mean
Wind_mean

#just choose a single month
#Srad <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_12.tif")
#Prec <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_12.tif")
#Wind <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_12.tif")


#1.2Plot the above data sets over China. You should make three plots, each should contain its own legend.
# Read china map, a shape file
China_map_crop <- readOGR("China_map", "bou2_4p") 
# Crop the raster with china map
Srad_crop <- crop(Srad_mean, China_map_crop)
Prec_crop <- crop(Prec_mean, China_map_crop)
Wind_crop <- crop(Wind_mean, China_map_crop)

# Plot cropped region
spplot(Srad_crop,main="Solar radiation in 2020.")
spplot(Prec_crop, main="Precipitation in 2020.")
spplot(Wind_crop, main="Wind speed in 2020.")

#1.3 search for regions with relatively high wind speed to build wind farms.
Wind_rc <- reclassify(Wind_crop,c(-Inf,6,NA))
Wind_rc2 <- reclassify(Wind_crop,c(-Inf,quantile(Wind_crop,0.9), NA))
spplot(Wind_rc,
       sp.layout =list("sp.polygons", China_map_crop,col='red',lwd=0.1),,main="Potential locations for wind farms.")
spplot(Wind_rc2,
       sp.layout =list("sp.polygons", China_map_crop,col='red',lwd=0.1),
       main="Potential locations for wind farms.")

#1.4 regions with relatively high solar radiation and low precipitation as potential locations of photovoltaics (PV) farms.
#reclassify the areas(srad>75%)=1,otherwise 0;
#reclassify the areas(prec<75%)=1,otherwise 0;
Srad_rc <- reclassify(Srad_crop,c(-Inf,quantile(Srad_crop,0.75),0,quantile(Srad_crop,0.75),Inf,1))
Prec_rcChina_map_crop <- reclassify(Prec_crop,c(-Inf,quantile(Prec_crop,0.75),1,quantile(Prec_crop,0.75), Inf, 0))
#get the mean of Srad_rc and Prec_rc
Srad_Prec <- stack(Srad_rc,Prec_rc)
PV_region_test <- stackApply(Srad_Prec,indices=c(1),fun=mean,na.rm = TRUE)
#reclassify the areas(Srad_Prec=1)=1,otherwise NA;
PV_region <- reclassify(PV_region_test,c(-Inf,0.9,NA))
#plot
spplot(PV_region,
       sp.layout =list("sp.polygons", China_map_crop,col='red',lwd=0.1),
       main="Potential locations for photovoltaics (PV).") 



