library(ggplot2)
library(ggmap)

#set basemap
basemap <- get_stamenmap(bbox =c(left = -126.000, bottom = 29.000, right = -117.000, top = 39.500), maptype = c("terrain-background"))
ggmap(basemap)

#load data
sites <- read.csv("Data/CalCOFI_stations_116.csv")

#add station points
ggmap(basemap) + xlab("longitude") + ylab("latitutde") +
  geom_point(data=sites, aes(x=Dlongitude, y=Dlatitude), 
             shape = 16, color="black", size=1)





