library(ggplot2)
library(ggmap)

#set basemap
basemap <- get_stamenmap(bbox =c(left = -126.000, bottom = 29.000, right = -117.000, top = 39.500), maptype = c("terrain-background"))
ggmap(basemap)

#load data
sites <- read.csv("Data/calcofi_sta_master_v.1.2.csv")

#add station points
ggmap(basemap) + xlab("longitude") + ylab("latitutde") +
  geom_point(data=sites, aes(x=lon, y=lat, color=leg), 
             shape = 16, size=1.25)+
  scale_color_manual(values=c("core" = "forestgreen", "north" = "darkred"))
ggsave("draft_CalCOFI_map_9-20-23.jpg")

