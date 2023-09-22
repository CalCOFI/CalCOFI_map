library(ggplot2)
library(ggmap)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#set basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

#load station data
sites <- read.csv("Data/calcofi_sta_master_v.1.2.csv") |>
  st_as_sf(
    coords = c("lon", "lat"),
    remove = F,
    crs = 4326)

#get BOEM wind areas
boem <- cc_places |>
  filter(category == "BOEM Wind Planning Areas")
st_geometry(boem) = "geometry"

#add station points to map
ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777" ))+
  geom_sf(data = boem, inherit.aes = F,
          color = "darkred", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  theme(legend.title=element_blank())

#save image
ggsave("draft_CalCOFI_map_9-22-23_sm_pt.jpg")





