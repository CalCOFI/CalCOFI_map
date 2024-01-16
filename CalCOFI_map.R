library(ggplot2)
library(ggmap)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(remotes)
library(calcofi4r)


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

#National Marine Sanctuaries area
nms <- cc_places |>
  filter(category == "National Marine Sanctuaries")
st_geometry(nms)="geometry"

#NOAA aquaculture opportunity areas
aqua <- cc_places |>
  filter(category == "NOAA Aquaculture Opportunity Areas")
st_geometry(aqua) =  "gemoetr"

#add station points to map + BOEM and NMS + state waters
ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = boem, inherit.aes = F,
          color = "darkred", fill = NA) +
  geom_sf(data = nms, inherit.aes = F,
          color = "orange", fill = NA) +
  geom_sf(data = st_water_4326, color = "green", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()

#map with NOAA aquaculture opportunity areas

ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = aqua, inherit.aes = F,
          color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  theme(legend.title=element_blank())
 

#save image
ggsave("draft_CalCOFI_map_9-27-23.jpg")


#Add MPA shapefile
unzip('Data/MPA_and_NMS.zip', exdir = 'data')
file.remove('Data/MPA_and_NMS.zip')

MPA_NMS <- read_sf("Data/MPA_and_NMS.shp")
MPA_NMS_4326 <- MPA_NMS %>% 
  st_transform(crs=4326)

#Add state waters (3 nautical miles) layer
unzip('state_waters.zip', exdir = 'data')

st_water <- read_sf("Data/CA_cst3nm.shp")
st_water_4326 <- st_water %>%
  st_transform(crs = 4326)

#map with state waters, MPA, and NMS
ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = st_water_4326,
          color = "lightblue", fill = NA)+
  geom_sf(data = MPA_NMS_4326,
          color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  theme(legend.title=element_blank())

#Find station points in NMS boundaries
pnts_nms <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, nms))
  , area = if_else(is.na(intersection), '', nms$name[intersection])
)

pnts_nms

