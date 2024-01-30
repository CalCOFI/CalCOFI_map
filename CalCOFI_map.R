library(ggplot2)
library(ggmap)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(remotes)
library(calcofi4r)


#set basemap----
world <- ne_countries(scale = "medium", returnclass = "sf")

#load station data----
sites <- read.csv("Data/calcofi_sta_master_v.1.2.csv") |>
  st_as_sf(
    coords = c("lon", "lat"),
    remove = F,
    crs = 4326)

#Interest area layers----
##BOEM wind areas----
boem <- cc_places |>
  filter(category == "BOEM Wind Planning Areas")
st_geometry(boem) = "geometry"

##National Marine Sanctuaries area----
nms <- cc_places |>
  filter(category == "National Marine Sanctuaries")
st_geometry(nms)="geometry"

##NOAA aquaculture opportunity areas----
aqua <- cc_places |>
  filter(category == "NOAA Aquaculture Opportunity Areas")
st_geometry(aqua) =  "gemoetr"

##Add MPA shapefile----
unzip('Data/California_Marine_Protected_Areas_[ds582].zip', exdir = 'Data')
file.remove('Data/California_Marine_Protected_Areas_[ds582].zip')

MPA <- read_sf('Data/California_Marine_Protected_Areas_[ds582].shp')
MPA_4326 <- MPA %>% 
  st_transform(crs=4326)

##Add state waters (3 nautical miles) layer----
unzip('state_waters.zip', exdir = 'Data')

st_water <- read_sf("Data/CA_cst3nm.shp")
st_water_4326 <- st_water %>%
  st_transform(crs = 4326)

##Areas of special biological significance----
unzip('Data/Areas_of_Special_Biological_Significance.zip', exdir = 'Data')
file.remove('Data/Areas_of_Special_Biological_Significance.zip')

bio_sig <- read_sf('Data/Areas_of_Special_Biological_Significance.shp')

##CA National Pollution Dishcard Ellimination Systen Major Outfalls----
unzip('Data/CA_NPDES_Major_outfalls.zip', exdir = 'Data')
file.remove('Data/CA_NPDES_Major_outfalls.zip')

NPDES_outfalls <- read_sf('Data/CA_NPDES_Major_outfalls.shp')
NPDES_outfalls_4326 <- NPDES_outfalls %>% 
  st_transform(crs = 4326)

##Ocean Outfalls----
unzip('Data/OceanOutfalls.zip', exdir = 'Data')
file.remove('Data/OceanOutfalls.zip')

ocean_outfalls <- read_sf('Data/OceanOutfalls.shp')

##Publicly Owned Treatment Works Outfalls----
unzip('Data/POTW_Outfalls.zip', exdir = 'Data')
file.remove('Data/POTW_Outfalls.zip')

POTW_outfalls <- read_sf('Data/POTW_Outfalls.shp')

##Underwater parks/marine managed areas----
unzip('Data/Underwater_parks_and_marine_managed_areas.zip', exdir = 'Data')
file.remove('Data/Underwater_parks_and_marine_managed_areas.zip')

underwater <- read_sf('Data/Underwater_parks_and_marine_managed_areas.shp')
underwater_4326 <- underwater %>% 
  st_transform(crs = 4326)

#Maps----
##add station points to map + BOEM and NMS + state waters----
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

##map with NOAA aquaculture opportunity areas----

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

##Map of Special Biological Significance areas----
ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.5,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = bio_sig, linewidth = 0.1,
          color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  theme(legend.title=element_blank())

##map with state waters and MPA----
ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.5,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = st_water_4326, linewidth = 0.1,
          color = "lightblue", fill = NA)+
  geom_sf(data = MPA_4326, linewidth = 0.1,
          color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  theme(legend.title=element_blank())

#Calculate the number of stations in an interest area----
##Find station points in NMS boundaries----
pnts_nms <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, nms))
  , area = if_else(is.na(intersection), '', nms$name[intersection])
)

pnts_nms

##Find station points in MPA boundaries----
pnts_mpa <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, MPA_4326)),
  area = if_else(is.na(intersection), '', MPA$NAME[intersection])
)

pnts_mpa
