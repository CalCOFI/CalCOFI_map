library(ggplot2)
library(ggmap)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(remotes)
library(calcofi4r)
library(ggspatial)


#set basemap----
world <- ne_countries(scale = "medium", returnclass = "sf")

#load station data----
sites <- read.csv("Data/calcofi_sta_master_v.1.2.csv") |>
  st_as_sf(
    coords = c("lon", "lat"),
    remove = F,
    crs = 4326)

sites_no_pilot <- sites %>% 
  filter(Station == "Core stations" | Station == "North stations")

#Interest area layers----

##BOEM wind areas----
unzip('Data/CA_Call_Area_Outline_2019_09_26.zip', exdir = 'Data')
file.remove('Data/CA_Call_Area_Outline_2019_09_26.zip')

boem <- read_sf('Data/CA_Call_Area_Outline_2018_09_26.shp')
boem_4326 <- boem %>% 
  st_transform(crs=4326)

##National Marine Sanctuaries area----
nms <- cc_places |>
  filter(category == "National Marine Sanctuaries")
st_geometry(nms)="geometry"

##NOAA aquaculture opportunity areas----
aqua <- cc_places |>
  filter(category == "NOAA Aquaculture Opportunity Areas")
st_geometry(aqua) =  "gemoetr"

##Marine Protected Aareas shapefile----
unzip('Data/California_Marine_Protected_Areas_[ds582].zip', exdir = 'Data')
file.remove('Data/California_Marine_Protected_Areas_[ds582].zip')

MPA <- read_sf('Data/California_Marine_Protected_Areas_[ds582].shp')
MPA_4326 <- MPA %>% 
  st_transform(crs=4326)

##State waters (3 nautical miles) layer----
unzip('state_waters.zip', exdir = 'Data')

st_water <- read_sf("Data/CA_cst3nm.shp")
st_water_4326 <- st_water %>%
  st_transform(crs = 4326)

##Areas of special biological significance----
unzip('Data/Areas_of_Special_Biological_Significance.zip', exdir = 'Data')
file.remove('Data/Areas_of_Special_Biological_Significance.zip')

bio_sig <- read_sf('Data/Areas_of_Special_Biological_Significance.shp')
bio_sig_4326 <- bio_sig %>% 
  st_transform(crs = 4326)

##CA National Pollution Discharge Elimination System Major Outfalls----
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

##Cowcod conservation areas----
unzip('Data/data_EPSG_4326.zip', exdir = 'Data')
file.remove('Data/data_EPSG_4326.zip')

cowcod <- read_sf('Data/MAN_SCSR_Cowcod_ConsArea.shp')

#Maps-----
##CalCOFI stations base map----
m <- ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude")+
  geom_sf()+
  geom_point(data = sites, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  annotation_scale()

##No pilot stations base map----
p <- ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude") +
  geom_sf() +
  geom_point(data = sites_no_pilot, aes(x = lon, y = lat), size = 0.75,
             shape = 16, color = "#1F40C7")+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  annotation_scale()

##Most layers included map----
ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude")+
  geom_sf()+
  geom_point(data = sites_no_pilot, aes(x = lon, y = lat, color = Station), size = 0.75,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  geom_sf(data = boem_4326, inherit.aes = F,
          color = "#7E2954", fill = NA) +
  geom_sf(data = nms, inherit.aes = F,
         color = "#DCCD7D", fill = NA) +
  geom_sf(data = st_water_4326, color = "#94CBEC", fill = NA) +
  geom_sf(data = MPA_4326, color = "#337538", fill = NA) +
  geom_sf(data = cowcod, color = "#2E2585", fill = NA) +
  geom_sf(data = aqua, color = "#C26A77", fill = NA) +
  geom_sf(data = underwater, color = "#5DA899", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()+
  annotation_scale()

##NOAA aquaculture opportunity areas map----

ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude")+
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

p + geom_sf(data = aqua, size = 0.5,
         color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##Areas of Special Biological Significance map----
ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude")+
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
  theme(legend.title=element_blank())+
  annotation_scale()

p + geom_sf(data = bio_sig, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##State + MPA map----
ggplot(data = world) +
  xlab("longitude") +
  ylab("latitude")+
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
  theme(legend.title=element_blank())+
  annotation_scale()

p + geom_sf(data = st_water_4326, size = 0.5,
            color = "lightblue", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = MPA_4326, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##NMS map----
m + geom_sf(data = nms, size = 0.5,
            color = "darkred", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = nms, size = 0.5,
            color = "darkred", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##CA NPDES outfalls map----
m + geom_sf(data = NPDES_outfalls_4326, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = NPDES_outfalls_4326, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##Ocean outfalls map----
m + geom_sf(data = ocean_outfalls, size = 0.5,
                color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = ocean_outfalls, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##POTW outfalls map----
m + geom_sf(data = POTW_outfalls, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = POTW_outfalls, size = 0.5,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##Underwater parks and marine managed areas map----
m + geom_sf(data = underwater_4326, linewidth = 0.1,
            color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = underwater_4326, linewidth = 0.1,
           color = "darkred", fill = NA)+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

##Cowcod conservation areas map----
m + geom_sf(data = cowcod, linewidth = 0.1,
            color = "darkred", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

p + geom_sf(data = cowcod, linewidth = 0.1,
              color = "darkred", fill = NA) +
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)

#Number of stations in an interest area----
##NMS boundaries with calCOFI stations----
pnts_nms <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, nms))
  , area = if_else(is.na(intersection), '', nms$name[intersection])
)

pnts_nms

##MPA boundaries with calCOFI stations----
pnts_mpa <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, MPA_4326)),
  area = if_else(is.na(intersection), '', MPA$NAME[intersection])
)

pnts_mpa

##BOEM wind areas with calCOFI stations----
pnts_boem <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, boem_4326)),
  area = if_else(is.na(intersection), '', boem_4326$Area_Name[intersection])
)

##NOAA aquaculture opportunity areas with calCOFI stations----
pnts_aqua <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, aqua)),
  area = if_else(is.na(intersection), '', aqua$name[intersection])
)

##State waters with calCOFI stations----
pnts_st_water <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, st_water_4326)),
  area = if_else(is.na(intersection), '', st_water_4326$SOURCETHM[intersection])
)

##Underwater parks/management areas with calCOFI stations----
pnts_underwater <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, underwater_4326)),
  area = if_else(is.na(intersection), '', underwater_4326$UNITNAME[intersection])
)

##Areas of Special Biological Significance areas with calCOFI stations----
sf::sf_use_s2(FALSE)
pnts_bio_sig_4326 <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, bio_sig_4326)),
  area = if_else(is.na(intersection), '', bio_sig_4326$ASBS_Name[intersection])
)

sf::sf_use_s2(TRUE)

##Cowcod conseration areas with calCOFI stations----
sf::sf_use_s2(FALSE)
cowcod$objectid <- as.character(cowcod$objectid)
pnts_cowcod <- sites %>% mutate(
  intersection = as.integer(st_intersects(geometry, cowcod)),
  area = if_else(is.na(intersection), '', cowcod$objectid[intersection])
)

sf::sf_use_s2(TRUE)

##buffers around stations----
sites_ft <- st_transform(sites, 2264) #change coordinate system to feet
buffer1km <- st_buffer(sites_ft, 3280.84) #setting buffer of 1km
buffer3km <- st_buffer(sites_ft, 9842.52)
buffer5km <- st_buffer(sites_ft, 16404.2)

##ocean outfalls near stations
ocean_outfalls_2264 <- ocean_outfalls %>%
  st_transform(crs = 2264)

pnts_ocean_outfalls <- ocean_outfalls_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer1km)),
  area = if_else(is.na(intersection), "", buffer1km$Station[intersection])
)

pnts_ocean_outfalls <- ocean_outfalls_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer5km)),
  area = if_else(is.na(intersection), "", buffer3km$Station[intersection])
)

pnts_ocean_outfalls <- ocean_outfalls_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer5km)),
  area = if_else(is.na(intersection), "", buffer5km$Station[intersection])
)

##[NOT WORKING] NPDES near stations----
NPDES_outfalls_2264 <- NPDES_outfalls_4326 %>% 
  st_transform(crs = 2264)

pnts_NPDES <- NPDES_outfalls_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer5km)),
  area = if_else(is.na(intersection), "", buffer5km$Station[intersection])
)

##[NOT WORKING] POTW near stations----
POTW_outfalls_2264 <- POTW_outfalls %>% 
  st_transform(crs = 2264)

pnts_POTW <- POTW_outfalls_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer5km)),
  area = if_else(is.na(intersection), "", buffer5km$Station[intersection])
)

#NOT WORKING
MPA_2264 <- MPA_4326 %>% 
  st_transform(crs = 2264)
buffer_MPA <- MPA_2264 %>% mutate(
  intersection = as.integer(st_intersects(geometry, buffer5km)),
  area = if_else(is.na(intersection), "", buffer5km$Station[intersection])
)



##map visualizing the buffers----
m <- ggplot(data = world) +
  xlab("longitute") +
  ylab("latitutde")+
  geom_sf()+
  geom_sf(data = buffer5km, fill = NA, color = "red")+
  geom_sf(data = MPA_4326, linewidth = 0.1,
          color = "darkred", fill = NA)+
  geom_point(data = sites_ft, aes(x = lon, y = lat, color = Station), size = 0.2,
             shape = 16)+
  scale_color_manual(values=c("Core stations" = "#1F40C7", 
                              "North stations" = "#ECCE15", 
                              "Pilot stations" = "#7B7777"))+
  coord_sf(xlim = c(-127, -117), ylim = c(29, 39.5), expand = FALSE)+
  theme_classic()
