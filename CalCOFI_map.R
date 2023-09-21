if (!"librarian" %in% installed.packages())
  install.packages("librarian")

librarian::shelf(
  CalCOFI/calcofi4r,
  dplyr, ggmap, ggplot2, mapview, sf)

# get BOEM wind areas
plys_boem <- cc_places |> 
  filter(category == "BOEM Wind Planning Areas")
st_geometry(plys_boem) = "geometry" # for geom_sf()

# load stations
pts_stations <- read.csv("Data/calcofi_sta_master_v.1.2.csv") |> 
  filter(
    !is.na(lon), 
    !is.na(lat),
    lon < -115) |> # remove orderocc==104 in Yellow Sea?!
  st_as_sf(
    coords = c("lon", "lat"),
    remove = F,
    crs = 4326)
# mapView(pts_stations)

bb <- st_bbox(pts_stations)

#set basemap
basemap <- get_stamenmap(
  bbox    = c(
    left   = bb[['xmin']], 
    bottom = bb[['ymin']], 
    right  = bb[['xmax']], 
    top    = bb[['ymax']]),
  zoom = 7,
  maptype = c("terrain-background"))
ggmap(basemap)

#add station points
ggmap(basemap) + 
  xlab("longitude") + 
  ylab("latitude") +
  geom_sf(
    data = plys_boem,
    inherit.aes = F,
    color = "darkblue", fill="blue") +
  geom_sf(
    data = pts_stations,
    inherit.aes = F,
    shape = 16, size=1.25,
    mapping = aes(color = leg)) +
  scale_color_manual(
    values = c("core" = "forestgreen", "north" = "darkred"))
ggsave("draft_CalCOFI_map_boem.jpg")


