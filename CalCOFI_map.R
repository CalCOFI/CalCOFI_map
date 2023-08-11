library(ggplot2)
library(ggmap)

#set basemap
basemap <- get_stamenmap(bbox =c(left = -123.737691, bottom = 32.000, right = -117.713201, top = 39.507131), maptype = c("terrain-background"))
ggmap(basemap)

