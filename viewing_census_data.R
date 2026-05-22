library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset

# Reading the greater London boundary map file 
lnd_boundary_map <- read_sf("maps/gla")
plot(st_geometry(lnd_boundary_map),border="darkgray")
#lnd_boundary_map = st_transform(lnd_boundary_map, crs=4283)

# Reading the Output_Areas map file 
lsoa_map <- read_sf("maps/Lower_layer_Super_Output_Areas")
#oa_map <- read_sf("maps/Output_Areas")
lnd_lsoa_map <- st_intersection(lnd_boundary_map, lsoa_map)
test = st_contains(lnd_boundary_map, lsoa_map)[[1]]
lnd_lsoa_map_contains <- lsoa_map[st_contains(lnd_boundary_map, lsoa_map)[[1]],]
lnd_lsoa_map_intersects <- lsoa_map[st_intersects(lnd_boundary_map, lsoa_map)[[1]],]

plot(st_geometry(lsoa_areas_map),border="darkgray")

ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = lnd_lsoa_map)

ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = lnd_boundary_map) +
  geom_sf(data = lnd_lsoa_map_contains)

ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = lnd_boundary_map) +
  geom_sf(data = lnd_lsoa_map_contains)

ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = lnd_boundary_map)
