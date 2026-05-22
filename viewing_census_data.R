library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset

# Reading the Output_Areas map file 
output_areas_map <- read_sf("maps/Output_Areas")
plot(st_geometry(output_areas_map),border="darkgray")
output_areas_map = st_transform(output_areas_map, crs=4283)