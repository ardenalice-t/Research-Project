library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset

# Reading the greater london boundary map file 
lnd_boundary_map <- read_sf("maps/gla")
plot(st_geometry(lnd_boundary_map),border="darkgray")
# testing something - to make them have the same crs by overriding 
lnd_boundary_map = st_transform(lnd_boundary_map, crs=4326)


# Getting the borough maps
data(lnd) # loads the specified London map dataset
summary(lnd)
ldn_boroughs <- st_geometry(lnd) # gets geometry from sf object
plot(ldn_boroughs)


# Importing the AED data
library(readxl)
AED_data <- read_excel("data/AED_data_20-05-26.xlsx", 
                                sheet = "data_extract_2026-05-06")
# changing lat to be a numeric
AED_data = transform(AED_data, lat = as.numeric(lat))
AED_data_sf = st_as_sf(AED_data, coords = c("long", "lat"), crs=st_crs(lnd_boundary_map))

# Finding the AED coordinates that intersect london
london_idx <- st_intersects(lnd_boundary_map$geometry, AED_data_sf)[[1]]
ldn_AED_data_sf <-AED_data_sf[london_idx,]
