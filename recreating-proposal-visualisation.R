library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset


# Reading the greater London boundary map file 
lnd_boundary_map <- read_sf("maps/gla")
plot(st_geometry(lnd_boundary_map),border="darkgray")
lnd_boundary_map = st_transform(lnd_boundary_map, crs=4283)
# 4283 universal, 27700 uk

# Getting the borough maps
ldn_boroughs <- st_transform(lnd, crs=st_crs(lnd_boundary_map))


# Importing the AED data
library(readxl)
AED_data <- read_excel("data/AED_data_20-05-26.xlsx", 
                       sheet = "data_extract_2026-05-06")
# changing lat to be a numeric
AED_data = transform(AED_data, lat = as.numeric(lat))
AED_data_sf = st_as_sf(AED_data, coords = c("long", "lat"), 
                       crs=st_crs(lnd_boundary_map))
AED_data_sf = st_transform(AED_data_sf, crs=st_crs(lnd_boundary_map))


# Finding the AED coordinates that intersect London
london_idx <- st_contains(lnd_boundary_map, AED_data_sf)[[1]]
ldn_AED_data_sf <-AED_data_sf[london_idx,]


# Plotting London AEDs
ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = ldn_boroughs) +
  geom_sf(data=ldn_AED_data_sf,
          size = 0.005,alpha = 0.5,
          aes(colour = 'AED')) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = st_crs(lnd_boundary_map))

