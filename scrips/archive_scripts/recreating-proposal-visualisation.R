library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset


# Reading the greater London boundary map file 
ldn_boundary_map <- read_sf("maps/gla")
plot(st_geometry(ldn_boundary_map),border="darkgray")
ldn_boundary_map = st_transform(ldn_boundary_map, crs=4283)
# 4283 universal, 27700 uk

# Getting the borough maps
ldn_boroughs <- st_transform(lnd, crs=st_crs(ldn_boundary_map))


# Importing the AED data
library(readxl)
AED_data <- read_excel("data/AED_data_20-05-26.xlsx", 
                       sheet = "data_extract_2026-05-06")
# changing lat to be a numeric
AED_data = transform(AED_data, lat = as.numeric(lat))
AED_data_sf = st_as_sf(AED_data, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
AED_data_sf = st_transform(AED_data_sf, crs=st_crs(ldn_boundary_map))


# Finding the AED coordinates that intersect London
london_idx <- st_contains(ldn_boundary_map, AED_data_sf)[[1]]
ldn_AED_data_sf <-AED_data_sf[london_idx,]

write_sf(ldn_AED_data_sf, "data/LDN_AEDs/ldn_AEDs_map.shp")


# Plotting London AEDs
ggplot() + 
  geom_sf(data = ldn_boroughs) +
  geom_sf(data=ldn_AED_data_sf,
          size = 0.0001,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(label = "Existing AED locations") + 
  coord_sf(crs = st_crs(ldn_boundary_map))


# Just plotting the central boroughs
central_boroughs <- ldn_boroughs[grepl('City of London' , ldn_boroughs$NAME) | grepl('Westminster' , ldn_boroughs$NAME) ,]

# Finding the AED coordinates that intersect London
central_idx <- c(st_contains(central_boroughs, AED_data_sf)[[1]], st_contains(central_boroughs, AED_data_sf)[[2]])
central_AED_data_sf <-AED_data_sf[central_idx,]


# Plotting London AEDs
ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = central_boroughs) +
  geom_sf(data=central_AED_data_sf,
          size = 0.005,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(label = "AED locations") + 
  coord_sf(crs = st_crs(central_boroughs))

