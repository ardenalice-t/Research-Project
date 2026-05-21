library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset

# Reading the greater london boundary map file 
lnd_boundary_map <- read_sf("maps/gla")
plot(st_geometry(lnd_boundary_map),border="darkgray")

# Alternative method
london_border <- read_sf(dsn = "maps/London_GLA_Boundary.shp")
plot(london_border$geometry)

# Getting the borough maps
data(lnd) # loads the specified London map dataset
summary(lnd)
ldn_boroughs <- st_geometry(lnd) # gets geometry from sf object
plot(ldn_boroughs)

# Importing the AED data
library(readxl)
AED_data_20_05_26 <- read_excel("data/AED_data_20-05-26.xlsx", 
                                sheet = "data_extract_2026-05-06")
