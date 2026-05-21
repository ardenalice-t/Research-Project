require(sf)
library(ggplot2)
library(ggmap)

# Reading the greater london boundary map file 
lnd_boundary_map <- read_sf("maps/gla")
plot(st_geometry(lnd_boundary_map),border="darkgray")
