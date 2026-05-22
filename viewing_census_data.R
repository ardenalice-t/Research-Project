library(sf)
library(ggplot2)
library(ggmap)
library(readr)
library(dplyr)

ldn_boroughs = c('Barking and Dagenham','Barnet','Bexley','Brent',
                'Bromley','Camden','City of London','Croydon','Ealing',
                'Enfield','Greenwich','Hackney','Hammersmith and Fulham',
                'Haringey','Harrow','Havering','Hillingdon','Hounslow',
                'Islington','Kensington and Chelsea','Kingston upon Thames',
                'Lambeth','Lewisham','Merton','Newham','Redbridge',
                'Richmond upon Thames','Southwark','Sutton','Tower Hamlets',
                'Waltham Forest','Wandsworth','Westminster')

# Reading the map files
lsoa_map <- read_sf("maps/Lower_layer_Super_Output_Areas")
oa_map <- read_sf("maps/Output_Areas")

# Reading a look up file to get just London areas
LSOA_to_region_lookup <- read_csv("maps/LSOA_to_region_lookup.csv", show_col_types = FALSE)
ldn_LSOA_region_lookup <- LSOA_to_region_lookup[LSOA_to_region_lookup$RGN22NM == "London",]

# Filtering to London LSOA
ldn_LSOA_map <- lsoa_map %>% filter(LSOA21NM %in% ldn_LSOA_region_lookup$LSOA21NM)
ldn_oa_map <- oa_map %>% filter(LSOA21NM %in% ldn_LSOA_region_lookup$LSOA21NM)

# Plotting
ggplot() + 
  geom_sf(data = ldn_LSOA_map)
ggplot() +
  geom_sf(data = ldn_oa_map)

# Saving the filtered maps
write_sf(ldn_LSOA_map, "maps/LDN_LSOA/ldn_LSOA_map.shp")
write_sf(ldn_oa_map, "maps/LDN_OA/ldn_oa_map.shp")

