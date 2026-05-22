library(sf)
library(ggplot2)
library(ggmap)
library(spData) # has the ldn dataset
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

# Reading the Output_Areas map file 
lsoa_map <- read_sf("maps/Lower_layer_Super_Output_Areas")

# Reading a lookup file to get just London LSOA
LSOA_to_region_lookup <- read_csv("maps/LSOA_to_region_lookup.csv", show_col_types = FALSE)
ldn_LSOA_region_lookup <- LSOA_to_region_lookup[LSOA_to_region_lookup$RGN22NM == "London",]

# Filtering to london LSOA
ldn_LSOA_map <- lsoa_map %>% filter(LSOA21NM %in% ldn_LSOA_region_lookup$LSOA21NM)

# Plotting LSOA
ggplot() + # initializes a ggplot object, layers added
  geom_sf(data = ldn_LSOA_map)

