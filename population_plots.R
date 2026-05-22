library(sf)
library(ggplot2)
library(ggmap)
library(dplyr)

# Reading the map files
ldn_lsoa_map <- read_sf("maps/LDN_LSOA")
ldn_oa_map <- read_sf("maps/LDN_OA")

# Reading population data 
library(readr)
OA_pop_data <- read_csv("data/Age-GH-Sex_Output-Area.csv")
LSOA_pop_data <- read_csv("data/Sex-GH-Age_LSOA.csv")

# Getting just the data for each area 
OA_observations <- OA_pop_data %>% 
  group_by(`Output Areas Code`)%>% 
  summarise(female_proportion = 
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation),
            over_50_proportion = 
              sum(Observation[(`Age (6 categories) Code`==5) | 
                                (`Age (6 categories) Code`==6)]) /
              sum(Observation)
            )

LSOA_observations <- LSOA_pop_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(female_proportion = 
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation),
            over_50_proportion = 
              sum(Observation[(`Age (6 categories) Code`==5) | 
                                (`Age (6 categories) Code`==6)]) /
              sum(Observation))

# Joining the data with the previous maps 
OA_map <- left_join(ldn_oa_map, OA_observations, 
                        by = c("OA21CD" = "Output Areas Code"))

LSOA_map <- left_join(ldn_lsoa_map, LSOA_observations, 
                        by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))


# Plotting the maps showing the gender split 
ggplot() +
  geom_sf(data = OA_map, lwd=0.001, 
          aes(fill = female_proportion))

ggplot() +
  geom_sf(data = LSOA_map,lwd=0.001, 
          aes(fill = female_proportion))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = over_50_proportion))

          
                