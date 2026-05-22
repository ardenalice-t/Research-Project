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

# Getting just the data on sex for each area 
OA_sex_data <- OA_pop_data %>% 
  group_by(`Output Areas Code`)%>% 
  summarise(female_proportion = 
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation))

LSOA_sex_data <- LSOA_pop_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(female_proportion = 
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation))

OA_sex_map <- left_join(ldn_oa_map, OA_sex_data, 
                        by = c("OA21CD" = "Output Areas Code"))

LSOA_sex_map <- left_join(ldn_lsoa_map, LSOA_sex_data, 
                        by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

ggplot() +
  geom_sf(data = OA_sex_map, aes(fill = female_proportion))

ggplot() +
  geom_sf(data = LSOA_sex_map, aes(fill = female_proportion))

          
                