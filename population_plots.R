library(sf)
library(sp)
library(ggplot2)
library(ggmap)
library(dplyr)
library(spdep)

# Reading the map files
OA_map <- read_sf("maps/LDN_LSOA")
LSOA_map <- read_sf("maps/LDN_OA")

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
              sum(Observation),
            bad_gh_proportion = 
              sum(Observation[`General health (4 categories) Code`==3]) /
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
              sum(Observation),
            bad_gh_proportion = 
              sum(Observation[`General health (4 categories) Code`==3]) /
              sum(Observation))

# Joining the data with the previous maps 
OA_map <- left_join(OA_map, OA_observations, 
                        by = c("OA21CD" = "Output Areas Code"))

LSOA_map <- left_join(LSOA_map, LSOA_observations, 
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

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = bad_gh_proportion))


# AED data
aed_map <- read_sf("data/LDN_AEDs")

# transforming to have the same crs 
st_crs(aed_map)
LSOA_map <- st_transform(LSOA_map, crs=st_crs(aed_map))
st_crs(LSOA_map)

test <-aed_map %>%
  st_intersection(LSOA_map)

sf_use_s2(FALSE)

test2 <- st_join(aed_map, LSOA_map, join = st_within)

count_aeds <- count(as_tibble(test2), LSOA21CD, name="count_AEDs")

LSOA_map <- left_join(LSOA_map, count_aeds, 
                      by = c("LSOA21CD" = "LSOA21CD"))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = count_AEDs)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,30))

                