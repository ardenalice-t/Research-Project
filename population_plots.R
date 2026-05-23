library(sf)
library(sp)
library(ggplot2)
library(ggmap)
library(dplyr)
library(spdep)

# Reading the map files
OA_map <- read_sf("maps/LDN_OA")
LSOA_map <- read_sf("maps/LDN_LSOA")

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


# Plotting the maps 
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


### Workday population density ###

LSOA_WD_data <- read_csv("data/Workday_population_lsoa.csv")
colnames(LSOA_WD_data)[3] <- "workday_population_density"
LSOA_map <- left_join(LSOA_map, LSOA_WD_data, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))
ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = workday_population_density))


### Household deprivation ###

LSOA_HD_data <- read_csv("data/household_deprivation_LSOA.csv")
LSOA_HD_data <- mutate(LSOA_HD_data, `Household deprivation (6 categories) Code` = ifelse(`Household deprivation (6 categories) Code` == -8, NA, `Household deprivation (6 categories) Code`))
LSOA_HD_data <- LSOA_HD_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(avg_deprivation = 
              sum(Observation * `Household deprivation (6 categories) Code`, na.rm = TRUE) /
              sum(Observation, na.rm = TRUE))
LSOA_map <- left_join(LSOA_map, LSOA_HD_data, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))
ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = avg_deprivation))


### AED locations ###

aed_map <- read_sf("data/LDN_AEDs")

# transforming to have the same crs 
LSOA_map <- st_transform(LSOA_map, crs=st_crs(aed_map))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the AEDs to the LSOA they are within
aed_to_LSOA <- st_join(aed_map, LSOA_map, join = st_within)

# counting AEDs in each LSOA
count_aeds <- count(as_tibble(aed_to_LSOA), LSOA21CD, name="count_AEDs")

# Plotting resulting map
LSOA_map <- left_join(LSOA_map, count_aeds, 
                      by = c("LSOA21CD" = "LSOA21CD"))
# changing any NA to 0
LSOA_map <- mutate(LSOA_map, "count_AEDs" = ifelse(is.na(count_AEDs), 0, count_AEDs))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = count_AEDs)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,30))

                