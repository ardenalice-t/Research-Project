
# Loading Data to LSOA map ----------------------------------------------------


# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

## Functions ---------------------------------------------------------------

source("src/plot_descriptive_ldn.R")

plot_descriptive_ldn_exp_scale <- function(relevant_col, #title,
                                           legend_title,
                                           map = LSOA_map){
  ggplot() +
    geom_sf(data = map, lwd=0,
            aes(fill  = .data[[relevant_col]])) +
    scale_fill_continuous(name = legend_title, labels = scales::label_number(),
                          limits = c(NA,100000),
                          palette = "viridis", transform = scales::log10_trans()) +
    #ggtitle(label = title) +
    xlab("Longitude") +
    ylab("Latitude")
}



## Files -------------------------------------------------------------------



# Creating London Map -----------------------------------------------------

# Creating London map of LSOAs
LSOA_map <- read_sf("data/maps/LDN_LSOA")
LSOA_map <- select(LSOA_map, -c(LSOA21NMW, BNG_E, BNG_N))


# Adding Data -------------------------------------------------------------

## Adding individual data --------------------------------------------------

LSOA_idv_data <- read_csv("data/external_datasets/Sex-GH-Age_LSOA.csv")

# Creating the desired statistics
LSOA_idv_statistics <- LSOA_idv_data %>%
  group_by(`Lower layer Super Output Areas Code`)%>%
  summarise(pc_f =
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation),
            pc_50_plus =
              sum(Observation[(`Age (6 categories) Code`==5) |
                                (`Age (6 categories) Code`==6)]) /
              sum(Observation),
            pc_65_plus =
              sum(Observation[ (`Age (6 categories) Code`==6)]) /
              sum(Observation),
            pc_bad_gh =
              sum(Observation[`General health (4 categories) Code`==3]) /
              sum(Observation))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_idv_statistics,
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting
plot_descriptive_ldn(relevant_col = 'pc_50_plus',
                     legend_title = "Percent of Population \n50 or above",
                     map=LSOA_map)


## Adding Household Data -------------------------------------------

LSOA_hos_data <- read_csv("data/external_datasets/household_deprivation_LSOA.csv")

# 'does not apply' changed to NA
LSOA_hos_data <- mutate(LSOA_hos_data,
                        `Household deprivation (6 categories) Code` =
                          ifelse(`Household deprivation (6 categories) Code` == -8,
                                 NA,
                                 `Household deprivation (6 categories) Code`))

LSOA_hos_statistics <- LSOA_hos_data %>%
  group_by(`Lower layer Super Output Areas Code`)%>%
  summarise(avg_hos_dpr =
              sum(Observation * `Household deprivation (6 categories) Code`, na.rm = TRUE) /
              sum(Observation, na.rm = TRUE))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_hos_statistics,
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting
plot_descriptive_ldn(relevant_col = 'avg_hos_dpr',
                     legend_title = "Average Number of \nDeprivation Dimensions",
                     map=LSOA_map)

## Adding Population Densities ---------------------------------------------

LSOA_popden_data <- read_csv("data/external_datasets/2021 pop density census.csv")

LSOA_popden_statistics <- LSOA_popden_data %>%
  group_by(`Lower layer Super Output Areas Code`)%>%
  summarise(pop_den = sum(Observation))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_popden_statistics,
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting
plot_descriptive_ldn(relevant_col = 'pop_den',
                     legend_title = "Residents per sq km",
                     map=LSOA_map)

# with exponential scale
plot_descriptive_ldn_exp_scale(relevant_col = 'pop_den',
                     legend_title = "Residents per sq km")


LSOA_WD_popden_data <- read_csv("data/external_datasets/WD_pop_den.csv")

LSOA_WD_popden_statistics <- LSOA_WD_popden_data %>%
  group_by(`Lower layer Super Output Areas Code`)%>%
  summarise(WD_pop_den = sum(`Population Density`))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_WD_popden_statistics,
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting
plot_descriptive_ldn(relevant_col = 'WD_pop_den',
                     legend_title = "Residents per sq km",
                     map=LSOA_map)

# with exponential scale
plot_descriptive_ldn_exp_scale(relevant_col = 'WD_pop_den',
                     legend_title = "Residents per sq km")
# Saving LSOA Map ---------------------------------------------------------

write_sf(LSOA_map, "outputs/01_interpolation/data/LSOA_data_ldn.gpkg")
#gpkg file allows for more than 10 character file names


# Figures -----------------------------------------------------------------

plot_descriptive_ldn_exp_scale(relevant_col = 'pop_den',
                               legend_title = "Residents per sq km")

plot_descriptive_ldn_exp_scale(relevant_col = 'WD_pop_den',
                               legend_title = "Residents per sq km")

plot_descriptive_ldn(relevant_col = 'avg_hos_dpr',
                     legend_title = "Average household \ndeprivation dimensions",
                     map=LSOA_map)

# Average LSOA size
print(paste("Average LSOA area:", mean(st_area(LSOA_map)/ 1000^2) ))
print(paste("SD LSOA area:", sd(st_area(LSOA_map)/ 1000^2) ))

# Average residents
print(paste("Average LSOA population:",
            mean(LSOA_map$pop_den * (st_area(LSOA_map)/ 1000^2))))
print(paste("SD LSOA population:",
            sd(LSOA_map$pop_den * (st_area(LSOA_map)/ 1000^2))))

population = LSOA_map$pop_den * (as.numeric(st_area(LSOA_map)/ 1000^2))
hist(population, breaks = 20)
