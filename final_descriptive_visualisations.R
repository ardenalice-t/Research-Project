
# Descriptive Investigation -----------------------------------------------



# Packages ----------------------------------------------------------------

library(readxl)
library(readr)

library(sf) # for read_sf
#library(sp)

library(ggplot2) # for plotting
#library(ggmap)

library(dplyr) # for select
#library(spdep)


# Functions ---------------------------------------------------------------

plot_descriptive_ldn <- function(relevant_col, 
                             title, legend_title, 
                             map = LSOA_map, scale = scales::label_number()){
  # plotting
  ggplot() +
    geom_sf(data = LSOA_map, lwd=0, 
            aes(fill = .data[[relevant_col]])) + 
    scale_fill_continuous(name = legend_title, labels = scale, 
                          palette = "viridis") +
    ggtitle(label = title) +
    xlab("Longitude") +
    ylab("Latitude") 
}

# Creating London Map -----------------------------------------------------

# Creating london map of LSOAs
LSOA_map <- read_sf("maps/LDN_LSOA")

LSOA_map <- select(LSOA_map, -c(LSOA21NMW, BNG_E, BNG_N))


## Adding individual data --------------------------------------------------

LSOA_idv_data <- read_csv("data/external_datasets/Sex-GH-Age_LSOA.csv")

# Creating the measurements we want 
LSOA_idv_observations <- LSOA_idv_data %>% 
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
# have checked these measurements with the data

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_idv_observations, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'pc_50_plus', title = "Population Age by LSOA", 
                     legend_title = "Percent of Population \n50 or above", scale = scales::label_percent())


## Adding Household Data -------------------------------------------

LSOA_hos_data <- read_csv("data/external_datasets/household_deprivation_LSOA.csv")

# any 'does not apply' changed to NA 
LSOA_hos_data <- mutate(LSOA_hos_data, 
                       `Household deprivation (6 categories) Code` = 
                         ifelse(`Household deprivation (6 categories) Code` == -8, NA, `Household deprivation (6 categories) Code`))

LSOA_hos_obs <- LSOA_hos_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(avg_hos_depr = 
              sum(Observation * `Household deprivation (6 categories) Code`, na.rm = TRUE) /
              sum(Observation, na.rm = TRUE))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_hos_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'avg_hos_depr', title = "Household Deprivation by LSOA", 
                     legend_title = "Average Number of \nDeprivation Dimensions")


## Adding Population Densities ---------------------------------------------

LSOA_popden_data <- read_csv("data/external_datasets/2021 pop density census.csv")

LSOA_popden_obs <- LSOA_popden_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(pop_den = sum(Observation))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_popden_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'pop_den', title = "Population Density by LSOA", 
                     legend_title = "Residents per Sq Km")

# with exponential scale
ggplot() +
  geom_sf(data = LSOA_map, lwd=0, 
          aes(fill = pop_den)) + 
  scale_fill_continuous(name = "Residents per Sq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log_trans(base = 10)) +
  ggtitle(label = "Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")

