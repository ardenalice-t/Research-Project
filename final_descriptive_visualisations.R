
# Descriptive Investigation -----------------------------------------------



# Packages ----------------------------------------------------------------

library(readxl)
library(readr)

library(sf) # for read_sf
#library(sp)
library(spData) # has the ldn dataset

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
    geom_sf(data = map, lwd=0, 
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
  scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans()) +
  ggtitle(label = "Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")


LSOA_WD_popden_data <- read_csv("data/external_datasets/WD_pop_den.csv")

LSOA_WD_popden_obs <- LSOA_WD_popden_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(WD_pop_den = sum(`Population Density`))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_WD_popden_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'WD_pop_den', title = "Workday Population Density by LSOA", 
                     legend_title = "Residents per Sq Km")

# with exponential scale
ggplot() +
  geom_sf(data = LSOA_map, lwd=0, 
          aes(fill = WD_pop_den)) + 
  scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans(), limits = c(NA,100000)) +
  ggtitle(label = "Workday Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")

# Plotting both side by side
library(gridExtra)
plot1 = ggplot() +
    geom_sf(data = LSOA_map, lwd=0, 
            aes(fill = pop_den)) + 
    scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                          palette = "viridis", transform = scales::log10_trans()) +
    ggtitle(label = "Population Density by LSOA") +
  theme(legend.position="none")+
    xlab("Longitude") +
    ylab("Latitude")
  
plot2 = ggplot() +
    geom_sf(data = LSOA_map, lwd=0, 
            aes(fill = WD_pop_den)) + 
    scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                          palette = "viridis", transform = scales::log10_trans(), limits = c(NA,100000)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), aspect.ratio = 0.8) + 
    ggtitle(label = "Workday Population Density by LSOA") +
    xlab("Longitude") +
    ylab("Latitude")


grid.arrange(plot1, plot2, ncol=2, widths=c(0.355, 0.4) )


# Saving LSOA Map ---------------------------------------------------------

write_sf(LSOA_map, "data/LSOA_complete_map_July.shp")


# Gridding Data -----------------------------------------------------------

ldn_boundary_map <- read_sf("maps/gla")

LSOA_map <- read_sf("data/LSOA_complete_map_July.shp")

names(LSOA_map)[10] = "avg_dpr"


# Function to make a grid of london
make_ldn_grid = function(cell_meters){
  st_grid <- st_make_grid(x=ldn_boundary_map, cellsize=cell_meters)
  london_idx <- st_intersects(ldn_boundary_map, st_grid)[[1]]
  return(st_grid[london_idx])
}

ldn_300_grid = make_ldn_grid(300)
ldn_400_grid = make_ldn_grid(400)
ldn_500_grid = make_ldn_grid(500)

LSOA_map <- st_transform(LSOA_map, crs=st_crs(ldn_300_grid))

# with LSOA
plot(LSOA_map$geometry)
plot(ldn_grid, add=TRUE)

# This is a grid of london but not sure how i would interpolate the values onto this

numeric_columns = c("pc_f", "pc_50_p", "pc_65_p", "pc_bd_g", "avg_dpr", "pop_den", "WD_pp_d")
ldn_grid_values = st_interpolate_aw(
  LSOA_map[c("geometry", numeric_columns)],
  to = ldn_500_grid,
  extensive=FALSE # mean is maintained 
)
plot(ldn_grid_values["pop_den"])

write_sf(ldn_grid_values, "data/grids/ldn_grid_values_500.shp")



# MSOA Map - Care Homes ---------------------------------------------------

ldn_boroughs = 'Havering|Barking and Dagenham|Barnet|Bexley|Brent|Bromley|Camden|City of London|Croydon|Ealing|Enfield|Greenwich|Hackney|Hammersmith and Fulham|Haringey|Harrow|Hillingdon|Hounslow|Islington|Kensington and Chelsea|Kingston upon Thames|Lambeth|Lewisham|Merton|Newham|Redbridge|Richmond upon Thames|Southwark|Sutton|Tower Hamlets|Waltham Forest|Wandsworth|Westminster'

# Creating london map of MSOAs
full_MSOA_map <- read_sf("maps/Middle_layer_Super_Output_Areas_December_2021")

full_MSOA_map <- select(full_MSOA_map, -c(MSOA21NMW, BNG_E, BNG_N))

st_crs(ldn_boundary_map)

# just taking the MSOA in London, using names
LDN_MSOA_map = dplyr::filter(full_MSOA_map, grepl(ldn_boroughs, MSOA21NM))
LDN_MSOA_map = dplyr::filter(LDN_MSOA_map, !grepl('Brentwood', MSOA21NM))


# plotting to check 
ggplot() + 
  geom_sf(data = ldn_boundary_map, alpha=0.2, colour="blue") +
  geom_sf(data = LDN_MSOA_map) + 
  geom_sf(data = ldn_boundary_map, alpha=0.2, colour="red") 

# saving
write_sf(LDN_MSOA_map, "maps/LDN_MSOA/ldn_MSOA_map.shp")


## Care Homes --------------------------------------------------------------

MSOA_CH_data <- read_csv("data/external_datasets/UK care home.csv", 
                         skip = 7)

names(MSOA_CH_data) = c('MSOA', 'mnemonic', 'total', 'LA_CHwN', 'LA_CHwoN', 'O_CHwN', 'O_CHwoN')

MSOA_CH_obs <- MSOA_CH_data %>% 
  group_by(MSOA)%>% 
  summarise(CH_pop = 
              sum(LA_CHwN) + sum(LA_CHwoN) + sum(O_CHwN) + sum(O_CHwoN) )

# Joining with the LSOA map
LDN_MSOA_map <- left_join(LDN_MSOA_map, MSOA_CH_obs, 
                      by = c("MSOA21NM" = "MSOA"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'CH_pop', title = "Care Population by MSOA", 
                     legend_title = "Number of people \nliving in Care Facilities", 
                     map = LDN_MSOA_map)

max(LDN_MSOA_map$CH_pop)

## Adding AEDs -------------------------------------------------------------

ldn_boundary_map <- read_sf("maps/gla")
ldn_boundary_map = st_transform(ldn_boundary_map, crs=4283)

AED_data <- read_excel("data/external_datasets/defibrillator_data July 2026.xlsx", 
                                                       sheet = "data_extract_2026-07-01")
# changing lat to be a numeric
AED_data = transform(AED_data, lat = as.numeric(lat))
AED_data_sf = st_as_sf(AED_data, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
AED_data_sf = st_transform(AED_data_sf, crs=st_crs(ldn_boundary_map))

# Finding the AED coordinates that intersect London
london_idx <- st_contains(ldn_boundary_map, AED_data_sf)[[1]]
ldn_AED_data_sf <-AED_data_sf[london_idx,]

write_sf(ldn_AED_data_sf, "data/LDN_AEDs_July/ldn_AEDs_map.shp")

ldn_boroughs <- st_transform(lnd, crs=st_crs(ldn_boundary_map))

ggplot() + 
  geom_sf(data = ldn_boroughs) +
  geom_sf(data=ldn_AED_data_sf,
          size = 0.0001,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(label = "Existing AED locations") + 
  coord_sf(crs = st_crs(ldn_boundary_map))

# transforming to have the same crs 
LSOA_map <- st_transform(LSOA_map, crs=st_crs(ldn_AED_data_sf))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the AEDs to the LSOA they are within
aed_to_LSOA <- st_join(ldn_AED_data_sf, LSOA_map, join = st_within)

# counting AEDs in each LSOA
count_aeds <- count(as_tibble(aed_to_LSOA), LSOA21CD, name="count_AEDs")

# Plotting resulting map
LSOA_map <- left_join(LSOA_map, count_aeds, 
                      by = c("LSOA21CD" = "LSOA21CD"))
# changing any NA to 0
LSOA_map <- mutate(LSOA_map, "count_AEDs" = ifelse(is.na(count_AEDs), 0, count_AEDs))

# Plotting Result
ggplot() +
  geom_sf(data = LSOA_map, lwd=0, 
          aes(fill = count_AEDs)) + 
  scale_fill_continuous(name = "Number of AEDs", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans()) +
  ggtitle(label = "AED count by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")
