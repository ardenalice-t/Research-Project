
# Interpolating LSOA data to grid -----------------------------------------


# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(openairmaps)

## Functions ---------------------------------------------------------------

source("src/plot_descriptive_ldn.R")

# Function to make a grid of london
make_ldn_grid = function(cell_meters, rule, to_cover = london_boundary_sf,
                         crs = british_crs){
  st_grid <- st_make_grid(x=to_cover, cellsize=cell_meters, crs=crs)
  if(rule == "contains"){
    # grid regions fully contained within London only
    london_idx <- st_contains(to_cover, st_grid)[[1]]
    grid_map = st_grid[london_idx]
  }
  if(rule == "intersects"){
    london_idx <- st_intersects(to_cover, st_grid)[[1]]
    grid_map = st_grid[london_idx]
  }
  if(rule == "intersection"){
    intersection <- st_intersection(to_cover, st_grid)
    grid_map <- st_cast(intersection, "POLYGON")
  }
  return(grid_map)
}

view_grid_on_ldn = function(grid_map, london_map = LSOA_map){
  ggplot() +
    geom_sf(data = london_map, lwd=0.1) +
    geom_sf(data = grid_map, lwd=0.1, col="red", alpha=0.1)
}

find_intersects_ldn = function(pointdata_sf){
  london_idx <- st_contains(london_boundary_sf, pointdata_sf)[[1]]
  pointdata_sf <-pointdata_sf[london_idx,]
  return(pointdata_sf)
}

check_crs = function(sf_object, other_object = london_boundary_sf){
  print(paste("Same crs: ",st_crs(sf_object) == st_crs(other_object)))
  print(head(st_coordinates(sf_object)))
  print(head(st_coordinates(other_object)))
}

plot_point_data = function(point_data_sf, title=""){
  ggplot() +
    geom_sf(data = london_boundary_sf) +
    geom_sf(data = point_data_sf, alpha=0.3, col="red", size=0.5) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(label = title)
}

join_points_to_ldn = function(point_data_sf,name, ldn_map = ldn_grid_map){
  # required for st_join st_within
  sf_use_s2(FALSE)

  # joining the points to the grid square they are within
  pd_to_grid <- st_join(point_data_sf, ldn_map, join = st_within)

  # counting points in each grid cell
  count_points <- count(as_tibble(pd_to_grid), ID, name=name)

  ldn_map <- left_join(ldn_map, count_points,
                            by = c("ID" = "ID"))

  # changing any NA to 0
  ldn_map[[name]] <- ifelse(is.na(ldn_map[[name]]), 0, ldn_map[[name]])

  return(ldn_map)

}

## Files -------------------------------------------------------------------

# Reading in LSOA map from 1_loading_data_to_LSOA_map
LSOA_map <- read_sf("outputs/01_interpolation/data/LSOA_data_ldn.gpkg")

# Covering London in Grid -------------------------------------------------

british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

# Creating boundary by joining all LSOAs
london_boundary_sf <- st_cast(st_union(LSOA_map), "POLYGON")

# Making 300 m and 500 m grids of London
ldn_300_grid = make_ldn_grid(300, rule="contains")
ldn_300_grid_intsects = make_ldn_grid(300, rule="intersects")
ldn_300_grid_intersection = make_ldn_grid(300, rule="intersection")
ldn_500_grid = make_ldn_grid(500, rule="intersection")

# Viewing outcomes
view_grid_on_ldn(ldn_300_grid)
view_grid_on_ldn(ldn_300_grid_intsects)
view_grid_on_ldn(ldn_300_grid_intersection)
view_grid_on_ldn(ldn_500_grid)

# Assessing Area loss
find_area_loss = function(grid) return((st_area(london_boundary_sf) - sum(st_area(grid))) /
                                         st_area(london_boundary_sf))
find_area_loss(ldn_300_grid)
find_area_loss(ldn_300_grid_intsects)

# Choosing a grid
ldn_grid_map <- ldn_300_grid_intersection

# Interpolation -----------------------------------------------------------

intensive_cols = c("pc_f", "pc_50_plus", "pc_65_plus",
                    "pc_bad_gh", "avg_hos_dpr", "pop_den", "WD_pop_den")

ldn_grid_map <- st_interpolate_aw(
  LSOA_map[c("geom", intensive_cols)],
  to = ldn_grid_map,
  extensive=FALSE # mean is maintained
)

ldn_grid_map$ID <- seq.int(nrow(ldn_grid_map))

plot_descriptive_ldn(relevant_col = "pop_den",
                     title = "Population Density",
                     legend_title = "Residents per \nsquare km",
                     map = ldn_grid_map)

# Adding Point Data -------------------------------------------------------


## Sports sites -----------------------------------------------------------

sport_site_coords <-  read_csv("data/external_datasets/GIS_Active_Places_Power_Sites_7588440123797672972.csv",
                                  col_types = cols_only(objectid = col_guess(),
                                                        lat = col_guess(),
                                                        long = col_guess()))


sports_sf = st_as_sf(sport_site_coords, coords = c("long", "lat"), crs=lon_lat_crs)
sports_sf <- st_transform(sports_sf, british_crs)

# checking they now have the same crs
check_crs(sports_sf)

# Finding the sports coordinates that intersect London
sports_sf <- find_intersects_ldn(sports_sf)

# Plotting results
plot_point_data(sports_sf, title="Sports Locations")

# Saving result
write_sf(sports_sf, "outputs/01_interpolation/data/ldn_sports_map.gpkg")

check_crs(sports_sf, ldn_grid_map)

# Joining points to map of London
ldn_grid_map <- join_points_to_ldn(sports_sf, "count_sports")

# Plotting results
plot(ldn_grid_map["count_sports"])


## AEDs --------------------------------------------------------------------

AED_data <- read_excel("data/external_datasets/defibrillator_data July 2026.xlsx",
                       sheet = "data_extract_2026-07-01")
AED_data = transform(AED_data, lat = as.numeric(lat)) # changing lat to be a numeric

AED_sf = st_as_sf(AED_data, coords = c("long", "lat"),
                       crs=lon_lat_crs)
AED_sf = st_transform(AED_sf, crs=british_crs)

# Finding the AED coordinates that intersect London
AED_sf <- find_intersects_ldn(AED_sf)

# Saving result
write_sf(AED_sf, "outputs/01_interpolation/data/ldn_AEDs_map.gpkg")

check_crs(AED_sf, ldn_grid_map)

# Joining points to map of London
ldn_grid_map <- join_points_to_ldn(AED_sf, "count_AEDs")

# Plotting results
max_relevant_val = 10

ggplot() +
  geom_sf(data = ldn_grid_map, lwd=0.001,
          aes(fill = count_AEDs)) +
  scale_fill_steps(breaks = seq(0, max_relevant_val, length = 6),
                   na.value = "light blue",
                   rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)),
                   name = "Number of AEDs") +
  ggtitle(label = "Number of AEDs")


## Care Homes --------------------------------------------------------------

care_facility_data <- read_excel("data/external_datasets/Care Facilities invididual sites.xlsx",
                                 sheet = "Sheet1") %>% select(c("Name", "Postcode", "Region"))

# Finding the centroid coordinates of every postcode
for(direction in c("lat", "lng")){
  coords = sapply(care_facility_data$Postcode, function(x){return(try(convertPostcode(x)[[direction]]))})
  care_facility_data[[direction]]= as.numeric(coords)
}

#care_facility_data = transform(care_facility_data, lat = as.numeric(lng), lng = as.numeric(lng))

# Found the postcodes that cannot be loaded manually
# From: https://findthatpostcode.uk/postcodes/RM7%200XY.html
HA7_3JE = c(51.624468, -0.340734)
RM7_0XY = c(51.559053, 0.176637)


care_facility_data[which(care_facility_data$Postcode == "HA7 3JE", arr.ind=TRUE),"lat"] =  HA7_3JE[1]
care_facility_data[which(care_facility_data$Postcode == "HA7 3JE", arr.ind=TRUE),"lng"] =  HA7_3JE[2]
care_facility_data[which(care_facility_data$Postcode == "RM7 0XY", arr.ind=TRUE),"lat"] =  RM7_0XY[1]
care_facility_data[which(care_facility_data$Postcode == "RM7 0XY", arr.ind=TRUE),"lng"] =  RM7_0XY[2]

# Checking if any NA remain
sapply(care_facility_data, anyNA)

# Transforming to sf object
care_facility_sf = st_as_sf(care_facility_data, coords = c("lng", "lat"),
                            crs=lon_lat_crs)
care_facility_sf = st_transform(care_facility_sf, crs=british_crs)


# Checking for same crs
check_crs(care_facility_sf)

# Joining points to map of London
ldn_grid_map <- join_points_to_ldn(care_facility_sf, "count_CHs")

plot(ldn_grid_map["count_CHs"])


# Adding AED count for model 2 --------------------------------------------

# Time intensive

library(maxcovr) # for the custom function

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}

ldn_grid_centers <- st_centroid(ldn_grid_map)
ldn_grid_centers <- st_transform(ldn_grid_centers, lon_lat_crs)
ldn_grid_centers <- sf_to_latlong_matix(ldn_grid_centers)
ldn_grid_centers <- as.matrix(ldn_grid_centers[ , c("lat", "long")])

total_coverage_matrix <- diag(1, nrow(ldn_grid_centers), nrow(ldn_grid_centers))
chance_of_survival <- 0.513 # calculated from Valenzuela et al, 10% every minute
# so the relative difference of 7 mins
max_distance <- 350 # at 6 km / hr in 7 mins can travel 700 m - then halved
partial_coverage_matrix <- binary_matrix_cpp(facility = ldn_grid_centers,
                                             user = ldn_grid_centers,
                                             distance_cutoff = 350)
partial_coverage_matrix <- (partial_coverage_matrix - total_coverage_matrix ) *
  chance_of_survival

coverage_matrix <- total_coverage_matrix + partial_coverage_matrix

# [Time Intensive Line]
#write.csv(coverage_matrix, "outputs/01_interpolation/data/gradated_coverage_matrix.csv")

ldn_grid_map$count_AEDs_gradated.350 = (ldn_grid_map$count_AEDs %*% coverage_matrix)[1,]

plot_descriptive_ldn("count_AEDs_gradated.350", map = ldn_grid_map,
                     title="gradated AEDs", "Number of AEDs")
plot_descriptive_ldn("count_AEDs", map = ldn_grid_map,
                     title="count AEDs", "Number of AEDs")

# Saving Grid Map ---------------------------------------------------------

write_sf(ldn_grid_map, "outputs/01_interpolation/data/interpolated_LDN_grid.gpkg")
#gpkg file allows for more than 10 character file names

