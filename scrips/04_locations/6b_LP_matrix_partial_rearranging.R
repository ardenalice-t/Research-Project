# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(dplyr)
library(maxcovr) # for the custom function + cite for code adapted from
library(ggplot2)

## Functions ---------------------------------------------------------------

join_points_to_ldn = function(point_data_sf,name, ldn_map = LDN_grid){
  # required for st_join st_within
  sf_use_s2(FALSE)

  # joining the points to the grid square they are within
  pd_to_grid <- st_join(point_data_sf, ldn_map, join = st_intersects)

  # counting points in each grid cell
  count_points <- count(as_tibble(pd_to_grid), ID, name=name)

  ldn_map <- left_join(ldn_map, count_points,
                       by = c("ID" = "ID"))

  # changing any NA to 0
  ldn_map[[name]] <- ifelse(is.na(ldn_map[[name]]), 0, ldn_map[[name]])

  return(ldn_map)

}

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}


## Files -------------------------------------------------------------------

LDN_grid <- read_sf("outputs/03_detrending/data/detrended_data_ldn.gpkg")

## Getting coverage matrix -----
british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"
ldn_grid_centers <- st_centroid(LDN_grid)
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


# Picking AEDs to rearrange -----------------------------------------------

AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")

nAEDs = nrow(AED_sf)
set.seed(1111)
get_sample_AEDs <- function(percent, filename){
  remaining_aeds <- AED_sf
  removing_indexes <- sample(x = 1:nAEDs,size = round(nAEDs * percent / 100))
  write.csv(removing_indexes, paste("outputs/04_locations/data/LP_exports/", filename, ".csv", sep=""))
  remaining_aeds = remaining_aeds[-removing_indexes,]
  return(remaining_aeds)
}

get_remaining_coverage <- function(percent_to_remove, filename){
  remaining_aeds <- get_sample_AEDs(percent_to_remove, filename)
  temp_grid <- join_points_to_ldn(st_transform(remaining_aeds,british_crs), "remaining_AEDs")
  return((temp_grid$remaining_AEDs %*% coverage_matrix)[1,])
}

get_demand_to_meet.partial_rearrange <- function(percent_to_remove, filename){
  remaining_existing_coverage = get_remaining_coverage(percent_to_remove, filename)
  demand_to_meet = ifelse(LDN_grid$AED_demand.grad.capped -remaining_existing_coverage < 0,
                          0,
                          LDN_grid$AED_demand.grad.capped - remaining_existing_coverage)
  return(demand_to_meet)
}

# Capping Values ----------------------------------------------------------

## Grad ------
LDN_grid <- mutate(LDN_grid, "AED_demand.grad.capped" =
                     ifelse(AED_demand.grad.detrended < 0, 0, AED_demand.grad.detrended))
chance_of_survival <- 0.513
top_cap <- 3 + (8 * 3 * chance_of_survival)
LDN_grid <- mutate(LDN_grid, "AED_demand.grad.capped" =
                     ifelse(AED_demand.grad.capped > top_cap, top_cap, AED_demand.grad.capped))

# adding rhs demand matrices for adding to the existing distribution
LDN_grid <- mutate(LDN_grid, "AED_demand.grad.additional" =
                     ifelse(AED_demand.grad.capped - count_AEDs_gradated < 0,
                            0,
                            AED_demand.grad.capped - count_AEDs_gradated))
plot(LDN_grid["AED_demand.grad.additional"])

# Testing
(min(LDN_grid$AED_demand.diag.capped) >= 0) && (max(LDN_grid$AED_demand.diag.capped) <= 3)
(min(LDN_grid$AED_demand.grad.capped) >= 0) && (max(LDN_grid$AED_demand.grad.capped) <= top_cap)

# Saving London -----------------------------------------------------------

write_sf(LDN_grid, "outputs/04_locations/data/capped_data_ldn.gpkg")

# Creating Grad LP Matrices ----------------------------------------------------

# Changing crs for later function
british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

LDN_grid <- st_transform(LDN_grid, lon_lat_crs)

truncated_grid = LDN_grid

#truncated_grid = LDN_grid[5001:6000,] # used to play with smaller grids
plot(truncated_grid$geom)
ggplot() +
  geom_sf(data = truncated_grid, aes(fill=as.character(ID)))


# Making a list of grid center coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

total_cap.rhs = 3  # updated in following python file

LDN_grid<- st_transform(LDN_grid, british_crs)
AED_sf <- st_transform(AED_sf, british_crs)

# Creating and saving partial rearrangement matrices
for(i in 1:5){
  pc = i * 10
  fulfilled.constraint.rhs.rearrange = get_demand_to_meet.partial_rearrange(pc, paste("removed_aed", pc, "pc", sep=""))
  constraint.rhs.rearrange = c(total_cap.rhs, fulfilled.constraint.rhs.rearrange )
  write.csv(constraint.rhs.rearrange, paste("outputs/04_locations/data/LP_exports/constraint_rhs_grad_rearrange",
                                            pc, "pc.csv", sep=""))
}


