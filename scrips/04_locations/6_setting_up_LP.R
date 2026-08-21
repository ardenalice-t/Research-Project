# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(dplyr)
library(maxcovr) # for the custom function + cite for code adapted from
library(ggplot2)

## Functions ---------------------------------------------------------------

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}


## Files -------------------------------------------------------------------

LDN_grid <- read_sf("outputs/03_detrending/data/detrended_data_ldn.gpkg")

# Capping Values ----------------------------------------------------------

LDN_grid <- mutate(LDN_grid, "AED_demand.diag.capped" =
                     ifelse(AED_demand.diag.detrended < 0, 0, AED_demand.diag.detrended))
LDN_grid <- mutate(LDN_grid, "AED_demand.diag.capped" =
                     ifelse(AED_demand.diag.capped > 3, 3, AED_demand.diag.capped))

LDN_grid <- mutate(LDN_grid, "AED_demand.diag.additional" =
                     ifelse(AED_demand.diag.capped - count_AEDs < 0,
                            0,
                            AED_demand.diag.capped - count_AEDs))
plot(LDN_grid["AED_demand.diag.additional"])



LDN_grid <- mutate(LDN_grid, "AED_demand.grad.capped" =
                     ifelse(AED_demand.grad.detrended < 0, 0, AED_demand.grad.detrended))
chance_of_survival <- 0.513
top_cap <- 3 + (8 * 3 * chance_of_survival)
LDN_grid <- mutate(LDN_grid, "AED_demand.grad.capped" =
                     ifelse(AED_demand.grad.capped > top_cap, top_cap, AED_demand.grad.capped))

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


# Finding max distance from centre ----------------------------------------

# Finding maximum distance from centre - to judge if i should use centres as the plotting points
center <- st_centroid(truncated_grid)
normal_area <- median(st_area(truncated_grid)) - 20
weird_regions <- as.numeric(st_area(truncated_grid)) < normal_area
weird_regions <- truncated_grid[weird_regions,]

boundary_points <- st_cast(weird_regions, "POINT")
relevent_centres <- st_is_within_distance(center, boundary_points,dist = 800)
relevant_TF <- c()
i=1
for (list in relevent_centres){
  relevant_TF[i] <- (length(list) > 0)
  i =i+1
}
relevent_centres <- center[relevant_TF,]

plot(truncated_grid$geom)
plot(boundary_points$geom, col = "red", add=TRUE)
distances <- st_distance(boundary_points, relevent_centres)
max_distance <- max(apply(distances, MARGIN = 1, min))
max_distance # output: [1] 220.4549


# Creating Diag LP Matrices ----------------------------------------------------

# Changing crs for later function
british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

LDN_grid <- st_transform(LDN_grid, lon_lat_crs)

truncated_grid = LDN_grid

# truncated_grid = LDN_grid[2001:3000,] # used to play with smaller grids
 plot(truncated_grid$geom)
# ggplot() +
#   geom_sf(data = truncated_grid, aes(fill=as.character(ID)))


# Making a list of grid center coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

# Creating a coverage matrix
coverage_matrix <- diag(1, Nlocations, Nlocations)

head(coverage_matrix)

# Making objective function
objective.fn = c(rep(0, Nlocations), rep(1, Nlocations))

# Making constraint matrix
total_cap = rep(1, Nlocations)
total_cap = c(total_cap, rep(0, Nlocations))
total_cap.rhs = 3  # CHANGE TO CHANGE TOTAL

fulfilled.constraint <- cbind(coverage_matrix,
                              diag(1,nrow = Nlocations))
fulfilled.constraint.rhs = truncated_grid$AED_demand.diag.capped

fulfilled.constraint.rhs.additional = truncated_grid$AED_demand.diag.additional

# Combining Matrices
constraint.mat = rbind(total_cap, fulfilled.constraint)
constraint.rhs = c(total_cap.rhs, fulfilled.constraint.rhs )
constraint.rhs.additional = c(total_cap.rhs, fulfilled.constraint.rhs.additional )


## Saving Matrices ---------------------------------------------------------

write.csv(constraint.mat, "outputs/04_locations/data/LP_exports/constraint_mat_diag.csv")
write.csv(constraint.rhs, "outputs/04_locations/data/LP_exports/constraint_rhs_diag.csv")
write.csv(constraint.rhs.additional, "outputs/04_locations/data/LP_exports/constraint_rhs_diag_additional.csv")
write.csv(objective.fn, "outputs/04_locations/data/LP_exports/objective_fct.csv")


# Creating Grad LP Matrices ----------------------------------------------------

# Changing crs for later function
british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

LDN_grid <- st_transform(LDN_grid, lon_lat_crs)

truncated_grid = LDN_grid

#truncated_grid = LDN_grid[5001:6000,] # used to play with smaller grids
plot(truncated_grid$geom)
# ggplot() +
#   geom_sf(data = truncated_grid, aes(fill=as.character(ID)))


# Making a list of grid center coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

# LOADING COVERAGE MATRIX FROM FILE
total_coverage_matrix <- diag(1, nrow(london_grid_centers), nrow(london_grid_centers))
chance_of_survival <- 0.513 # calculated from Valenzuela et al, 10% every minute
# so the relative difference of 7 mins
max_distance <- 350 # at 6 km / hr in 7 mins can travel 700 m - then halved

partial_coverage_matrix <- binary_matrix_cpp(facility = london_grid_centers,
                                             user = london_grid_centers,
                                             distance_cutoff = 350)
partial_coverage_matrix <- (partial_coverage_matrix - total_coverage_matrix ) *
  chance_of_survival

coverage_matrix <- total_coverage_matrix + partial_coverage_matrix

head(coverage_matrix)



# Making objective function
objective.fn = c(rep(0, Nlocations), rep(1, Nlocations))

# Making constraint matrix
total_cap = rep(1, Nlocations)
total_cap = c(total_cap, rep(0, Nlocations))
total_cap.rhs = 3  # CHANGE TO CHANGE TOTAL

fulfilled.constraint <- cbind(coverage_matrix,
                              diag(1,nrow = Nlocations))
fulfilled.constraint.rhs = truncated_grid$AED_demand.grad.capped
fulfilled.constraint.rhs.additional = truncated_grid$AED_demand.grad.additional

# Combining Matrices
constraint.mat = rbind(total_cap, fulfilled.constraint)
constraint.rhs = c(total_cap.rhs, fulfilled.constraint.rhs )
constraint.rhs.additional = c(total_cap.rhs, fulfilled.constraint.rhs.additional )


## Saving Matrices ---------------------------------------------------------

write.csv(constraint.mat, "outputs/04_locations/data/LP_exports/constraint_mat_grad.csv")
write.csv(constraint.rhs, "outputs/04_locations/data/LP_exports/constraint_rhs_grad.csv")
write.csv(constraint.rhs.additional, "outputs/04_locations/data/LP_exports/constraint_rhs_grad_additional.csv")
write.csv(objective.fn, "outputs/04_locations/data/LP_exports/objective_fct.csv")



