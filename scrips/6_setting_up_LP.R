# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(dplyr)
library(maxcovr) # for the custom function + cite for code adapted from

## Functions ---------------------------------------------------------------

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}


## Files -------------------------------------------------------------------

LDN_grid <- read_sf("data/detrended_data_ldn_2026_08_05.gpkg")

# Capping Values ----------------------------------------------------------

LDN_grid <- mutate(LDN_grid, "AED_demand.capped" = ifelse(AED_demand.detrended < 0, 0, AED_demand.detrended))
LDN_grid <- mutate(LDN_grid, "AED_demand.capped" = ifelse(AED_demand.capped > 3, 3, AED_demand.capped))

# Testing
(min(LDN_grid$AED_demand.capped) >= 0) && (max(LDN_grid$AED_demand.capped) <= 3)


# Constructing Matrices ---------------------------------------------------

british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

LDN_grid <- st_transform(LDN_grid, lon_lat_crs)

truncated_grid = LDN_grid[2001:2010,]
plot(truncated_grid$geom)

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

### for use in the ds coursework
furrr::future_map_dbl(split(distances, row(x)), min)


# Making a list of grid center coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

# Creating a coverage matrix 
# true if the distance satisifes the distance cut off condition. 
total_coverage_matrix <- binary_matrix_cpp(facility = london_grid_centers,
                                     user = london_grid_centers,
                                     distance_cutoff = 1) 
chance_of_survival <- 0.5
partial_coverage_matrix <- binary_matrix_cpp(facility = london_grid_centers,
                                           user = london_grid_centers,
                                           distance_cutoff = 700)
partial_coverage_matrix <- (partial_coverage_matrix - total_coverage_matrix )* chance_of_survival

coverage_matrix <- total_coverage_matrix + partial_coverage_matrix

# Making objective function
objective.fn = c(rep(0, Nlocations), rep(1, Nlocations))


# Making constraint matrix 

location_zero_matrix = matrix(0, nrow = Nlocations, ncol= Nlocations)

total_cap = rep(1, Nlocations)
total_cap = c(total_cap, rep(0, Nlocations))
total_cap.dir = rep("<=", 1)
total_cap.rhs = 100  # CHANGE TO CHANGE TOTAL

fulfilled.constraint <- cbind(coverage_matrix, 
                              diag(1,nrow = Nlocations))
fulfilled.constraint.dir <- rep(">=", Nlocations)
fulfilled.constraint.rhs = truncated_grid$AED_demand.capped 

constraint.mat = rbind(total_cap, fulfilled.constraint)
constraint.dir = c(total_cap.dir, fulfilled.constraint.dir)
constraint.rhs = c(total_cap.rhs, fulfilled.constraint.rhs )


# Saving Matrices ---------------------------------------------------------

write.csv(constraint.mat, "matrix_exports/constraint_mat_gradated.csv")
write.csv(constraint.rhs, "matrix_exports/constraint_rhs.csv")
write.csv(objective.fn, "matrix_exports/constraint_fct.csv")


