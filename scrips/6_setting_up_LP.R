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

truncated_grid = LDN_grid[2001:2010,]
plot(truncated_grid$geom)

# Making a list of grid center coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

# Creating a coverage matrix 
# true if the distance satisifes the distance cut off condition. 
coverage_matrix <- binary_matrix_cpp(facility = london_grid_centers,
                                     user = london_grid_centers,
                                     distance_cutoff = 1) # this is working weird for some reason

# maybve use like within or between or something like this. 

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
fulfilled.constraint.rhs = truncated_grid$F1_mutated 

constraint.mat = rbind(total_cap, fulfilled.constraint)
constraint.dir = c(total_cap.dir, fulfilled.constraint.dir)
constraint.rhs = c(total_cap.rhs, fulfilled.constraint.rhs )



