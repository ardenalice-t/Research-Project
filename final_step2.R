
# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

#library(lpSolve)
library(readr) # to read csv
library(sf) # to read sf
#library(readxl)
library(ggplot2)
library(dplyr) # for mutate 
library(maxcovr) # for the custom function + cite for code adapted from bniaryccp

## Maps -------------------------------------------------------------------

LDN_grid_map <- read_sf("data/grids/ldn_grid_fitted.shp")
ldn_boundary_map <- read_sf("maps/gla")
ldn_boundary_map = st_transform(ldn_boundary_map, crs=4283)


## Functions ---------------------------------------------------------------

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}


# Capping Values ----------------------------------------------------------

LDN_grid_map <- mutate(LDN_grid_map, "demand_mutated" = ifelse(fttd_vl < 0, 0, fttd_vl))
LDN_grid_map <- mutate(LDN_grid_map, "demand_mutated" = ifelse(demand_mutated > 3, 3, demand_mutated))

# Testing
(min(LDN_grid_map$demand_mutated) >= 0) && (max(LDN_grid_map$demand_mutated) <= 3)


# MCLP --------------------------------------------------------------------


## Making matrices ---------------------------------------------------------


truncated_grid = LDN_grid_map

# Making a list of grid centre coordinates
london_grid_centers <- st_centroid(truncated_grid)
london_grid_centers <- sf_to_latlong_matix(london_grid_centers)
london_grid_centers <- as.matrix(london_grid_centers[ , c("lat", "long")])
Nlocations <- nrow(london_grid_centers)

# Creating a coverage matrix 
# true if the distance satisifes the distance cut off condition. 
coverage_matrix <- binary_matrix_cpp(facility = london_grid_centers,
                                     user = london_grid_centers,
                                     distance_cutoff = 100)

# Making objective function
objective.fn = c(rep(0, Nlocations), rep(1, Nlocations))


# Making constraint matrix 

location_zero_matrix = matrix(0, nrow = Nlocations, ncol= Nlocations)

total_cap = rep(1, Nlocations)
total_cap = c(total_cap, rep(0, Nlocations))
total_cap.dir = rep("=", 1)
total_cap.rhs = 100  # CHANGE TO CHANGE TOTAL

fulfilled.constraint <- cbind(-coverage_matrix, 
                              diag(-1,nrow = Nlocations))
fulfilled.constraint.dir <- rep("<=", Nlocations)
fulfilled.constraint.rhs = - truncated_grid$demand_mutated 

constraint.mat = rbind(total_cap, fulfilled.constraint)
constraint.dir = c(total_cap.dir, fulfilled.constraint.dir)
constraint.rhs = c(total_cap.rhs, fulfilled.constraint.rhs )

write.csv(constraint.mat, "matrix_exports/final/constraint_mat.csv")
write.csv(constraint.dir, "matrix_exports/final/constraint.dir.csv")
write.csv(constraint.rhs, "matrix_exports/final/constraint.rhs.csv")
write.csv(objective.fn, "matrix_exports/final/objective.fn.csv")


## Performing LP -----------------------------------------------------------


# now import these matrices to google colab for use there

lp.solution <- lp(direction= "min",
                  objective.fn, 
                  constraint.mat,
                  constraint.dir,
                  constraint.rhs,
                  int.vec = 1:Nlocations
)

fac_sol = process_lp_result(lp.solution, print_sol_vec = FALSE, map = truncated_grid)

max(fac_sol)

solution_to_plot(fac_sol, st_centroid(truncated_grid), map=truncated_grid)


## Importing GC Solution ---------------------------------------------------

import_solution = function(filename, value_map = truncated_grid){
  print("--- Importing Solution ---")
  # reading csv
  solution <- read_csv(filename, col_names = FALSE, show_col_types = FALSE)
  
  # just getting number of placed AEDs
  solution_facsol = as.list(solution[1:Nlocations,])$X1
  print(paste("Number of AEDs Placed: ", sum(solution_facsol)))
  
  initial_demand <- value_map$demand_mutated
  remaining_demand <- solution[(Nlocations + 1):(2 *Nlocations),]
  remaining_demand2 <- initial_demand - solution[1:Nlocations,]$X1
  sanity_check = min(remaining_demand == remaining_demand2) == 1
  print(paste("Sanity Check: ", sanity_check))
  if(sanity_check == FALSE){
    print(paste("Sum demand: ", sum(initial_demand)))
    print(paste("Sum end of solution: ", sum(remaining_demand)))
    print(paste("Sum subtracting solutions: ", sum(remaining_demand2)))
  }
  
  
  pc_demand_covered = (sum(initial_demand) - sum(remaining_demand2)) / sum(initial_demand) * 100
  print(paste("% Demand covered: ", pc_demand_covered))
  #solution_to_plot(solution_facsol, st_centroid(truncated_grid), map=truncated_grid)
  return (solution$X1)
}

solution_to_plot = function(facility_solution, facility_object, map, demand=TRUE,
                            num_bins=6, max_relevant_val=3, legend_title="demand"){
  chosen_1facilities = facility_object$geometry[facility_solution==1]
  chosen_2facilities = facility_object$geometry[facility_solution==2]
  chosen_3facilities = facility_object$geometry[facility_solution==3]
  if (demand==TRUE){
    ggplot() + 
      geom_sf(data = ldn_boundary_map) +
      geom_sf(data = map, lwd=0.0001, 
              aes(fill = demand_mutated)) +
      scale_fill_steps(breaks = seq(0, max_relevant_val, length = num_bins),
                       limit = c(0,10000),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)), 
                       name = legend_title) + 
      geom_sf(data=chosen_1facilities,
              size = 0.1,alpha = 0.5,
              colour="yellow") +
      geom_sf(data=chosen_2facilities,
              size = 0.1,alpha = 0.5,
              colour="orange") +
      geom_sf(data=chosen_3facilities,
              size = 0.1,alpha = 0.5,
              colour="red") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(ldn_boundary_map))
  }
  else{
    ggplot() + 
      geom_sf(data = ldn_boundary_map) +
      geom_sf(data=chosen_1facilities,
              size = 0.1,alpha = 1,
              colour="yellow") +
      geom_sf(data=chosen_2facilities,
              size = 0.1,alpha = 1,
              colour="orange") +
      geom_sf(data=chosen_3facilities,
              size = 0.1,alpha = 1,
              colour="red") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(ldn_boundary_map))
  }
  
}

# reading csv
solution <- import_solution("matrix_exports/solution7500.csv")

solution_to_plot(facility_solution = solution, facility_object = LDN_grid_map,
                 map=LDN_grid_map, demand=TRUE)
