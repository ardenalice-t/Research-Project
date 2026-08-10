# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(readr)
library(ggplot2)

## Functions ---------------------------------------------------------------

import_solution = function(filename, value_map = LDN_grid){
  print("--- Importing Solution ---")
  Nlocations <- nrow(LDN_grid)

  # reading csv
  solution <- read_csv(filename, col_names = FALSE, show_col_types = FALSE)

  # just getting number of placed AEDs
  solution_facsol = as.list(solution[1:Nlocations,])$X1
  print(paste("Number of AEDs Placed: ", sum(solution_facsol)))

  # Calculating remaining demand
  initial_demand <- value_map$AED_demand.capped
  remaining_demand <- solution[(Nlocations + 1):(2 *Nlocations),]$X1
  remaining_demand2 <- initial_demand - solution[1:Nlocations,]$X1
  remaining_demand2 = pmax(remaining_demand2, 0)
  sanity_check =( max(abs(remaining_demand - remaining_demand2)) < 1e-10)

  # Checking calculations have worked correctly
  print(paste("Sanity Check: ", sanity_check))
  if(sanity_check == FALSE){
    print(paste("Sum demand: ", sum(initial_demand)))
    print(paste("Sum end of solution: ", sum(remaining_demand)))
    print(paste("Sum subtracting solutions: ", sum(remaining_demand2)))
  }

  # Calculating % demand covered
  pc_demand_covered = (sum(initial_demand) - sum(remaining_demand2)) / sum(initial_demand) * 100
  print(paste("Demand covered: ", round(pc_demand_covered, digits = 2), "%"))

  # Returning full solution
  return (solution$X1)
}


solution_to_plot = function(facility_solution, facility_object, map, demand=TRUE,
                            num_bins=6, max_relevant_val=3, legend_title="demand"){
  facility_object$num_placed <- round(facility_solution)
  facility_object <- facility_object[(facility_object$num_placed  > 0 ),]
  if (demand==TRUE){
    ggplot() +
      geom_sf(data = LDN_boundary) +
      geom_sf(data = map, lwd=0.0001,
              aes(fill = AED_demand.capped)) +
      scale_fill_steps(breaks = seq(0, max_relevant_val, length = num_bins),
                       limit = c(0,10000),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)),
                       name = legend_title) +
      geom_sf(data=facility_object,
              size = 0.0001,alpha = 0.5,
              aes(colour=as.character(num_placed))) +
      scale_color_discrete(palette = c( "yellow", "orange", "red"), name = "Number of AEDs") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(LDN_boundary))
  }
  else{
    ggplot() +
      geom_sf(data = LDN_boundary) +
      geom_sf(data=facility_object,
              size = 0.0001,alpha = 0.5,
              aes(colour=as.character(num_placed))) +
      scale_color_discrete(palette = c( "yellow", "orange", "red"),
                           name = "Number of AEDs") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(LDN_boundary))
  }

}



## Files -------------------------------------------------------------------

LDN_grid <- read_sf("outputs/04_locations/data/capped_data_ldn.gpkg")

# Making Boundary Map -----------------------------------------------------

LDN_boundary <- st_cast(st_union(LDN_grid), "POLYGON")


# Reading in a Solution ---------------------------------------------------

solution_2062 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_2062AEDs_18159locations.csv")

solution_4124 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_4124AEDs_18159locations.csv")

solution_8248 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_8248AEDs_18159locations.csv")

# Diagonal Solutions

diag_solution_8248 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_8248AEDs_18159locations.csv")

diag_solution_4124 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_4124AEDs_18159locations.csv")

diag_solution_1650 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_1650AEDs_18159locations.csv")

old_solution_8248 <- read_csv("outputs/04_locations/data/LP_imports/solution_7479.csv",
                              col_names = FALSE, show_col_types = FALSE)$X1

solution_10_loc <- read_csv("outputs/04_locations/data/LP_imports/gradated_sol_3AEDs_10locations.csv",
                            col_names = FALSE, show_col_types = FALSE)$X1


# Plotting Solutions ------------------------------------------------------

solution_to_plot(facility_solution = diag_solution_8248[1:18159],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE)

solution_to_plot(facility_solution = diag_solution_1650[1:18159],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE)


# Remaining Demand --------------------------------------------------------

LDN_grid$remaining_demand <- diag_solution_1650[(18159 + 1):(18159 * 2)]

plot(LDN_grid["remaining_demand"])

# i think i show this very partial plot and be like look its weird maybe you need
# more of like an exponential
# error for the amount of demand notbeing met
# but then arguably is it better to not give a section the one that it needs or the
# three that it needs
# maybe its the number of people but thats taken into account with the number that
# they need
# so its interesting
# but does seem arbitrary
# arguably we want to decrease the distance to neared defib
# then segue into introducing the second model that helps with median response
# times of ambulance
# talk about time to acc defib but then accounting for getting decid eout of boz,
# calling wuould need to be done in both situations etc.

# Test manual solution ----------------------------------------------------

manual_test_sol <- read_csv("outputs/04_locations/data/LP_imports/solution_manual_test.csv")$x

facility_solution = manual_test_sol[1:18159]
facility_object = st_centroid(LDN_grid)
facility_object$num_placed <- round(facility_solution)
facility_object <- facility_object[(facility_object$num_placed  > 0 ),]

ggplot() +
  geom_sf(data = LDN_boundary) +
  geom_sf(data = LDN_grid, lwd=0.0001,
          aes(fill = AED_demand.capped)) +
  scale_fill_steps(breaks = seq(0, 15, length = 6),
                   limit = c(0,10000),
                   na.value = "light blue",
                   rescaler = ~ scales::rescale_max(.x, from =c(0,15)),
                   name = "legend_title") +
  geom_sf(data=facility_object,
          size = 0.0001,alpha = 0.5,
          aes(colour=as.character(num_placed))) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = st_crs(LDN_boundary))
