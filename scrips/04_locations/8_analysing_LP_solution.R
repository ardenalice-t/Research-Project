# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)

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

import_solution = function(filename, value_map = LDN_grid, grad= FALSE){
  sol_name = str_sub(filename, start=38, end=-5)
  print(paste("--- Importing Solution:", sol_name," ---"))
  Nlocations <- nrow(LDN_grid)

  # reading csv
  solution <- read_csv(filename, col_names = FALSE, show_col_types = FALSE)

  # just getting number of placed AEDs
  solution_facsol = as.list(solution[1:Nlocations,])$X1
  print(paste("Number of AEDs Placed: ", sum(solution_facsol)))

  remaining_demand_endsol <- solution[(Nlocations + 1):(2 *Nlocations),]$X1

  # Calculating remaining demand
  if(grad) initial_demand <- value_map$AED_demand.grad.capped
  if(!grad) {
    initial_demand <- value_map$AED_demand.diag.capped
    remaining_demand_subtract <- initial_demand - solution[1:Nlocations,]$X1
    remaining_demand_subtract = pmax(remaining_demand_subtract, 0)
    sanity_check =( max(abs(remaining_demand_endsol - remaining_demand_subtract)) < 1e-10)

    # Checking calculations have worked correctly
    print(paste("Sanity Check: ", sanity_check))
    if(sanity_check == FALSE){
      print(paste("Sum subtracting solutions: ", sum(remaining_demand_subtract)))
    }
  }
  print(paste("Sum demand: ", sum(initial_demand)))
  print(paste("Sum remaining demand: ", sum(remaining_demand_endsol)))

  # Calculating % demand covered
  pc_demand_covered = (sum(initial_demand) - sum(remaining_demand_endsol)) / sum(initial_demand) * 100
  print(paste("Demand covered: ", round(pc_demand_covered, digits = 2), "%"))

  # Returning full solution
  assign(sol_name, solution$X1, envir = .GlobalEnv)

  return (solution$X1)
}

get_diag_pc_demand_covered <- function(aed_sol){
  existing_remaining_demand <- pmax(LDN_grid$AED_demand.diag.capped - aed_sol,0)
  pc_demand_covered = (sum(LDN_grid$AED_demand.diag.capped) -
                         sum(existing_remaining_demand)) /
    sum(LDN_grid$AED_demand.diag.capped) * 100

  print(paste("Demand covered: ", round(pc_demand_covered, digits = 2), "%"))
  return(pc_demand_covered)
}

get_grad_pc_demand_covered <- function(aed_sol, coverage_matrix, plot=FALSE){
  test_map = LDN_grid
  demand_covered <- aed_sol %*% coverage_matrix
  existing_remaining_demand <- pmax(LDN_grid$AED_demand.grad.capped -
                                      demand_covered,0)
  pc_demand_covered = (sum(LDN_grid$AED_demand.grad.capped) -
                         sum(existing_remaining_demand)) /
    sum(LDN_grid$AED_demand.grad.capped) * 100
  if(plot){
    test_map$remaining_demand = existing_remaining_demand
    print(ggplot() + geom_sf(data= test_map, aes(fill=remaining_demand)))
  }
  print(paste("Demand covered: ", round(pc_demand_covered, digits = 2), "%"))
  return(pc_demand_covered)
}


solution_to_plot = function(facility_solution, facility_object, map, grad = FALSE,
                            demand=TRUE, additional=FALSE, existing_aeds = existing_aeds,
                            num_bins=6, max_relevant_val=3, legend_title="demand"){
  if(grad) map$AED_demand <- map$AED_demand.grad.capped
  if(!grad) map$AED_demand <- map$AED_demand.diag.capped

  facility_object$num_placed <- round(facility_solution)
  facility_object <- facility_object[(facility_object$num_placed  > 0 ),]
  facility_object$num_placed <- ifelse(facility_object$num_placed < 4,
                                       as.character(facility_object$num_placed),
                                       "4+")
  if (demand==TRUE){
    ggplot() +
      geom_sf(data = LDN_boundary) +
      geom_sf(data = map, lwd=0.0001,
              aes(fill = AED_demand)) +
      scale_fill_steps(breaks = seq(0, max_relevant_val, length = num_bins),
                       limit = c(0,10000),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)),
                       name = legend_title) +
      geom_sf(data=facility_object,
              size = 0.0001,alpha = 0.5,
              aes(colour=as.factor(num_placed))) +
      #scale_color_discrete(palette = c( "yellow", "orange", "red"), name = "Number of AEDs") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(LDN_boundary))
  }
  else{
    if(additional){
      ggplot() +
        geom_sf(data = LDN_boundary, fill="darkgray") +
        geom_sf(data=facility_object,
                size = 0.01,alpha = 1,
                aes(colour=as.character(num_placed))) +
        scale_color_discrete(palette = c( "red", "orange", "yellow", "lightyellow", "lightgray"),
                             name = "Number of AEDs") +
        geom_sf(data=AED_sf,
                size = 0.0001,alpha = 0.3,
                aes(colour=rep("Existing AED",nrow(AED_sf)))) +
        xlab("Longitude") +
        ylab("Latitude") +
        coord_sf(crs = st_crs(LDN_boundary)) +
        theme(legend.key = element_rect(fill = "darkgray")) +
        guides(colour = guide_legend(override.aes = list(size=3)))
    }
    else{
      ggplot() +
        geom_sf(data = LDN_boundary, fill="darkgray") +
        geom_sf(data=facility_object,
                size = 0.0001,alpha = 0.5,
                aes(colour=as.character(num_placed))) +
        scale_color_discrete(palette = c( "red", "orange", "yellow", "lightyellow"),
                             name = "Number of AEDs") +
        xlab("Longitude") +
        ylab("Latitude") +
        coord_sf(crs = st_crs(LDN_boundary)) +
        theme(legend.key = element_rect(fill = "darkgray")) +
        guides(colour = guide_legend(override.aes = list(size=3)))
    }
  }

}



## Files -------------------------------------------------------------------

LDN_grid <- read_sf("outputs/04_locations/data/capped_data_ldn.gpkg")

british_crs = "EPSG:27700"
lon_lat_crs = "EPSG:4326"

# Creating Coverage Matrix
ldn_grid_centers <- st_centroid(LDN_grid)
ldn_grid_centers <- st_transform(ldn_grid_centers, lon_lat_crs)
ldn_grid_centers <- sf_to_latlong_matix(ldn_grid_centers)
ldn_grid_centers <- as.matrix(ldn_grid_centers[ , c("lat", "long")])
total_coverage_matrix <- diag(1, nrow(LDN_grid), nrow(LDN_grid))
chance_of_survival <- 0.513
max_distance <- 350
partial_coverage_matrix <- maxcovr::binary_matrix_cpp(facility = ldn_grid_centers,
                                                      user = ldn_grid_centers,
                                                      distance_cutoff = 350)
partial_coverage_matrix <- (partial_coverage_matrix - total_coverage_matrix ) *
  chance_of_survival
coverage_matrix <- total_coverage_matrix + partial_coverage_matrix

# Making Boundary Map -----------------------------------------------------

LDN_boundary <- st_cast(st_union(LDN_grid), "POLYGON")


# Reading in a Solution ---------------------------------------------------

Nlocations = 16774

sol_files <- list.files(path="outputs/04_locations/data/LP_imports", pattern="*.csv",
                         full.names=TRUE, recursive=FALSE)

# Diagonal Solutions
for(sol_file in sol_files) {
  import_solution(sol_file)
}


# Current Distribution ----------------------------------------------------

existing_aeds <- LDN_grid$count_AEDs
AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")

get_diag_pc_demand_covered(existing_aeds)

get_grad_pc_demand_covered(existing_aeds, coverage_matrix)


# Assessing Partially Rearranged Distribution -----------------------------

getting_rearranged_distribution <- function(fileOfRemovedIndexes, solution,
                                            return_all_aeds = TRUE){
  removed_indexes = read_csv(fileOfRemovedIndexes)$x
  remaining_AEDs = AED_sf[-removed_indexes,]
  ldn_map <- join_points_to_ldn(st_transform(remaining_AEDs,british_crs), "remaining_AEDs")
  all_aeds = ldn_map$remaining_AEDs + solution
  if(return_all_aeds) return(all_aeds)
  if(!return_all_aeds) return(ldn_map$remaining_AEDs)
}

all_aeds = getting_rearranged_distribution("outputs/04_locations/data/LP_exports/removed_aed10pc.csv",
                                           gradated_sol_793AEDs_16774locationsrearranged10pc[1:Nlocations])

get_grad_pc_demand_covered(all_aeds, coverage_matrix)

# Coverage by % Used ------------------------------------------------------

diag_pc_covered = data.frame(pc_AEDs=c(0),
                             coverage=c(0))
add_diag_sol <- function(solution){
  to_add <- c(sum(solution) / num_existing_aeds * 100, get_diag_pc_demand_covered(solution))
  diag_pc_covered <<- rbind(diag_pc_covered, to_add)
}


add_diag_sol(diagional_sol_793AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_1585AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_2378AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_3170AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_3963AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_4756AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_5548AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_6341AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_7133AEDs_16774locations[1:Nlocations])
add_diag_sol(diagional_sol_7926AEDs_16774locations[1:Nlocations])

ggplot() +
  geom_line(data = diag_pc_covered, aes(x=pc_AEDs, y=coverage)) +
  geom_point(data = diag_pc_covered, aes(x=pc_AEDs, y=coverage)) +
  geom_hline(yintercept = get_diag_pc_demand_covered(existing_aeds),
             color = "red", linetype = 2) +
  annotate("text", x = 15, y = get_diag_pc_demand_covered(existing_aeds) + 5,
           label = "Current distribution's coverage", size=3, col="red") +
  xlab("Number of AEDs (% of current distribution)") +
  ylab("Demand Coverage (%)")

# Plotting Solutions ------------------------------------------------------

solution_to_plot(facility_solution = diagional_sol_7926AEDs_16774locations[1:Nlocations],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = FALSE,
                 additional=FALSE)

solution_to_plot(facility_solution = existing_aeds,
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = FALSE)

solution_to_plot(facility_solution = diagional_sol_3963AEDs_16774locations[1:Nlocations],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = FALSE,
                 additional=FALSE)

solution_to_plot(facility_solution = gradated_sol_793AEDs_16774locations_additional[1:Nlocations],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = FALSE,
                 additional=TRUE,
                 grad=TRUE)

solution_to_plot(facility_solution = gradated_sol_793AEDs_16774locationsrearranged10pc[1:Nlocations],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = FALSE,
                 additional=TRUE,
                 grad=TRUE,
                 existing_aeds =
                   getting_rearranged_distribution("outputs/04_locations/data/LP_exports/removed_aed10pc.csv",
                                                   gradated_sol_793AEDs_16774locationsrearranged10pc[1:Nlocations],
                                                   return_all_aeds = FALSE))

# Remaining Demand --------------------------------------------------------

LDN_grid$remaining_demand <- diag_solution_100pc[(Nlocations + 1):(Nlocations * 2)]

plot(LDN_grid["remaining_demand"])


# New Statistics ----------------------------------------------------------

get_filtered_placement_grid = function(solution, full=TRUE){
  # To get an sf of just the centres of grids with AEDs placed in them,
  # along with the number of AEDs in each of those grid squares

  if (full) solution <- solution[1:Nlocations]

  LDN_grid_centres <- st_centroid(LDN_grid)
  LDN_grid_centres$num_placed <- round(solution) # for overflow errors
  LDN_solution_centres <- LDN_grid_centres[(LDN_grid_centres$num_placed  > 0 ),]

  return(LDN_solution_centres)
}

LDN_solution_centres <- get_filtered_placement_grid(diagional_sol_7926AEDs_16774locations)


# Proportion within -------------------------------------------------------


find_pc_pop_uncovered = function(coverage_distance, AED_sites_sf, plot=TRUE, map = LDN_grid){
  map$OG_region_area = as.numeric(st_area(map))

  covered_zones = st_buffer(AED_sites_sf,dist = coverage_distance)

  not_covered_zones = st_difference(map, st_union(covered_zones))

  print(paste("Area removed: ", round(sum(st_area(map)) -
                                        sum(st_area(not_covered_zones)),
                                      digits =1)))

  uncovered_plot = ggplot() +
    geom_sf(data = st_union(map), fill="red", alpha = 0.7) +
    geom_sf(data = st_union(not_covered_zones), fill="gray")
  if(plot) print(uncovered_plot)

  not_covered_zones$area_proportion_kept = as.numeric(st_area(not_covered_zones)) /
    not_covered_zones$OG_region_area

  not_covered_zones$pop_not_covered = not_covered_zones$area_proportion_kept *
    (not_covered_zones$pop_den * as.numeric(st_area(not_covered_zones)) / 1000)

  pc_pop_uncovered = sum(not_covered_zones$pop_not_covered) /
    sum(LDN_grid$pop_den * as.numeric(st_area(LDN_grid)) / 1000)

  print(paste("% of population not covered:", round(pc_pop_uncovered * 100, digits = 2)))

  return(pc_pop_uncovered)
}

## Proportion covered at 350 m -----
# rearranged distribution
pc_pop_uncovered_sol = find_pc_pop_uncovered(coverage_distance = 350,
                             AED_sites_sf = LDN_solution_centres,
                             map=LDN_grid)

# existing distribuion
AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")
pc_pop_uncovered_existing = find_pc_pop_uncovered(coverage_distance = 350,
                             AED_sites_sf = AED_sf,
                             map=LDN_grid)

# partial rearranged distribution - grad model
pc_pop_uncovered_sol = find_pc_pop_uncovered(coverage_distance = 350,
                                             AED_sites_sf = get_filtered_placement_grid(getting_rearranged_distribution("outputs/04_locations/data/LP_exports/removed_aed10pc.csv",
                                                                                                                        gradated_sol_793AEDs_16774locationsrearranged10pc[1:Nlocations])),
                                             map=LDN_grid)

## Proportion covered at 212 m -----
# rearranged distribution
pc_pop_uncovered_sol = find_pc_pop_uncovered(coverage_distance = 212,
                                             AED_sites_sf = LDN_solution_centres,
                                             map=LDN_grid)

# existing distribuion
AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")
pc_pop_uncovered_existing = find_pc_pop_uncovered(coverage_distance = 212,
                                                  AED_sites_sf = AED_sf,
                                                  map=LDN_grid)



# % with aed in their square
sum(LDN_grid[LDN_grid$count_AEDs > 0,]$pop_den) / sum(LDN_grid$pop_den) #existing
sum(LDN_solution_centres$pop_den) / sum(LDN_grid$pop_den) # rearranged


## Finding the bin containing the median distance to AED -----
find_median_aed_distance_bin <- function(binsize, starting_distance, aed_locations){
  pc_uncovered = 1
  distance = starting_distance - binsize
  while(pc_uncovered > 0.5){
    distance = distance + binsize
    print(paste("---------- distance:", distance, "----------"))
    pc_uncovered = find_pc_pop_uncovered(coverage_distance = distance,
                                         AED_sites_sf = aed_locations,
                                         map=LDN_grid,
                                         plot=(distance %% 50 == 0))
    #print(paste("% uncovered:",pc_uncovered))
  }
}

# rearranged distribution
find_median_aed_distance_bin(binsize = 5,
                             starting_distance = 100,
                             aed_locations = LDN_solution_centres)

# existing distribution
find_median_aed_distance_bin(binsize = 5,
                             starting_distance = 100,
                             aed_locations = AED_sf)

# partial rearranged distribution - gradated model

find_median_aed_distance_bin(binsize = 5,
                            starting_distance = 100,
                            aed_locations =
                              get_filtered_placement_grid(getting_rearranged_distribution("outputs/04_locations/data/LP_exports/removed_aed10pc.csv",
                                                                                          gradated_sol_793AEDs_16774locationsrearranged10pc[1:Nlocations])))




