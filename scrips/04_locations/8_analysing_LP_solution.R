# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(readr)
library(ggplot2)

## Functions ---------------------------------------------------------------

import_solution = function(filename, value_map = LDN_grid, grad= FALSE){
  print("--- Importing Solution ---")
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
  return (solution$X1)
}


solution_to_plot = function(facility_solution, facility_object, map, grad = FALSE,
                            demand=TRUE,
                            num_bins=6, max_relevant_val=3, legend_title="demand"){
  if(grad) map$AED_demand <- map$AED_demand.grad.capped
  if(!grad) map$AED_demand <- map$AED_demand.diag.capped

  facility_object$num_placed <- round(facility_solution)
  facility_object <- facility_object[(facility_object$num_placed  > 0 ),]
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
LDN_grid <- LDN_grid#[5001:6000,]

# Making Boundary Map -----------------------------------------------------

LDN_boundary <- st_cast(st_union(LDN_grid), "POLYGON")


# Reading in a Solution ---------------------------------------------------

solution_2062 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_2062AEDs_18159locations.csv")

solution_4124 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_4124AEDs_18159locations.csv")

solution_8248 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_8248AEDs_18159locations.csv")

tiny_3 <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_3AEDs_10locations.csv",
                          grad=TRUE)
tiny_3_placement = tiny_3[1:10]

medium <- import_solution("outputs/04_locations/data/LP_imports/gradated_sol_100AEDs_1000locations.csv",
                          grad=TRUE)
medium_placement = medium[1:1000]


# Diagonal Solutions

diag_solution_8248 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_8248AEDs_18159locations.csv")

diag_solution_4124 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_4124AEDs_18159locations.csv")

diag_solution_1650 <- import_solution("outputs/04_locations/data/LP_imports/diag_sol_1650AEDs_18159locations.csv")

old_solution_8248 <- read_csv("outputs/04_locations/data/LP_imports/solution_7479.csv",
                              col_names = FALSE, show_col_types = FALSE)$X1

solution_10_loc <- read_csv("outputs/04_locations/data/LP_imports/gradated_sol_3AEDs_10locations.csv",
                            col_names = FALSE, show_col_types = FALSE)$X1

non_integer_8248 <- read_csv("outputs/04_locations/data/LP_imports/gradated_sol_8248AEDs_18159locations_non-integer.csv",
                              col_names = FALSE, show_col_types = FALSE)$X1
non_integer_placed <- non_integer_8248[1:18159]

sum(round(non_integer_placed))
non_integer_placed.rounded = round(non_integer_placed)


# Plotting Solutions ------------------------------------------------------

solution_to_plot(facility_solution = diag_solution_8248[1:18159],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE)
plot(LDN_grid["AED_demand.diag.capped"])

solution_to_plot(facility_solution = diag_solution_1650[1:18159],
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE)

solution_to_plot(facility_solution = medium_placement,
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE,
                 grad=TRUE,
                 max_relevant_val = 15)

solution_to_plot(facility_solution = non_integer_placed,
                 facility_object = st_centroid(LDN_grid),
                 map = LDN_grid,
                 demand = TRUE,
                 grad=TRUE,
                 max_relevant_val = 15)

medium_remains = medium[1001:2000]
round(LDN_grid$AED_demand.grad.capped[151:160], digits=2)
medium_placement[151:160]
round(medium_remains[151:160], digits=2)

LDN_grid$remaining_demand <- medium_remains

plot(LDN_grid["AED_demand.grad.capped"])
plot(facility_object["num_placed"], add=TRUE)

# Remaining Demand --------------------------------------------------------

LDN_grid$remaining_demand <- diag_solution_1650[(18159 + 1):(18159 * 2)]

plot(LDN_grid["remaining_demand"])


# New Statistic -----------------------------------------------------------

solution <- diag_solution_8248[1:18159]

LDN_grid_centres <- st_centroid(LDN_grid)
LDN_grid_centres$num_placed <- round(solution) # for overflow errors
LDN_solution_centres <- LDN_grid_centres[(LDN_grid_centres$num_placed  > 0 ),]

find_distance_to_nearest_aed = function(location_sf, AED_sites_sf = LDN_solution_centres){
  nearest_aed_idx = st_nearest_feature(location_sf, AED_sites_sf)
  distance = st_distance(AED_sites_sf[nearest_aed_idx,], location_sf)
  return(distance)
}

# Calculating distances to nearest aed from each centre
Nlocations = 18159
distances = c()
peoples = c()
for(grid_centre_idx in 1:Nlocations){
  distance = find_distance_to_nearest_aed(LDN_grid_centres[grid_centre_idx,])
  distances = c(distances, distance)
  people = st_area(LDN_grid[grid_centre_idx,]) * LDN_grid$pop_den[grid_centre_idx] / 1000
  peoples = c(peoples, people)
}
peoples
distances
hist(distances, breaks = 20) # hm not quite it would be diff
hist(people_weighted_distances, break = 20)

bin_nums = floor(distances / 1) + 1
bin_freqs = c()
for (i in 1:max(bin_nums)){
  new_bin_freq = sum(peoples[bin_nums == i])
  bin_freqs = c(bin_freqs, new_bin_freq)
}
medium_index = sum(bin_freqs) / 2
medium_index = medium_index - bin_freqs[1]
medium_index = medium_index - bin_freqs[2]
medium_index = medium_index - bin_freqs[3]
medium_index = medium_index - bin_freqs[4]
medium_index = medium_index - bin_freqs[5]

barplot(bin_freqs)

median(distances) # thus the median bin is 212 - 512 meatures

# Calculating number of people each distance from an AED
person_distance = (st_area(LDN_grid) * LDN_grid$pop_den * distances)

avg_distance = sum(person_distance) / sum(st_area(LDN_grid) * LDN_grid$pop_den)

print(avg_distance) # 146.0931

# Calculating avg distance per person for
AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")

Nlocations = 18159
distances_to_existingAED = c()
for(grid_centre_idx in 1:Nlocations){
  distance = find_distance_to_nearest_aed(LDN_grid_centres[grid_centre_idx,],
                                          AED_sites_sf = AED_sf)
  distances_to_existingAED = c(distances_to_existingAED, distance)
}
hist(distances_to_existingAED, breaks = 200)
median(distances_to_existingAED)

ggplot() +
  geom_sf(data = LDN_grid) +
  geom_sf(data=AED_sf,
          size = 0.1,alpha = 0.3,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = st_crs(LDN_grid))


bin_nums_existing = floor(distances_to_existingAED / 212) + 1
bin_freqs_existing = c()
for (i in 1:max(bin_nums_existing)){
  new_bin_freq = sum(peoples[bin_nums_existing == i])
  bin_freqs_existing = c(bin_freqs_existing, new_bin_freq)
}
medium_index = sum(bin_freqs_existing) / 2
medium_index = medium_index - bin_freqs_existing[1]
barplot(bin_freqs_existing)

# so maybe we say that both have the median person with an aed within their grid
# but we have increased the proportion within 350m ?
# which is important because of ambulance arrival times


# Proportion within -------------------------------------------------------

LDN_solution_centres[1,]
plot_points = function(points_sf, map=LDN_grid){
  ggplot() +
    geom_sf(data = map) +
    geom_sf(data=points_sf,
            size = 0.1,alpha = 0.3,
            colour="red") +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_sf(crs = st_crs(map))
}

buffer_zones = st_buffer(LDN_solution_centres,dist = 350)

LDN_grid$OG_region_area = as.numeric(st_area(LDN_grid))

not_covered_zones = st_difference(LDN_grid, st_union(buffer_zones))
sum(st_area(LDN_grid)) - sum(st_area(not_covered_zones))
ggplot() +
  geom_sf(data = not_covered_zones)


not_covered_zones$area_proportion_kept = as.numeric(st_area(not_covered_zones)) /
  not_covered_zones$OG_region_area

min(not_covered_zones$area_proportion_kept )

not_covered_zones$pop_not_covered = not_covered_zones$area_proportion_kept *
  not_covered_zones$pop_den

sum(not_covered_zones$pop_not_covered) / sum(LDN_grid$pop_den)

# this would be good ? 12 % not covered ?




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
