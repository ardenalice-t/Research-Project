# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(sf)
library(readr)
library(ggplot2)
library(stringr)

## Functions ---------------------------------------------------------------

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


solution_to_plot = function(facility_solution, facility_object, map, grad = FALSE,
                            demand=TRUE, additional=FALSE,
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
LDN_grid <- LDN_grid#[5001:6000,]

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

get_diag_pc_demand_covered(existing_aeds)


AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")

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
#add_diag_sol(diagional_sol_11889AEDs_16774locations[1:Nlocations])

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

# Remaining Demand --------------------------------------------------------

LDN_grid$remaining_demand <- diag_solution_100pc[(Nlocations + 1):(Nlocations * 2)]

plot(LDN_grid["remaining_demand"])


# New Statistic -----------------------------------------------------------

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
  # the units arent correct but it might not matter - if regions uniform

  pc_pop_uncovered = sum(not_covered_zones$pop_not_covered) /
    sum(LDN_grid$pop_den * as.numeric(st_area(LDN_grid)) / 1000)

  print(paste("% of population not covered:", round(pc_pop_uncovered * 100, digits = 2)))

  return(pc_pop_uncovered)
}

pc_pop_uncovered_sol = find_pc_pop_uncovered(coverage_distance = 350,
                             AED_sites_sf = LDN_solution_centres,
                             map=LDN_grid)


AED_sf = read_sf("outputs/01_interpolation/data/ldn_AEDs_map.gpkg")
pc_pop_uncovered_existing = find_pc_pop_uncovered(coverage_distance = 350,
                             AED_sites_sf = AED_sf,
                             map=LDN_grid)

# % with aed in their square
sum(LDN_grid[LDN_grid$count_AEDs > 0,]$pop_den) / sum(LDN_grid$pop_den) #existing
sum(LDN_solution_centres$pop_den) / sum(LDN_grid$pop_den) # rearranged

pc_uncovered = 1
distance = 100
while(pc_uncovered > 0.5){
  distance = distance + 20
  print(paste("---------- distance:", distance, "----------"))
  pc_uncovered = find_pc_pop_uncovered(coverage_distance = distance,
                                       AED_sites_sf = LDN_solution_centres,
                                       map=LDN_grid,
                                       plot=(distance %% 50 == 0))
  #print(paste("% uncovered:",pc_uncovered))
}

# for my solution, bin was 100-150 for 50%
# now 130-140

pc_uncovered = 1
distance = 169.5
while(pc_uncovered > 0.5){
  distance = distance + 1
  print(paste("---------- distance:", distance, "----------"))
  pc_uncovered = find_pc_pop_uncovered(coverage_distance = distance,
                                       AED_sites_sf = AED_sf,
                                       map=LDN_grid,
                                       plot=(distance %% 50 == 0))
  #print(paste("% uncovered:",pc_uncovered))
}
# came out as 150-200
# came out at 170-190 - much closer to 170




# this would be good ? 12 % not covered ?




# ARCHIVE -----------------------------------------------------------------

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
