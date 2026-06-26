
# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(lpSolve)
library(sf)
library(readxl)
library(ggplot2)
library(dplyr) # for mutate 

## Functions ---------------------------------------------------------------


## Maps -------------------------------------------------------------------

LDN_grid_map <- read_sf("data/300grid_regressed.shp")


# Capping Values ----------------------------------------------------------

LDN_grid_map <- mutate(LDN_grid_map, "F1_mutated" = ifelse(F1_scld < 0, 0, F1_scld))
LDN_grid_map <- mutate(LDN_grid_map, "F1_mutated" = ifelse(F1_mutated > 3, 3, F1_mutated))

# Testing
(min(LDN_grid_map$F1_mutated) >= 0) && (max(LDN_grid_map$F1_mutated) <= 3)


# Linear Programming ------------------------------------------------------

existing =c(2,1,0)

objective.fn = c(1,-1)

constraint.mat = matrix(c(1,0, 0, 1), ncol=2)
constraint.dir = c("=", "<=")
constraint.rhs = c(2,1)

lp.solution <- lp(direction= "min",
                  objective.fn, 
                  constraint.mat,
                  constraint.dir,
                  constraint.rhs
                  )
lp.solution$solution



# Using maxcovr package ---------------------------------------------------

install.packages("maxcovr", repos = c("https://njtierney.r-universe.dev", "https://cloud.r-project.org"))
library(maxcovr)

york_selected <- york %>% filter(grade == "I")

york_unselected <- york %>% filter(grade != "I")
mc_20 <- max_coverage(existing_facility = york_selected,
                      proposed_facility = york_unselected,
                      user = york_crime,
                      n_added = 20,
                      distance_cutoff = 100)
test = york_crime
mc_20$facility_selected
plot(mc_20$facility_selected)

london_grid_centers = st_centroid(LDN_grid_map)

london_grid_centers.coordinates = as.data.frame(st_coordinates(london_grid_centers))
names(london_grid_centers.coordinates)= c("long","lat")

class(ldn_AEDs)
ldn_AED_data_sf =read_sf("data/LDN_AEDs/ldn_AEDs_map.shp")
ldn_AEDs = as.data.frame(st_coordinates(ldn_AED_data_sf))
names(ldn_AEDs)= c("long","lat")
ldn_AEDs = cbind(ldn_AEDs, ldn_AED_data_sf)

existing_facility <- data.frame(lat=double(),
                                long=double(),
                                stringsAsFactors=FALSE)

# Reading the greater London boundary map file 
ldn_boundary_map <- read_sf("maps/gla")
plot(st_geometry(ldn_boundary_map),border="darkgray")
ldn_boundary_map = st_transform(ldn_boundary_map, crs=4283)
# 4283 universal, 27700 uk

# Getting the borough maps
ldn_boroughs <- st_transform(lnd, crs=st_crs(ldn_boundary_map))


mc_20 <- max_coverage(existing_facility = london_grid_centers.coordinates[1,],
                      proposed_facility = london_grid_centers.coordinates,
                      user = ldn_AEDs,
                      n_added = 1000,
                      distance_cutoff = 300)


new_sites = mc_20$facility_selected[[1]]
test_new_aeds = st_as_sf(new_sites, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
test_new_aeds = st_transform(test_new_aeds, crs=st_crs(ldn_boundary_map))

# Plotting London AEDs
ggplot() + 
  geom_sf(data = ldn_boundary_map) +
  geom_sf(data=test_new_aeds,
          size = 0.0001,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(label = "Existing AED locations") + 
  coord_sf(crs = st_crs(ldn_boundary_map))


# working through manually #################
ldn_AEDs = as.data.frame(st_coordinates(ldn_AED_data_sf))
names(ldn_AEDs)= c("long","lat")

ldn_AEDs <- tibble::rowid_to_column(ldn_AEDs, var = "user_id")

london_grid_centers = st_centroid(LDN_grid_map)

london_grid_centers.coordinates = as.data.frame(st_coordinates(london_grid_centers))
names(london_grid_centers.coordinates)= c("long","lat")

facility_cpp <- as.matrix(london_grid_centers.coordinates[ , c("lat", "long")])

user_cpp <-as.matrix(ldn_AEDs[ , c("lat", "long")])

# true if the distance satisifes the distance cut off condition. 
A <- binary_matrix_cpp(facility = facility_cpp,
                       user = user_cpp,
                       distance_cutoff = 300)


colnames(A) <- 1:nrow(london_grid_centers.coordinates)
user_id_list <- 1:nrow(ldn_AEDs)
Nx <- nrow(A) # number of = number of users
Ny <- ncol(A) # number of locations
c_vec <- c(rep(0, Ny), rep(1, Nx)) #all of the users = aeds
d_vec <- c(rep(1, Ny), rep(0, Nx)) # all of the all of the locations 

# this is a line to optimise with cpp
Ain <- cbind(-A, diag(Nx))
bin <- matrix(rep(0, Nx), ncol = 1)

# matrix of constraint coefs, one row per constraint, one col per variable
constraint_matrix <- rbind(Ain, d_vec)
rhs_matrix <- rbind(bin, 500) # number to add 100

# Another line to optimise with c++
constraint_directions <- c(rep("<=", Nx), "==")


solution <- lpSolve::lp(direction = "max",
                        objective.in = c_vec, # objective_in,
                        const.mat = constraint_matrix,
                        const.dir = constraint_directions,
                        const.rhs = rhs_matrix, # constraint_rhs,
                        transpose.constraints = TRUE,
                        all.bin = TRUE,
                        num.bin.solns = 1,
                        use.rw = TRUE)

# coerce to integer to save space
solution$solution <- as.integer(solution$solution)
solution_vector = solution$solution
A_mat = A

I <- ncol(A_mat)

facility_solution <- solution_vector[1:I]

facility_id <- readr::parse_number(colnames(A_mat))


# Which facilities were selected -------------------------------------------
facility_temp <- tibble::tibble(facility_id = facility_id,
                                facility_chosen = facility_solution) |>
  dplyr::filter(facility_chosen == 1)

# join these back on.
facility_selected <- london_grid_centers.coordinates |>
  dplyr::mutate(facility_id = facility_id) |>
  dplyr::filter(facility_id %in% facility_temp$facility_id) |>
  # drop facility_id as it is not needed anymore
  dplyr::select(-facility_id)

SOLUTION_FACILITIES = st_as_sf(facility_selected, coords = c("long", "lat"), 
                               crs=st_crs(ldn_boundary_map))
ggplot() + 
  geom_sf(data = ldn_boundary_map) +
  geom_sf(data=SOLUTION_FACILITIES,
          size = 0.0001,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(label = "Existing AED locations") + 
  coord_sf(crs = st_crs(ldn_boundary_map))


# Testing with one AED ----------------------------------------------------

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}

two_AED = sf_to_latlong_matix(ldn_AED_data_sf[1:15,])
two_AED <- tibble::rowid_to_column(two_AED, var = "user_id")

london_grid_centers = st_centroid(LDN_grid_map[3194:3200,])
london_grid_centers = sf_to_latlong_matix(london_grid_centers)

facility_cpp <- as.matrix(london_grid_centers[ , c("lat", "long")])
user_cpp <-as.matrix(two_AED[ , c("lat", "long")])

# true if the distance satisifes the distance cut off condition. 
coverage_check <- binary_matrix_cpp(facility = facility_cpp,
                       user = user_cpp,
                       distance_cutoff = 300)

colnames(coverage_check) <- 1:nrow(london_grid_centers)
user_id_list <- 1:nrow(two_AED)

Nusers <- nrow(coverage_check) # number of = number of users
Nfacility <- ncol(coverage_check) # number of locations

user_vec <- c(rep(0, Nfacility), rep(1, Nusers)) #all of the users = aeds
AUGMENTED_user_vec  <- c(rep(0, Nfacility), rep(1, 13),10,1) # thi is the one that i need to make into demand
AUGMENTED_user_vec2  <- c(rep(0, Nfacility), rep(1, 13),10,1) 

location_vec <- c(rep(1, Nfacility), rep(0, Nusers)) # one for each of the locations
# so that the chosen locaitons have to add to the correct amount

# this is a line to optimise with cpp
Ain <- cbind(-coverage_check, diag(Nusers)) # user - the locations that cover it, has to => 0
bin <- matrix(rep(0, Nusers), ncol = 1) # 0 to make sure the covered locations - the number of aeds is 0? 

# matrix of constraint coefs, one row per constraint, one col per variable
constraint_matrix <- rbind(Ain, location_vec)
rhs_matrix <- rbind(bin, 1) # number to add 100
constraint_directions <- c(rep("<=", Nusers), "==")


solution <- lpSolve::lp(direction = "max",
                        objective.in = AUGMENTED_user_vec, # objective_in,
                        const.mat = constraint_matrix,
                        const.dir = constraint_directions,
                        const.rhs = rhs_matrix, # constraint_rhs,
                        transpose.constraints = TRUE,
                        all.bin = TRUE,
                        num.bin.solns = 1,
                        use.rw = TRUE)

solution_vector =solution$solution

facility_solution2 <- solution_vector[1:Nfacility]

solution_to_plot(A_mat=coverage_check, facility_solution = facility_solution, 
                 facility_object = london_grid_centers)

solution_to_plot = function(A_mat, facility_solution, facility_object){
  facility_id <- readr::parse_number(colnames(A_mat))
  facility_temp <- tibble::tibble(facility_id = facility_id,
                                  facility_chosen = facility_solution) |>
    dplyr::filter(facility_chosen == 1)
  
  # join these back on.
  facility_selected <- facility_object |>
    dplyr::mutate(facility_id = facility_id) |>
    dplyr::filter(facility_id %in% facility_temp$facility_id) |>
    # drop facility_id as it is not needed anymore
    dplyr::select(-facility_id)
  
  SOLUTION_FACILITIES = st_as_sf(facility_selected, coords = c("long", "lat"), 
                                 crs=st_crs(ldn_boundary_map))
  ggplot() + 
    geom_sf(data = ldn_boundary_map) +
    geom_sf(data=SOLUTION_FACILITIES,
            size = 0.0001,alpha = 0.5,
            colour="red") +
    xlab("Longitude") +
    ylab("Latitude") +
    #ggtitle(label = "Existing AED locations") + 
    coord_sf(crs = st_crs(ldn_boundary_map))
}


# Real Data ---------------------------------------------------------------
# trying with real data 
london_grid_centers = st_centroid(LDN_grid_map[3100:3200,])
london_grid_centers = sf_to_latlong_matix(london_grid_centers)

facility_cpp <- as.matrix(london_grid_centers[ , c("lat", "long")])
user_cpp <-as.matrix(london_grid_centers[ , c("lat", "long")])

# true if the distance satisifes the distance cut off condition. 
coverage_check <- binary_matrix_cpp(facility = facility_cpp,
                                    user = user_cpp,
                                    distance_cutoff = 300)

colnames(coverage_check) <- 1:nrow(london_grid_centers)
user_id_list <- 1:nrow(london_grid_centers)

Nusers <- nrow(coverage_check) # number of = number of users
Nfacility <- ncol(coverage_check) # number of locations

demand_vector = LDN_grid_map[3100:3200,]$F1_mutated
AUGMENTED_user_vec2  <- c(rep(0, Nfacility), demand_vector) 

location_vec <- c(rep(1, Nfacility), rep(0, Nusers)) # one for each of the locations
# so that the chosen locaitons have to add to the correct amount

# this is a line to optimise with cpp
Ain <- cbind(-coverage_check, diag(demand_vector)) # user - the locations that cover it, has to => 0
bin <- matrix(rep(0, Nusers), ncol = 1) # 0 to make sure the covered locations - the number of aeds is 0? 

# matrix of constraint coefs, one row per constraint, one col per variable
constraint_matrix <- rbind(Ain, location_vec)
rhs_matrix <- rbind(bin, 100) # number to add 100
constraint_directions <- c(rep("<=", Nusers), "==")


solution <- lpSolve::lp(direction = "max",
                        objective.in = AUGMENTED_user_vec2, # objective_in,
                        const.mat = constraint_matrix,
                        const.dir = constraint_directions,
                        const.rhs = rhs_matrix, # constraint_rhs,
                        transpose.constraints = TRUE,
                        all.bin = FALSE,
                        use.rw = TRUE)

solution_vector =solution$solution

facility_solution <- solution_vector[1:Nfacility]

mutated_facility_solution <-pmin(facility_solution, 1)

solution_to_plot(A_mat=coverage_check, facility_solution = mutated_facility_solution, 
                 facility_object = london_grid_centers)
