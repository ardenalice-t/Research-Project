n_aeds = 1000
n_locations = 10000
demand= rnorm(n_locations, 1, sd=2)
demand = ifelse(demand > 0, demand, 0)

coverage = diag(1, n_locations, n_locations)
coverage[abs(row(coverage) - col(coverage)) == 1] <- 0.5

placed_aeds = rep(0, n_locations)

remaining_demand <- demand

for (AED in 1:n_aeds){
  max_idx = which.max(remaining_demand)
  placed_aeds[max_idx] = placed_aeds[max_idx] + 1

  remaining_demand <- remaining_demand - coverage[max_idx,]
  remaining_demand <- ifelse(remaining_demand > 0, remaining_demand, 0)
}

#print("Original demand:"); print(demand)
#print("AEDs Given:"); print(placed_aeds)
#print("Remaining demand:"); print(remaining_demand)

print(paste("Original demand:", sum(demand)))
print(paste("Remaining demand:", sum(remaining_demand)))



#### REAL DATA
coverage <- read_csv("outputs/01_interpolation/data/gradated_coverage_matrix.csv")
demand <- sf::read_sf("outputs/04_locations/data/capped_data_gradated_ldn.gpkg")
demand = demand$AED_demand.capped
n_locations = length(demand)

placed_aeds = rep(0, n_locations)
n_aeds = 1000

remaining_demand <- demand
print("Start")

for (AED in 1:n_aeds){
  max_idx = which.max(remaining_demand)
  placed_aeds[max_idx] = placed_aeds[max_idx] + 1

  remaining_demand <- remaining_demand - coverage[max_idx,]
  remaining_demand <- ifelse(remaining_demand > 0, remaining_demand, 0)

  print(paste("Placed AED number", AED))
}

#print("Original demand:"); print(demand)
#print("AEDs Given:"); print(placed_aeds)
#print("Remaining demand:"); print(remaining_demand)

print(paste("Original demand:", sum(demand)))
print(paste("Remaining demand:", sum(remaining_demand)))
