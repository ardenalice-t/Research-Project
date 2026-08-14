n_aeds = 1000
n_locations = 18159
demand= rnorm(n_locations, 1, sd=2)
demand = ifelse(demand > 0, demand, 0)

coverage = diag(1, n_locations, n_locations)
coverage[abs(row(coverage) - col(coverage)) == 1] <- 0.5

placed_aeds = rep(0, n_locations)
reducable_demand <- demand %*% coverage
remaining_demand <- demand

coverage = Matrix::Matrix(coverage, sparse=T)

for (AED in 1:n_aeds){
  tic(paste("round:", AED))
  max_idx = which.max(reducable_demand)
  placed_aeds[max_idx] = placed_aeds[max_idx] + 1

  remaining_demand <- demand - coverage[max_idx,]

  reducable_demand <- reducable_demand - (coverage[max_idx,] %*% coverage)
  reducable_demand <- ifelse(reducable_demand > 0, reducable_demand, 0)
  toc()
}

remaining_demand <-demand - (placed_aeds %*% coverage)
remaining_demand <- ifelse(remaining_demand > 0, remaining_demand, 0)

print("Original demand:"); print(demand)
print("AEDs Given:"); print(placed_aeds)
print("Remaining demand:"); print(remaining_demand)

print(paste("Original demand:", sum(demand)))
print(paste("Remaining demand:", sum(remaining_demand)))

##### Plotting
library(sf)
facility_object = st_centroid(LDN_grid)
facility_object$num_placed <- round(placed_aeds)
facility_object <- facility_object[(facility_object$num_placed  > 0 ),]

ggplot() +
  geom_sf(data = LDN_boundary) +
  geom_sf(data=facility_object,
          size = 0.0001,alpha = 0.5,
          aes(colour=as.character(num_placed))) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = st_crs(LDN_boundary))



#### REAL DATA
coverage <- readr::read_csv("outputs/01_interpolation/data/gradated_coverage_matrix.csv")
coverage <- coverage[,-1]
coverage <- as.matrix(coverage)

trunc_coverage = coverage
trunc_demand = demand

demand <- sf::read_sf("outputs/04_locations/data/capped_data_gradated_ldn.gpkg")
coverage_mat = Matrix::Matrix(trunc_coverage, sparse=T)

demand = demand$AED_demand.detrended
n_locations = length(trunc_demand)


# redo each time
reducable_demand <- as.numeric(trunc_demand %*% trunc_coverage)

#LDN_grid$reducable_demand = reducable_demand
#plot(LDN_grid["reducable_demand"])
#ggplot() +
#  geom_sf(data = LDN_grid, aes(fill = reducable_demand))

placed_aeds = rep(0, n_locations)
n_aeds = 8000
satiated = rep(0, n_locations)

for (AED in 1:n_aeds){
  tic(paste("round:", AED))

  if(max(reducable_demand)<=0) print("All 0 or negative")

  remaining_options = ifelse(reducable_demand > 0 & satiated ==0, reducable_demand, 0)

  max_idx = which.max(remaining_options)

  if(placed_aeds[max_idx] == 4) {
    satiated[max_idx] = 1
    print(paste("satisfied region:", max_idx))}

  placed_aeds[max_idx] = placed_aeds[max_idx] + 1

  #remaining_demand <- demand - coverage_mat[max_idx,]

  reducable_demand <- reducable_demand - as.numeric(coverage_mat[max_idx,] %*% coverage_mat)
  toc()

  #plot_it(placed_aeds)
}

plot_it(placed_aeds)

remaining_demand <-demand - (placed_aeds %*% coverage_mat)
remaining_demand <- ifelse(remaining_demand > 0, remaining_demand, 0)

LDN_grid$remaining_demand = as.numeric(remaining_demand)
LDN_grid$reduced_demand = demand - LDN_grid$remaining_demand
LDN_grid$coverage = as.numeric(placed_aeds %*% coverage_mat)
plot(LDN_grid["coverage"])

print("Original demand:"); print(demand)
print("AEDs Given:"); print(placed_aeds)
print("Remaining demand:"); print(remaining_demand)

print(paste("Original demand:", sum(demand)))
print(paste("Remaining demand:", sum(remaining_demand)))


## Plot
library(ggplot2)

plot_it = function(placed_aeds){
  facility_object = st_centroid(LDN_grid)
  facility_object$num_placed <- round(placed_aeds)
  facility_object <- facility_object[(facility_object$num_placed  > 0 ),]

  cur_plot = ggplot() +
  geom_sf(data = LDN_grid, lwd=0.0001,
          aes(fill = remaining_demand)) +
  scale_fill_steps(breaks = seq(0, 5, length = 6),
                   limit = c(0,10000),
                   na.value = "light blue",
                   rescaler = ~ scales::rescale_max(.x, from =c(0,5)),
                   name = "legend_title") +
  geom_sf(data=facility_object,
          size = 0.0001,alpha = 0.5,
          aes(colour=as.character(num_placed))) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_sf(crs = st_crs(LDN_boundary))
  print(cur_plot)
}
