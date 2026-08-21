

# Regression --------------------------------------------------------------

# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(spdep)
library(spatialreg)
library(ggplot2)

## Functions ---------------------------------------------------------------

source("src/plot_descriptive_ldn.R")
source("src/plotRegression.R")

## Files -------------------------------------------------------------------

# Reading in grid map from 2_interpolating_to_grid.R
LDN_grid <- read_sf("outputs/02_regression/data/scaled_data_ldn.gpkg")


# Final Model Diag ------------------------------------------------------------

model_10 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pc_f.scaled

LDN_grid_centroid <- st_centroid(LDN_grid)
nearest4.nb <- knn2nb(knearneigh(LDN_grid_centroid,k=4), sym = TRUE)
A.nearest4 <- nb2listw(nearest4.nb,style="B", zero.policy = TRUE)

final_model <- model_10
final_matrix <- A.nearest4

car_output <- spautolm(formula = final_model,
                       data = LDN_grid, listw=final_matrix,
                       family="CAR",
                       method = "Matrix_J")

# Saving final result
unique_model_name = "selected_model_diag"
filename = paste("outputs/02_regression/data/", unique_model_name,
                 ".csv", sep="")

model_summary = summary(car_output)
coefs = as.data.frame(model_summary$Coef)
coefs$variable = names(model_summary$fit$coefficients)
coefs$formula = "model_10"
coefs$neighbours = "A.nearest4"
write.csv(coefs, filename)


# Attaching fitted values
LDN_grid$AED_demand.diag <- car_output$fit$fitted.values


plot_descriptive_ldn("AED_demand.diag",
                     legend_title = "Number of AEDs", map = LDN_grid,
                     cap=TRUE, max_val = 3, bins=5)
plotRegresssion("AED_demand.diag",title = "AED Demand", map = LDN_grid,
                max_relevant_val = 6)


# Final Model Grad -------------------------------------------------------------

# Need to redo to find best model
model_10 <- LDN_grid$count_AEDs_gradated ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pc_f.scaled

rook_shared_edge.nb <- poly2nb(LDN_grid,queen=FALSE)
A.rook_shared_edge <- nb2listw(rook_shared_edge.nb,style="B", zero.policy = TRUE)

final_model <- model_10
final_matrix <- A.rook_shared_edge

car_output.grad <- spautolm(formula = final_model,
                       data = LDN_grid, listw=final_matrix,
                       family="CAR",
                       method = "Matrix_J")

# Saving final result
unique_model_name = "selected_model_gradated"
filename = paste("outputs/02_regression/data/", unique_model_name,
                 ".csv", sep="")

model_summary = summary(car_output.grad)
coefs = as.data.frame(model_summary$Coef)
coefs$variable = names(model_summary$fit$coefficients)
coefs$formula = "model_10"
coefs$neighbours = "A.rook_shared_edge"
write.csv(coefs, filename)

# Attaching fitted values
LDN_grid$AED_demand.grad <- car_output.grad$fit$fitted.values

plotRegresssion("AED_demand.grad",title = "AED Demand", map = LDN_grid,
                max_relevant_val = 10)


# Saving Model ------------------------------------------------------------

write_sf(LDN_grid, "outputs/02_regression/data/regressed_data_ldn.gpkg")

