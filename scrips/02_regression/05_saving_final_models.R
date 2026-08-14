

# Regression --------------------------------------------------------------

# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(spdep)
library(spatialreg)

## Functions ---------------------------------------------------------------

source("src/plot_descriptive_ldn.R")
source("src/plotRegression.R")

## Files -------------------------------------------------------------------

# Reading in grid map from 2_interpolating_to_grid.R
LDN_grid <- read_sf("outputs/02_regression/data/scaled_data_ldn.gpkg")


# Final Model Diag ------------------------------------------------------------

model_9 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  #LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_f.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_sports.scaled : LDN_grid$pop_den.scaled

rook_shared_edge.nb <- poly2nb(LDN_grid,queen=FALSE)
A.rook_shared_edge <- nb2listw(rook_shared_edge.nb,style="B", zero.policy = TRUE)


final_model <- model_9
final_matrix <- A.rook_shared_edge

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
coefs$formula = "model_9"
coefs$neighbours = "A.rook_shared_edge"
write.csv(coefs, filename)


# Attaching fitted values
LDN_grid$AED_demand.diag <- car_output$fit$fitted.values


plot_descriptive_ldn("AED_demand.diag",title = "AED Demand",
                     legend_title = "Number of AEDs", map = LDN_grid,
                     cap=TRUE, max_val = 3, bins=5)
plotRegresssion("AED_demand.diag",title = "AED Demand", map = LDN_grid,
                max_relevant_val = 6)


# Final Model Grad -------------------------------------------------------------

model_8 <- LDN_grid$count_AEDs_gradated ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_f.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_sports.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_CHs.scaled : LDN_grid$pop_den.scaled

final_model <- model_8
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
coefs$formula = "model_8"
coefs$neighbours = "A.rook_shared_edge"
write.csv(coefs, filename)

# Attaching fitted values
LDN_grid$AED_demand.grad <- car_output.grad$fit$fitted.values


plot_descriptive_ldn("AED_demand",title = "AED Demand",
                     legend_title = "Number of AEDs", map = LDN_grid,
                     cap=TRUE, max_val = 3, bins=5)
plotRegresssion("AED_demand.grad",title = "AED Demand", map = LDN_grid,
                max_relevant_val = 15)


# Saving Model ------------------------------------------------------------

write_sf(LDN_grid, "outputs/02_regression/data/regressed_data_ldn.gpkg")
