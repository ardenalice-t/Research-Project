# Detrending the modelled 'AED Demand' so that household deprivation has no 
# effect on the number of AEDs in an area. 


# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(spatialreg)
library(spdep)
library(ggplot2)

## Functions ---------------------------------------------------------------

source("functions/plotRegression.R")

## Files -------------------------------------------------------------------

LDN_grid_map <- read_sf("data/grids/ldn_grid_fitted.shp")


# Setting up Model - Change to read from file -----------------------------

# Rescaling all relevant variables
LDN_grid_map$pc_f.scaled = scale(LDN_grid_map$pc_f)
LDN_grid_map$pc_50_p.scaled = scale(LDN_grid_map$pc_50_p)
LDN_grid_map$pc_65_p.scaled = scale(LDN_grid_map$pc_65_p)
LDN_grid_map$pc_bd_g.scaled = scale(LDN_grid_map$pc_bd_g)
LDN_grid_map$avg_dpr.scaled = scale(LDN_grid_map$avg_dpr)
LDN_grid_map$pop_den.scaled = scale(LDN_grid_map$pop_den)
LDN_grid_map$WD_pp_d.scaled = scale(LDN_grid_map$WD_pp_d)
LDN_grid_map$cnt_spr.scaled = scale(LDN_grid_map$cnt_spr)
LDN_grid_map$cnt_CHs.scaled = scale(LDN_grid_map$cnt_CHs)

distance1km_nb <- spdep::dnearneigh(st_centroid(LDN_grid_map), d1=0, d2=1)
distance1km_A <- spdep::nb2listw(distance1km_nb, style="B", zero.policy = TRUE)

# want this to be a file at some point
model_10_formula <- LDN_grid_map$cnt_AED ~ 
  LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_bd_g.scaled + 
  LDN_grid_map$avg_dpr.scaled +
  LDN_grid_map$WD_pp_d.scaled + 
  LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$cnt_CHs.scaled

model_10_solution <- spatialreg::spautolm(formula = model_10_formula,
                                          data = LDN_grid_map, 
                                          listw = distance1km_A, 
                                          family = "CAR",
                                          method = "Matrix_J")

# Detrending --------------------------------------------------------------

dpr_coef <- coef(model_10_solution)[["LDN_grid_map$avg_dpr.scaled"]]

LDN_grid_map$demand <- fitted(model_10_solution)
min(fitted(model_10_solution) == model_10_solution$fit$fitted.values)

LDN_grid_map$demand_detrended <- fitted(model_10_solution) - 
  (dpr_coef * LDN_grid_map$avg_dpr.scaled)
LDN_grid_map$AED_count_detrended <- LDN_grid_map$cnt_AED- 
  (dpr_coef * LDN_grid_map$avg_dpr.scaled)

cor(LDN_grid_map$demand,  LDN_grid_map$avg_dpr.scaled)
cor(LDN_grid_map$demand_detrended,  LDN_grid_map$avg_dpr.scaled)
cor(LDN_grid_map$AED_count_detrended,  LDN_grid_map$avg_dpr.scaled)

# Plotting result
plotRegresssion("demand_detrended", "AEDs using CAR model (n=1) DETREND", max_relevant_val = 6,
                map = LDN_grid_map)

detrend_formula <- LDN_grid_map$demand_detrended ~ 
  LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_bd_g.scaled + 
  LDN_grid_map$avg_dpr.scaled +
  LDN_grid_map$WD_pp_d.scaled + 
  LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$cnt_CHs.scaled

detrend_formula2 <- LDN_grid_map$AED_count_detrended ~ 
  LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_bd_g.scaled + 
  LDN_grid_map$avg_dpr.scaled +
  LDN_grid_map$WD_pp_d.scaled + 
  LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$cnt_CHs.scaled

test_formula <-LDN_grid_map$demand ~ 
  LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$pc_bd_g.scaled + 
  LDN_grid_map$avg_dpr.scaled +
  LDN_grid_map$WD_pp_d.scaled + 
  LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled + 
  LDN_grid_map$cnt_CHs.scaled

# Regression of the demand against deprivation to show there is now no relation, 
# when accounting for medically relevant variables
detrend_regression_solution <- spatialreg::spautolm(formula = detrend_formula, 
                                                    data = LDN_grid_map, 
                                                    listw = distance1km_A, 
                                                    family = "CAR",
                                                    method = "Matrix_J")

detrend2_regression_solution <- spatialreg::spautolm(formula = detrend_formula2, 
                                                    data = LDN_grid_map, 
                                                    listw = distance1km_A, 
                                                    family = "CAR",
                                                    method = "Matrix_J")

test_refit_car_solution <- spatialreg::spautolm(formula = test_formula, 
                                                data = LDN_grid_map, 
                                                listw = distance1km_A, 
                                                family = "CAR",
                                                method = "Matrix_J")

summary(detrend_regression_solution)
summary(detrend2_regression_solution)
summary(test_refit_car_solution)
summary(model_10_solution)

coef(detrend2_regression_solution)
coef(model_10_solution)

(coef(model_10_solution) - coef(detrend2_regression_solution)) / 
  coef(model_10_solution) 

