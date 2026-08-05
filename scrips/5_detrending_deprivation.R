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

LDN_grid <- read_sf("data/Regressed_data_ldn_2026_08_04.gpkg")


# Setting up Model - Change to read from file -----------------------------

final_coefs <- read.csv("results/regression_results/selected_model.csv")

# Detrending --------------------------------------------------------------

dpr_coef <- final_coefs[(final_coefs$variable == 
                           "LDN_grid$avg_hos_dpr.scaled"),]$Estimate

# Detrending our demand proxy
LDN_grid$count_AEDs.detrended <- LDN_grid$count_AEDs - 
  (dpr_coef * LDN_grid$avg_hos_dpr.scaled)

# Testing Correlation
cor(LDN_grid$AED_demand,  LDN_grid$avg_hos_dpr.scaled)
cor(LDN_grid$count_AEDs.detrended,  LDN_grid$avg_hos_dpr.scaled)

# Plotting result
plotRegresssion("count_AEDs.detrended", "AEDs using CAR model DETREND", max_relevant_val = 6,
                map = LDN_grid)

# EDIT - to match the final selected formula and neighbourhood
detrend_formula <- LDN_grid$count_AEDs.detrended ~
  LDN_grid$pc_f.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_50_plus.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_bad_gh.scaled + 
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled * LDN_grid$pop_den.scaled+ 
  LDN_grid$count_CHs.scaled

rook_shared_edge.nb <- poly2nb(LDN_grid,queen=FALSE)
A.rook_shared_edge <- nb2listw(rook_shared_edge.nb,style="B", zero.policy = TRUE)


# Regression of the demand against deprivation to show there is now no relation, 
# when accounting for medically relevant variables

detrend_regression_solution <- spatialreg::spautolm(formula = detrend_formula, 
                                                    data = LDN_grid, 
                                                    listw = A.rook_shared_edge, 
                                                    family = "CAR",
                                                    method = "Matrix_J")

# Showing there is now no trend with household deprivation
summary(detrend_regression_solution)

# Finding % change between models 
percent_change <- ((final_coefs$Estimate - unname(detrend_regression_solution$fit$coefficients)) / 
  final_coefs$Estimate) * 100 

percent_change


# Attaching fitted values 
LDN_grid$AED_demand.detrended <- detrend_regression_solution$fit$fitted.values


# Checking the means are the same (since we scaled to zero mean)
print(mean(LDN_grid$AED_demand) - mean(LDN_grid$AED_demand.detrended))


# Plotting result
plotRegresssion("AED_demand.detrended", "Demand Detrended", 
                max_relevant_val = 6, map = LDN_grid)
plotRegresssion("AED_demand", "Demand", 
                max_relevant_val = 6, map = LDN_grid)


# Saving Model ------------------------------------------------------------

write_sf(LDN_grid, "data/detrended_data_ldn_2026_08_05.gpkg")
#gpkg file allows for more than 10 character file names 
