# Detrending the modelled 'AED Demand' so that household deprivation has no
# effect on the number of AEDs in an area.


# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(spatialreg)
library(spdep)
library(ggplot2)

## Functions ---------------------------------------------------------------

source("src/plotRegression.R")

detrend_col <- function(coefs, count_aed_col){
  dpr_coef <- coefs[(coefs$variable ==
                             "LDN_grid$avg_hos_dpr.scaled"),]$Estimate

  # Detrending our demand proxy
  return (LDN_grid[[count_aed_col]] -
    (dpr_coef * LDN_grid$avg_hos_dpr.scaled))
}

## Files -------------------------------------------------------------------

LDN_grid <- read_sf("outputs/02_regression/data/regressed_data_ldn.gpkg")


# Setting up Model - Change to read from file -----------------------------

final_coefs.diag <- read.csv("outputs/02_regression/data/selected_model_diag.csv")
final_coefs.grad <- read.csv("outputs/02_regression/data/selected_model_gradated.csv")

# Detrending Diag --------------------------------------------------------------

LDN_grid$count_AEDs.detrended <- detrend_col(coefs = final_coefs.diag,
                                             count_aed_col = "count_AEDs")


# Testing Correlation
cor(LDN_grid$AED_demand.diag,  LDN_grid$avg_hos_dpr.scaled)
cor(LDN_grid$count_AEDs.detrended,  LDN_grid$avg_hos_dpr.scaled)

# Plotting result
plotRegresssion("count_AEDs.detrended", "AEDs using CAR model DETREND", max_relevant_val = 6,
                map = LDN_grid)

# EDIT - to match the final selected formula and neighbourhood
detrend_formula <- LDN_grid$count_AEDs.detrended ~
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

# Regression of the demand against deprivation to show there is now no relation,
# when accounting for medically relevant variables

detrend_regression_solution <- spatialreg::spautolm(formula = detrend_formula,
                                                    data = LDN_grid,
                                                    listw = A.nearest4,
                                                    family = "CAR")

# Showing there is now no trend with household deprivation
summary(detrend_regression_solution)

# Finding % change between models
percent_change <- ((final_coefs.diag$Estimate - unname(detrend_regression_solution$fit$coefficients)) /
                     final_coefs.diag$Estimate) * 100

percent_change


# Attaching fitted values
LDN_grid$AED_demand.diag.detrended <- detrend_regression_solution$fit$fitted.values


# Checking the means are the same (since we scaled to zero mean)
print(mean(LDN_grid$AED_demand.diag) - mean(LDN_grid$AED_demand.diag.detrended))


# Plotting result
plotRegresssion("AED_demand.diag.detrended", "Demand Detrended",
                max_relevant_val = 6, map = LDN_grid)
plotRegresssion("AED_demand.diag", "Demand",
                max_relevant_val = 6, map = LDN_grid)


# Detrending Grad ---------------------------------------------------------

LDN_grid$count_AEDs_grad.detrended <-
  detrend_col(coefs = final_coefs.grad,
              count_aed_col = "count_AEDs_gradated")


# Testing Correlation
cor(LDN_grid$AED_demand.grad,  LDN_grid$avg_hos_dpr.scaled)
cor(LDN_grid$count_AEDs_grad.detrended,  LDN_grid$avg_hos_dpr.scaled)

# Plotting result
plotRegresssion("count_AEDs_grad.detrended", "AEDs using CAR model DETREND", max_relevant_val = 6,
                map = LDN_grid)

# EDIT - to match the final selected formula and neighbourhood
detrend_formula <- LDN_grid$count_AEDs_grad.detrended ~
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
percent_change <- ((final_coefs.grad$Estimate - unname(detrend_regression_solution$fit$coefficients)) /
                     final_coefs.grad$Estimate) * 100

percent_change


# Attaching fitted values
LDN_grid$AED_demand.grad.detrended <- detrend_regression_solution$fit$fitted.values


# Checking the means are the same (since we scaled to zero mean)
print(mean(LDN_grid$AED_demand.grad) - mean(LDN_grid$AED_demand.grad.detrended))


# Plotting result
plotRegresssion("AED_demand.grad.detrended", "Demand Detrended",
                max_relevant_val = 6, map = LDN_grid)
plotRegresssion("AED_demand.grad", "Demand",
                max_relevant_val = 6, map = LDN_grid)


# Saving Model ------------------------------------------------------------

write_sf(LDN_grid, "outputs/03_detrending/data/detrended_data_ldn.gpkg")
#gpkg file allows for more than 10 character file names
