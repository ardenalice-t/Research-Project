# Packages ----------------------------------------------------------------

library(sf)
library(spatialreg)
library(ggplot2)
library(spdep)


# Reading Files -----------------------------------------------------------

# Reading the map file
LDN_grid_map.500 <- read_sf("data/grid_map_500.shp")
LDN_grid_map.400 <- read_sf("data/grid_map_400.shp")
LDN_grid_map.300 <- read_sf("data/grid_map_300.shp")

LDN_grid_map = LDN_grid_map.300
# created in gridding_data

colnames(LDN_grid_map)[4] <- "workday_popden"
colnames(LDN_grid_map)[6] <- "popden"


# Functions ---------------------------------------------------------------

source("functions/regression.allParams.R")
source("functions/plotRegression.R")


# Scaling -----------------------------------------------------------------

LDN_grid_map$f_pr.scaled = scale(LDN_grid_map$f_pr)
LDN_grid_map$cnt_AED.scaled = scale(LDN_grid_map$cnt_AED)
LDN_grid_map$ovr_50_.scaled = scale(LDN_grid_map$ovr_50_)
LDN_grid_map$bd_gh_p.scaled = scale(LDN_grid_map$bd_gh_p)
LDN_grid_map$cnt_spr.scaled = scale(LDN_grid_map$cnt_spr)
LDN_grid_map$workday_popden.scaled = scale(LDN_grid_map$workday_popden)
LDN_grid_map$popden.scaled = scale(LDN_grid_map$popden)
LDN_grid_map$hos_dpr.scaled = scale(LDN_grid_map$hos_dpr)


# Neighbours --------------------------------------------------------------

# neighbors found from distance from centers - anything within 1km 
distance.nb <- dnearneigh(st_centroid(LDN_grid_map), d1=0, d2=1)
A <- nb2listw(distance.nb,style="B", zero.policy = TRUE)

image(nb2mat(distance.nb,zero.policy=TRUE, style="B"))


# Regression - 1 depth ----------------------------------------------------

#grid_nb <- poly2nb(LDN_grid_map,queen=TRUE)
#A <- nb2listw(poly2nb(LDN_grid_map), style="B")

car.out <- spautolm(formula = LDN_grid_map$cnt_AED ~
                      LDN_grid_map$f_pr + 
                      LDN_grid_map$ovr_50_ + 
                      LDN_grid_map$bd_gh_p +
                      LDN_grid_map$cnt_spr +
                      LDN_grid_map$workday_popden+ 
                      LDN_grid_map$popden + 
                      LDN_grid_map$hos_dpr, 
                    data = LDN_grid_map, listw=A, family="CAR")


# Now scaled

grid_nb <- poly2nb(LDN_grid_map,queen=TRUE)
A <- nb2listw(poly2nb(LDN_grid_map), style="B")

car.out.scaled <- spautolm(formula = LDN_grid_map$cnt_AED ~
                      LDN_grid_map$f_pr.scaled + 
                      LDN_grid_map$ovr_50_.scaled + 
                      LDN_grid_map$bd_gh_p.scaled +
                      LDN_grid_map$cnt_spr.scaled +
                      LDN_grid_map$workday_popden.scaled+ 
                      LDN_grid_map$popden.scaled + 
                      LDN_grid_map$hos_dpr.scaled, 
                    data = LDN_grid_map, listw=A, family="CAR")
coef(car.out.scaled)
summary(car.out.scaled)
print(lm.out.scaled)
var()
var(residuals(car.out.scaled))

(coef(car.out.scaled)[8]* var(LDN_grid_map$hos_dpr)) + mean(LDN_grid_map$hos_dpr)

# Saving the model
saveRDS(car.out.scaled, file = "models/300_grid.full_scaled_regression.rds")
car.out.scaled = readRDS("models/300_grid.full_scaled_regression.rds")

LDN_grid_map$F1_scaled = fitted(car.out.scaled)
plot(LDN_grid_map["F1_scaled"])
median(LDN_grid_map$F1_scaled)

plotRegresssion("F1_scaled", "", num_bins=7, max_relevant_val = 3,legend_title = "AED 'demand'",
                map = LDN_grid_map)

length(LDN_grid_map$f_pr)


#checking a linear model

lm.out.scaled <- lm(formula = LDN_grid_map$cnt_AED ~
                             LDN_grid_map$f_pr.scaled + 
                             LDN_grid_map$ovr_50_.scaled + 
                             LDN_grid_map$bd_gh_p.scaled +
                             LDN_grid_map$cnt_spr.scaled +
                             LDN_grid_map$workday_popden.scaled+ 
                             LDN_grid_map$popden.scaled + 
                             LDN_grid_map$hos_dpr.scaled, 
                           data = LDN_grid_map,)
summary(lm.out.scaled)

# seeing if residualds are autocorrelated
# using morans I test

#https://www.youtube.com/watch?v=myqXQ1QBbOc

spdep::lm.morantest(
  model = lm.out.scaled,
  listw = A
)
# very small p value so yes autocorrelated - there is global autocorrealtion

image(A)

summary(car.out)
summary(car.rmPD.scaled)
summary(car.rmWD.scaled)

# both increase the AIC compared to car out so maybe just stick with this one 

car.out.testmethod <- spautolm(formula = LDN_grid_map$cnt_AED ~
                             LDN_grid_map$f_pr.scaled + 
                             LDN_grid_map$ovr_50_.scaled + 
                             LDN_grid_map$bd_gh_p.scaled +
                             LDN_grid_map$cnt_spr.scaled +
                             LDN_grid_map$workday_popden.scaled+ 
                             LDN_grid_map$popden.scaled + 
                             LDN_grid_map$hos_dpr.scaled, 
                           data = LDN_grid_map, listw=A, family="CAR",
                         method = "Matrix_J")

loaded_model1 = readRDS("models/300_grid_linearmodel.rds")
saveRDS(lm.out.scaled, file = "models/300_grid_linearmodel.rds")
summary(loaded_model)


# Detrending Deprivation --------------------------------------------------

LDN_grid_map$F1_detrend = fitted(car.out.scaled) - 
  coef(car.out.scaled)[8] * LDN_grid_map$hos_dpr.scaled

plotRegresssion("F1_detrend", "AEDs using CAR model (n=1) DETREND", max_relevant_val = 6,
                map = LDN_grid_map)

car.out.detrend <- spautolm(formula = LDN_grid_map$F1_detrend ~
                             LDN_grid_map$f_pr.scaled + 
                             LDN_grid_map$ovr_50_.scaled + 
                             LDN_grid_map$bd_gh_p.scaled +
                             LDN_grid_map$cnt_spr.scaled +
                             LDN_grid_map$workday_popden.scaled+ 
                             LDN_grid_map$popden.scaled + 
                             LDN_grid_map$hos_dpr.scaled, 
                           data = LDN_grid_map, listw=A, family="CAR")

summary(car.out.detrend)

# Iteration 2
LDN_grid_map$F1_detrend2 = fitted(car.out.detrend) - 
  coef(car.out.detrend)[8] * LDN_grid_map$hos_dpr.scaled

plotRegresssion("F1_detrend2", "AEDs using CAR model (n=1) DETREND 2", max_relevant_val = 6,
                map = LDN_grid_map)

car.out.detrend2 <- spautolm(formula = LDN_grid_map$F1_detrend2 ~
                              LDN_grid_map$f_pr.scaled + 
                              LDN_grid_map$ovr_50_.scaled + 
                              LDN_grid_map$bd_gh_p.scaled +
                              LDN_grid_map$cnt_spr.scaled +
                              LDN_grid_map$workday_popden.scaled+ 
                              LDN_grid_map$popden.scaled + 
                              LDN_grid_map$hos_dpr.scaled, 
                            data = LDN_grid_map, listw=A, family="CAR")

summary(car.out.detrend2)


# Removing pop / wd pop ---------------------------------------------------

car.rmWD.scaled <- spautolm(formula = LDN_grid_map$cnt_AED ~
                             LDN_grid_map$f_pr.scaled + 
                             LDN_grid_map$ovr_50_.scaled + 
                             LDN_grid_map$bd_gh_p.scaled +
                             LDN_grid_map$cnt_spr.scaled +
                             LDN_grid_map$popden.scaled + 
                             LDN_grid_map$hos_dpr.scaled, 
                           data = LDN_grid_map, listw=A, family="CAR")
coef(car.rmWD.scaled)
summary(car.rmWD.scaled)

car.rmPD.scaled <- spautolm(formula = LDN_grid_map$cnt_AED ~
                             LDN_grid_map$f_pr.scaled + 
                             LDN_grid_map$ovr_50_.scaled + 
                             LDN_grid_map$bd_gh_p.scaled +
                             LDN_grid_map$cnt_spr.scaled +
                             LDN_grid_map$workday_popden.scaled+ 
                             LDN_grid_map$hos_dpr.scaled, 
                           data = LDN_grid_map, listw=A, family="CAR")
coef(car.rmPD.scaled)
summary(car.rmPD.scaled)


