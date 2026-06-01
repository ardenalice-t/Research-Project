### Practicing Detrending ###

library(sf)
library(spatialreg)
library(ggplot2)
library(spdep)

### Setting up  ###

# Reading the map file
LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
# created in population_plots.R

# Renaming
colnames(LSOA_map)[13] <- "workday_population_density"
colnames(LSOA_map)[15] <- "population"
colnames(LSOA_map)[16] <- "population_density"

# Neighbors matrix 
lsoa_nb <- poly2nb(LSOA_map,queen=TRUE)
A <- nb2listw(poly2nb(LSOA_map), style="B")


### First Test: Population and Deprivation ###

# De-trending AED count
car.AED_dpr <- spautolm(formula = LSOA_map$cnt_AED ~
                      LSOA_map$hos_dpr, 
                    data = LSOA_map, listw=A, family="CAR")

coef(car.AED_dpr)

LSOA_map$AED_dpr <- fitted(car.AED_dpr)
plot(LSOA_map["AED_dpr"], main="Fitted data from CAR: AEDs using Deprivation", lwd=0.001)

LSOA_map$AED_dpr.R <- LSOA_map$cnt_AED - fitted(car.AED_dpr)
plot(LSOA_map["cnt_AED"],  main="AEDs")
plot(LSOA_map["AED_dpr.R"], main="Residuals: AEDs vs Deprivation")

LSOA_map$AED_dpr.R2 <- LSOA_map$cnt_AED - (coef(car.AED_dpr)[2]* LSOA_map$hos_dpr)
plot(LSOA_map["cnt_AED"],  main="AEDs")
plot(LSOA_map["AED_dpr.R2"], main="Residuals: AEDs vs Deprivation")


# De-trending population
car.pop_dpr <- spautolm(formula = LSOA_map$population ~
                          LSOA_map$hos_dpr, 
                        data = LSOA_map, listw=A, family="CAR")

coef(car.pop_dpr)

LSOA_map$pop_dpr <- fitted(car.pop_dpr)
plot(LSOA_map["pop_dpr"], main="Fitted data from CAR: Population using Deprivation", lwd=0.001)

LSOA_map$pop_dpr.R <- LSOA_map$population - fitted(car.pop_dpr)
plot(LSOA_map["population"],  main="Population")
plot(LSOA_map["pop_dpr.R"], main="Residuals: Population vs Deprivation")

LSOA_map$pop_dpr.R2 <- LSOA_map$population - (coef(car.AED_dpr)[2]* LSOA_map$hos_dpr)
plot(LSOA_map["population"],  main="Population")
plot(LSOA_map["pop_dpr.R2"], main="Residuals2: AEDs vs Deprivation")


# AEDs using population
car.AED_pop <- spautolm(formula = LSOA_map$cnt_AED ~
                          LSOA_map$population, 
                        data = LSOA_map, listw=A, family="CAR")

coef(car.AED_pop)

LSOA_map$AED_pop <- fitted(car.AED_pop)
plot(LSOA_map["AED_pop"], 
     main="Fitted data from CAR: AEDs using Population", lwd=0.001)

LSOA_map$AED_pop.R <- LSOA_map$cnt_AED - fitted(car.AED_pop)
plot(LSOA_map["AED_pop.R"], main="Residuals: AEDs vs Deprivation")


# Residual: AEDs using population
car.AED_pop.detrend <- spautolm(formula = LSOA_map$AED_dpr.R ~
                          LSOA_map$pop_dpr.R, 
                        data = LSOA_map, listw=A, family="CAR")

coef(car.AED_pop.detrend)

LSOA_map$AED_pop.detrend <- fitted(car.AED_pop.detrend)
plot(LSOA_map["AED_pop.detrend"], 
     main="Fitted data from CAR: AEDs using Population (Residuals)", 
     lwd=0.001)


# Testing: AEDs using population
car.AED_pop.detrend.test <- spautolm(formula = LSOA_map$AED_dpr.R ~
                                  LSOA_map$pop_dpr.R + LSOA_map$hos_dpr, 
                                data = LSOA_map, listw=A, family="CAR")

coef(car.AED_pop.detrend.test)
summary(car.AED_pop.detrend.test)

car.AED.detrend.test2 <- spautolm(formula = LSOA_map$AED_dpr.R2 ~
                                    LSOA_map$pop_dpr.R + LSOA_map$hos_dpr, 
                                  data = LSOA_map, listw=A, family="CAR")

coef(car.AED.detrend.test2)
summary(car.AED.detrend.test2)


LSOA_map$AED_pop.detrend.test <- fitted(car.AED_pop.detrend.test)
plot(LSOA_map["AED_pop.detrend.test"], 
     main="Fitted data from CAR: AEDs using Population (Residuals)", 
     lwd=0.001)

LSOA_map$AED_pop.detrend.test2 <- fitted(car.AED.detrend.test2)
plot(LSOA_map["AED_pop.detrend.test2"], 
     main="Fitted data from CAR: AEDs using Population (Residuals)", 
     lwd=0.001)


car.AED.detrend.test3 <- spautolm(formula = LSOA_map$AED_dpr.R2 ~
                                    LSOA_map$pop_dpr.R2 + LSOA_map$hos_dpr, 
                                  data = LSOA_map, listw=A, family="CAR")

coef(car.AED.detrend.test3)
summary(car.AED.detrend.test3)

LSOA_map$AED_pop.detrend.test3 <- fitted(car.AED.detrend.test3)
plot(LSOA_map["AED_pop.detrend.test3"], 
     main="Fitted data from CAR: AEDs using Population (Residuals)", 
     lwd=0.001)



