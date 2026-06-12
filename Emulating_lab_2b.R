
# Packages ----------------------------------------------------------------

library(sf)
library(spatialreg)
library(ggplot2)
library(spdep)


# Reading Files -----------------------------------------------------------

# Reading the map file
LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
# created in population_plots.R

colnames(LSOA_map)[13] <- "workday_population_density"
colnames(LSOA_map)[15] <- "population"
colnames(LSOA_map)[16] <- "population_density"
LSOA_map$workday_population <- LSOA_map$workday_population_density * LSOA_map$AreSqKm


# Functions ---------------------------------------------------------------

source("functions/regression.allParams.R")
source("functions/plotRegression.R")

# Regression - 1 depth ----------------------------------------------------

### REGRESSION ###
plot(LSOA_map$geometry)

lsoa_nb <- poly2nb(LSOA_map,queen=FALSE)
plot.nb(lsoa_nb, LSOA_map$geometry, add = TRUE, col='red')  

plot(LSOA_map["f_pr"])

A <- nb2listw(poly2nb(LSOA_map), style="B")
car.out <- regression.allParams(A)

coef(car.out)
summary(car.out)
table(coef(car.out))
mean(LSOA_map$AreSqKm)
var(LSOA_map$AreSqKm)
LSOA_map$F1 <- fitted(car.out)
plot(LSOA_map["F1"], main="Fitted data from CAR model", lwd=0.001)

LSOA_map$F1R<-fitted(car.out) - LSOA_map$cnt_AED
plot(LSOA_map["F1R"], main="Residuals")


# Plots
plotRegresssion("F1", "AEDs using CAR model (n=1)", max_relevant_val = 10)

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = cnt_AED)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,10))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = F1R)) +
  scale_fill_steps2(n.breaks = 8, limits = c(-100,100),low = "red",
                   mid = "white",
                   high = "blue")



# Regression - 2 depth ----------------------------------------------------

# Using 2 neighbors deep 

n.neighbors = 2

lsoa_nb_lag <- nblag(lsoa_nb, maxlag = n.neighbors)
A2 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")

car.out2 <- regression.allParams(A2)
LSOA_map$F2 <- fitted(car.out2)

coef(car.out2)

plotRegresssion("F2", "AEDs using CAR model (n=2)", max_relevant_val = 30)

LSOA_map$F2R<-fitted(car.out) - LSOA_map$cnt_AED
plot(LSOA_map["F2R"], main="Change")

# Regression - 3 depth ----------------------------------------------------

n.neighbors = 3

lsoa_nb_lag <- nblag(lsoa_nb, maxlag = n.neighbors)
A3 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")

car.out3 <- regression.allParams(A3)
LSOA_map$F3 <- fitted(car.out3)
coef(car.out3)

plotRegresssion("F3", "AEDs using CAR model (n=3)", max_relevant_val = 10)


# Regression - 4 depth ----------------------------------------------------

n.neighbors = 4

lsoa_nb_lag <- nblag(lsoa_nb, maxlag = n.neighbors)
A4 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")

car.out4 <- regression.allParams(A4)
LSOA_map$F4 <- fitted(car.out4)
coef(car.out4)

plotRegresssion("F4", "AEDs using CAR model (n=4)", max_relevant_val = 10)


# Removing City of London -------------------------------------------------

LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
colnames(LSOA_map)[13] <- "workday_population_density"

LSOA_map <- LSOA_map[!grepl('City of London', LSOA_map$LSOA21NM),]
LSOA_map <- LSOA_map[!grepl('Westminster', LSOA_map$LSOA21NM),]

plot(LSOA_map$geometry)

lsoa_nb <- poly2nb(LSOA_map,queen=TRUE)
A <- nb2listw(poly2nb(LSOA_map), style="B")
lsoa_nb_lag <- nblag(lsoa_nb, maxlag = 3)
A2 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")
plot.nb(lsoa_nb, LSOA_map$geometry, add = TRUE, col='red')  


car.out <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ +
                       LSOA_map$workday_population_density + 
                      LSOA_map$bd_gh_p, data = LSOA_map, listw=A, family="CAR")
LSOA_map$F3 <- fitted(car.out)
plot(LSOA_map["F3"], main="Fitted data from CAR model", lwd=0.001)

LSOA_map$F3R<-fitted(car.out) - LSOA_map$cnt_AED
coef(car.out)
plot(LSOA_map["F3R"], main="Residuals")

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = F3)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,3))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = cnt_AED)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,3))



# Testing Detrending ------------------------------------------------------

# tryung to set a coef to 0
coef(car.out2)
coef(car.out2)[2]
coef(car.out2)[2] = 0
class(car.out2)

car.out2 <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + LSOA_map$fml_prp + 
                       LSOA_map$`2024_p_`+ offset(LSOA_map$avg_dpr) +  
                       LSOA_map$workday_population_density + 
                       LSOA_map$bd_gh_p, data = LSOA_map, listw=A2, family="CAR")
coef(car.out2)
LSOA_map$F2 <- fitted(car.out2)
plot(LSOA_map["F2"], main="Fitted data from CAR model 2", lwd=0.001)

car.out.test = car.out2
coef(car.out.test)
car.out.test$fit$coefficients[2] = 0
car.out.test$fit$coefficients[3] = 0
car.out.test$fit$coefficients[6] = 0

LSOA_map$F_test <- fitted(car.out.test)
plot(LSOA_map["F_test"], main="Fitted data from CAR model Tes", lwd=0.001)

LSOA_map$F1_removed <- fitted(car.out) - LSOA_map$hos_dpr
plot(LSOA_map["F1_removed"], main="Fitted data from CAR model", lwd=0.001)

# seting to 0 is just not including it ? 
car.out3 <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$fml_prp + 
                       LSOA_map$`2024_p_`+ offset(LSOA_map$avg_dpr) +
                       LSOA_map$workday_population_density + 
                       LSOA_map$bd_gh_p, data = LSOA_map, listw=A2, family="CAR")
coef(car.out3)
car.out3$fit$coefficients[2]=0
LSOA_map$F3 <- fitted(car.out3)
plot(LSOA_map["F3"], main="Fitted data from CAR model 3", lwd=0.001)

car.out.removal <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + 
                              LSOA_map$population_density+ LSOA_map$fml_prp + 
                              LSOA_map$avg_dpr +  LSOA_map$workday_population_density + 
                              LSOA_map$bd_gh_p, data = LSOA_map, listw=A, family="CAR")
car.out4$fit$coefficients[7]
LSOA_map$F2<- fitted(car.out4.removal)
LSOA_map$F4_removed <- fitted(car.out4) - (car.out4$fit$coefficients[7] * LSOA_map$hos_dpr)
plot(LSOA_map["F1_removed"], main="Fitted data from CAR model Removal", lwd=0.001)
plot(LSOA_map["F2"], main="Fitted data from CAR model", lwd=0.001)

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = F2_removed)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,5))

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = F2)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,5))


# Neighbours based on Distance --------------------------------------------

### Maps based on distance neighbors 

# k nearest neighbours from centers - makes into neighbours list class nb
knearest.nb <- knn2nb(knearneigh(st_centroid(LSOA_map, longlat=TRUE), k=2), 
                      sym=TRUE)

# neighbors found from distance from centers - anything within 1km 
distance.nb <- dnearneigh(st_centroid(LSOA_map), d1=0, d2=1)
distance.A <- nb2listw(distance.nb,style="B", zero.policy = TRUE)

distance.out = regression.allParams(distance.A)

summary(distance.out)
coef(distance.out) 
LSOA_map$F.distance = fitted(distance.out)

plotRegresssion("F.distance", "Example title")
plotRegresssion("F.distance", "Example title", max_relevant_val = 30)
plotRegresssion("F.distance", "Example title", max_relevant_val = 30)


# optional plot - lab2b
plot(st_geometry(LSOA_map),border="darkgray", lwd=0.1)
plot.nb(distance.nb,st_geometry(LSOA_map),
        add=TRUE,col="purple", lwd = 0.1, points=FALSE)


# Cross Terms Testing  ----------------------------------------------------

lsoa_nb <- poly2nb(LSOA_map,queen=FALSE)
A <- nb2listw(poly2nb(LSOA_map), style="B")

cross.out <- spautolm(formula = LSOA_map$cnt_AED ~
                      LSOA_map$f_pr * LSOA_map$ovr_50_ + 
                      LSOA_map$bd_gh_p +
                      LSOA_map$hos_dpr + 
                      LSOA_map$workday_population +
                      LSOA_map$population + 
                      LSOA_map$population:LSOA_map$bd_gh_p +
                      LSOA_map$population:LSOA_map$f_pr +
                      LSOA_map$population:LSOA_map$ovr_50_, 
                    data = LSOA_map, listw=A , family="CAR")

## tried to do a cross term of workday pop and pop but too correlated 
cov(LSOA_map$workday_population, LSOA_map$workday_population)

summary(cross.out)
coef(cross.out)

LSOA_map$Fcross <- fitted(cross.out)

plotRegresssion("Fcross", "AEDs using CAR model (n=1), Cross term", max_relevant_val = 10)
plotRegresssion("F1", "AEDs using CAR model (n=1)", max_relevant_val = 10)


cross.squared <- spautolm(formula = LSOA_map$cnt_AED ~
                        LSOA_map$f_pr + LSOA_map$ovr_50_ + 
                        I(LSOA_map$ovr_50_^2) + 
                        LSOA_map$bd_gh_p +
                        LSOA_map$workday_population+LSOA_map$population, 
                      data = LSOA_map, listw=A , family="CAR")

summary(cross.squared)
summary(cross.out)
coef(cross.squared)

LSOA_map$Fcross_squared <- fitted(cross.squared)

plotRegresssion("Fcross_squared", "AEDs using CAR model (n=1), Cross term", max_relevant_val = 10)
plotRegresssion("F1", "AEDs using CAR model (n=1)", max_relevant_val = 10)

var(LSOA_map$cnt_AED)
