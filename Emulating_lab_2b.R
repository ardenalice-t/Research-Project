library(sf)
library(spatialreg)
library(ggplot2)

# Reading the map file
LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
# created in population_plots.R


### REGRESSION ###
plot(LSOA_map$geometry)

lsoa_nb <- poly2nb(LSOA_map,queen=FALSE)
plot.nb(lsoa_nb, LSOA_map$geometry, add = TRUE, col='red')  

lsoa_nb_lag <- nblag(lsoa_nb, maxlag = 2)

plot(LSOA_map["ovr_50_"])

colnames(LSOA_map)[13] <- "workday_population_density"
colnames(LSOA_map)[16] <- "population_density"

A <- nb2listw(poly2nb(LSOA_map), style="B")
car.out <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + 
                      LSOA_map$`2024_p_`+ LSOA_map$fml_prp + 
                      LSOA_map$avg_dpr +  LSOA_map$workday_population_density + 
                      LSOA_map$bd_gh_p, data = LSOA_map, listw=A, family="CAR")
coef(car.out)
LSOA_map$F1 <- fitted(car.out)
plot(LSOA_map["F1"], main="Fitted data from CAR model", lwd=0.001)

LSOA_map$F1R<-fitted(car.out) - LSOA_map$cnt_AED
plot(LSOA_map["F1R"], main="Residuals")

ggplot() +
  geom_sf(data = LSOA_map, lwd=0.001, 
          aes(fill = F1)) +
  scale_fill_steps(n.breaks = 8, limits = c(0,10))

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

A2 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")
car.out2 <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + LSOA_map$fml_prp + 
                       LSOA_map$`2024_p_`+ LSOA_map$avg_dpr +  
                       LSOA_map$workday_population_density + 
                      LSOA_map$bd_gh_p, data = LSOA_map, listw=A2, family="CAR")
LSOA_map$F2 <- fitted(car.out2)
plot(LSOA_map["F2"], main="Fitted data from CAR model", lwd=0.001)

coef(car.out2)

plot(LSOA_map["F1"], main="Fitted data from CAR model", lwd=0.001)

LSOA_map$F2R<-fitted(car.out) - LSOA_map$cnt_AED
plot(LSOA_map["F2R"], main="Change")


### Removing City of London ###
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


### TESTING 
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
plot(LSOA_map["F2"], main="Fitted data from CAR mowdel", lwd=0.001)

