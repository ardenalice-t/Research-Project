library(sf)

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

A <- nb2listw(poly2nb(LSOA_map), style="B")
car.out <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + LSOA_map$fml_prp + 
                      LSOA_map$avg_dpr +  LSOA_map$workday_population_density + 
                      LSOA_map$bd_gh_p, data = LSOA_map, listw=A, family="CAR")
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
  scale_fill_steps2(n.breaks = 8, limits = c(-10,10),low = "red",
                   mid = "white",
                   high = "blue")

A2 = nb2listw(nblag_cumul(lsoa_nb_lag),style="B")
car.out <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + LSOA_map$fml_prp + 
                      LSOA_map$avg_dpr +  LSOA_map$workday_population_density + 
                      LSOA_map$bd_gh_p, data = LSOA_map, listw=A2, family="CAR")
LSOA_map$F2 <- fitted(car.out)
plot(LSOA_map["F2"], main="Fitted data from CAR model", lwd=0.001)

plot(LSOA_map["F1"], main="Fitted data from CAR model", lwd=0.001)

LSOA_map$F2R<-fitted(car.out) - LSOA_map$cnt_AED
plot(LSOA_map["F2R"], main="Change")
