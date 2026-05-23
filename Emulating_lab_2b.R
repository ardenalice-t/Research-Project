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
