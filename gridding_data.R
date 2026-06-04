# Making grid

?st_make_grid

ldn_boundary_map <- read_sf("maps/gla")

test_grid <- st_make_grid(x=ldn_boundary_map, cellsize=1000)
plot(ldn_boundary_map$geometry)
plot(test_grid, add=TRUE)

london_idx <- st_intersects(ldn_boundary_map, test_grid)[[1]]
plot(test_grid[london_idx])

install.packages("akima")
library(akima)
?interp

test.interp = interp(LSOA_map)

??idw


library(raster)
r <- raster()
extent(r) <- extent(LSOA_map)
# setting resolution to 1km
res(r) <- 1000
plot(r)

london.raster <- st_rasterize(LSOA_map["geometry"], raster_test, LSOA_map$cnt_AED, fun="mean")
plot(london.raster)
?rasterize

library(fasterize)
??fasterize

london.raster <- fasterize(LSOA_map, r, "cnt_AED", fun="sum")
plot(london.raster)
?fasterize

library(raster)
r <- raster()
extent(r) <- extent(LSOA_map)
# setting resolution to 1km
res(r) <- 1000
crs(r) = crs(LSOA_map)

london.raster <- rasterize(LSOA_map, r, 
                                 field      = "cnt_AED",
                                 background = -9999,
                                 update     = TRUE)
plot(london.raster)
levels(london.raster)       

crs(LSOA_map)
crs(r) = crs(LSOA_map)



rast <- raster()
LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
colnames(LSOA_map)[13] <- "workday_population_density"
colnames(LSOA_map)[15] <- "population"
colnames(LSOA_map)[16] <- "population_density"
LSOA_map$workday_population <- LSOA_map$workday_population_density * LSOA_map$AreSqKm
extent(rast) <- extent(LSOA_map) 
#crs(rast) = crs(LSOA_map)
#res(rast) <- 0.005
ncol(rast) <- 150 # 50 roughly 1km, 
nrow(rast) <- 150

raster_and_plot <- function(column,title, fun=mean, map=LSOA_map, boxmeters=200){
  r <- raster()
  extent(r) <- extent(map) 
  ncol(r) <- 50 * 1000 / boxmeters
  nrow(r) <- ncol(r)
  
  rast_data <- rasterize(map, r, column, fun)
  plot(rast_data, main=title)
  return (rast_data)
}

rast_popden <- rasterize(LSOA_map, rast, LSOA_map$population_density, fun=mean) 
plot(rast_popden)

rast_wdpopden <- rasterize(LSOA_map, rast, LSOA_map$workday_population_density, fun=mean) 
plot(rast_wdpopden)

raster_and_plot(LSOA_map$cnt_AED, title = "Average AED count")
raster_and_plot(LSOA_map$population_density, title = "Population Density")
raster_and_plot(LSOA_map$workday_population_density, title = "WD Population Density")
raster_and_plot(LSOA_map$f_pr, title = "Average Female Proportion")
raster_and_plot(LSOA_map$ovr_50_, title = "Average Over 50 Proportion")
raster_and_plot(LSOA_map$bd_gh_p, title = "Average Bad General Health Proportion")
raster_and_plot(LSOA_map$hos_dpr, title = "Average Household Deprivation")

rasterToGrid(x, target, fun = "mean", crop = TRUE, na.rm = TRUE)

test.out = raster_and_plot(LSOA_map$population_density, title = "Population Density")
test.out = as.polygons(test.out)

test = st_as_sf(test.out)

st_as_sf(test.out[[1]], as_points = FALSE, merge = FALSE)
