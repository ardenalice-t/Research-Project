# Making grid


# Reading Maps ------------------------------------------------------------

ldn_boundary_map <- read_sf("maps/gla")

LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
colnames(LSOA_map)[13] <- "workday_population_density"
colnames(LSOA_map)[15] <- "population"
colnames(LSOA_map)[16] <- "population_density"
LSOA_map$workday_population <- LSOA_map$workday_population_density * LSOA_map$AreSqKm


# Making an ST map --------------------------------------------------------

# with ldn boundary map

cell_meters = 500

make_ldn_grid = function(cell_meters){
  st_grid <- st_make_grid(x=ldn_boundary_map, cellsize=cell_meters)
  london_idx <- st_intersects(ldn_boundary_map, st_grid)[[1]]
  return(st_grid[london_idx])
}

ldn_grid = make_ldn_grid(cell_meters = 300)

LSOA_map <- st_transform(LSOA_map, crs=st_crs(ldn_grid))

# with LSOA
plot(LSOA_map$geometry)
plot(ldn_grid, add=TRUE)

# This is a grid of london but not sure how i would interpolate the values onto this

numeric_columns = c("f_pr", "ovr_50_", "bd_gh_p", "workday_population_density", "hos_dpr", "population_density")
ldn_grid_values = st_interpolate_aw(
  LSOA_map[c("geometry", numeric_columns)],
  to = ldn_grid,
  extensive=FALSE # mean is maintained 
)
plot(ldn_grid_values["hos_dpr"])

# Raster Object Grids -----------------------------------------------------

library(raster)

rast <- raster()

source("functions/raster_and_plot.R")


raster_and_plot(LSOA_map$cnt_AED, title = "Average AED count")
raster_and_plot(LSOA_map$cnt_AED, title = "Average AED count", boxmeters = NA)

raster_and_plot(LSOA_map$population_density, title = "Population Density")
raster_and_plot(LSOA_map$population_density, title = "Population Density2", boxmeters = NA)

raster_and_plot(LSOA_map$workday_population_density, title = "WD Population Density")
raster_and_plot(LSOA_map$f_pr, title = "Average Female Proportion")
raster_and_plot(LSOA_map$ovr_50_, title = "Average Over 50 Proportion")
raster_and_plot(LSOA_map$bd_gh_p, title = "Average Bad General Health Proportion")
raster_and_plot(LSOA_map$hos_dpr, title = "Average Household Deprivation")


# Transforming Raster back to SF ------------------------------------------

rasterToGrid(x, target, fun = "mean", crop = TRUE, na.rm = TRUE)

test.out = raster_and_plot(LSOA_map$population_density, title = "Population Density")
test.out = as.polygons(test.out)

test = st_as_sf(test.out)
x_rast = rast(test.out)
x_raster = raster(test.out)
plot(x_rast)
test= st_as_sf(x_rast)
test = sf_as_st(x_rast)
plot(x_raster)
st_as_sf(test.out[1], as_points = FALSE, merge = FALSE)

install.packages("stars")
library(stars)

save(test.out, )

?save

test = read_stars(test.out)

car.out <- spautolm(formula = LSOA_map$cnt_AED~LSOA_map$ovr_50_ + LSOA_map$fml_prp + 
                       LSOA_map$`2024_p_`+ offset(LSOA_map$avg_dpr) +  
                       LSOA_map$workday_population_density + 
                       LSOA_map$bd_gh_p, data = LSOA_map, listw=A2, family="CAR")





# Adding Point Data -------------------------------------------------------

### AED Data -------------------------------------------------------

aed_map <- read_sf("data/LDN_AEDs")
# adding an ID column
ldn_grid_values$ID <- seq.int(nrow(ldn_grid_values))

# transforming to have the same crs 
ldn_grid_values <- st_transform(ldn_grid_values, crs=st_crs(aed_map))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the AEDs to the grid square they are within
aed_to_grid <- st_join(aed_map, ldn_grid_values, join = st_within)

# counting AEDs in each LSOA
count_aeds <- count(as_tibble(aed_to_grid), ID, name="count_AEDs")

# Plotting resulting map
ldn_grid_values <- left_join(ldn_grid_values, count_aeds, 
                      by = c("ID" = "ID"))
# changing any NA to 0
ldn_grid_values <- mutate(ldn_grid_values, "count_AEDs" = ifelse(is.na(count_AEDs), 0, count_AEDs))

max_relevant_val = 10
ggplot() +
  geom_sf(data = ldn_grid_values, lwd=0.001, 
          aes(fill = count_AEDs)) +
  scale_fill_steps(breaks = seq(0, max_relevant_val, length = 6),
                   na.value = "light blue",
                   rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)), 
                   name = "Number of AEDs") + 
  ggtitle(label = "Number of AEDs")


### Sportsground Data -------------------------------------------------------

library(readr)
sportsgrounds_coords <- read_csv("data/sports_coordinates.csv", 
                                 col_types = cols_only(objectid = col_guess(), 
                                                       lat = col_guess(), long = col_guess()))

sportsgrounds_coords = st_as_sf(sportsgrounds_coords, coords = c("long", "lat"), 
                       crs=st_crs(ldn_grid_values))
sportsground_sf = st_transform(sportsgrounds_coords, crs=st_crs(ldn_boundary_map))


# Finding the sports coordinates that intersect London
london_idx <- st_contains(ldn_boundary_map, sportsground_sf)[[1]]
sportsground_sf <-sportsground_sf[london_idx,]

# plot to test output
ggplot() + 
  geom_sf(data = ldn_grid_values) +
  geom_sf(data=sportsground_sf,
          size = 0.005,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(label = "sports locations") + 
  coord_sf(crs = st_crs(ldn_grid_values))

# transforming to have the same crs 
sportsground_sf <- st_transform(sportsground_sf, crs=st_crs(ldn_grid_values))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the sports to the grid square they are within
sportsground_to_grid <- st_join(sportsground_sf, ldn_grid_values, join = st_within)

# counting sports in each grid cell
count_sportsground <- count(as_tibble(sportsground_to_grid), ID, name="count_sports")

# Plotting resulting map
ldn_grid_values <- left_join(ldn_grid_values, count_sportsground, 
                      by = c("ID" = "ID"))
# changing any NA to 0
ldn_grid_values <- mutate(ldn_grid_values, "count_sports" = ifelse(is.na(count_sports), 0, count_sports))

plot(ldn_grid_values["count_sports"])

# Saving ------------------------------------------------------------------

write_sf(ldn_grid_values, "data/grid_map.shp", )

