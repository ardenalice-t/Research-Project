
# Descriptive Investigation -----------------------------------------------



# Packages ----------------------------------------------------------------

library(readxl)
library(readr)

library(sf) # for read_sf
#library(sp)
library(spData) # has the ldn dataset

library(ggplot2) # for plotting
#library(ggmap)

library(dplyr) # for select
library(tidyr) # for pivoting longer in dataframe coef collecting
library(stringr) #to split strings
#library(spdep)

library(openairmaps) # for converting postcodes out

# Functions ---------------------------------------------------------------

plot_descriptive_ldn <- function(relevant_col, 
                             title, legend_title, 
                             map = LSOA_map, scale = scales::label_number()){
  # plotting
  ggplot() +
    geom_sf(data = map, lwd=0, 
            aes(fill = .data[[relevant_col]])) + 
    scale_fill_continuous(name = legend_title, labels = scale, 
                          palette = "viridis") +
    ggtitle(label = title) +
    xlab("Longitude") +
    ylab("Latitude") 
}

# Creating London Map -----------------------------------------------------

# Creating london map of LSOAs
LSOA_map <- read_sf("maps/LDN_LSOA")

LSOA_map <- select(LSOA_map, -c(LSOA21NMW, BNG_E, BNG_N))


## Adding individual data --------------------------------------------------

LSOA_idv_data <- read_csv("data/external_datasets/Sex-GH-Age_LSOA.csv")

# Creating the measurements we want 
LSOA_idv_observations <- LSOA_idv_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(pc_f = 
              sum(Observation[`Sex (2 categories) Code`==1]) /
              sum(Observation),
            pc_50_plus = 
              sum(Observation[(`Age (6 categories) Code`==5) | 
                                (`Age (6 categories) Code`==6)]) /
              sum(Observation),
            pc_65_plus = 
              sum(Observation[ (`Age (6 categories) Code`==6)]) /
              sum(Observation),
            pc_bad_gh = 
              sum(Observation[`General health (4 categories) Code`==3]) /
              sum(Observation))
# have checked these measurements with the data

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_idv_observations, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'pc_50_plus', title = "Population Age by LSOA", 
                     legend_title = "Percent of Population \n50 or above", scale = scales::label_percent())


## Adding Household Data -------------------------------------------

LSOA_hos_data <- read_csv("data/external_datasets/household_deprivation_LSOA.csv")

# any 'does not apply' changed to NA 
LSOA_hos_data <- mutate(LSOA_hos_data, 
                       `Household deprivation (6 categories) Code` = 
                         ifelse(`Household deprivation (6 categories) Code` == -8, NA, `Household deprivation (6 categories) Code`))

LSOA_hos_obs <- LSOA_hos_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(avg_hos_depr = 
              sum(Observation * `Household deprivation (6 categories) Code`, na.rm = TRUE) /
              sum(Observation, na.rm = TRUE))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_hos_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'avg_hos_depr', title = "Household Deprivation by LSOA", 
                     legend_title = "Average Number of \nDeprivation Dimensions")


## Adding Population Densities ---------------------------------------------

LSOA_popden_data <- read_csv("data/external_datasets/2021 pop density census.csv")

LSOA_popden_obs <- LSOA_popden_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(pop_den = sum(Observation))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_popden_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'pop_den', title = "Population Density by LSOA", 
                     legend_title = "Residents per Sq Km")

# with exponential scale
ggplot() +
  geom_sf(data = LSOA_map, lwd=0, 
          aes(fill = pop_den)) + 
  scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans()) +
  ggtitle(label = "Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")


LSOA_WD_popden_data <- read_csv("data/external_datasets/WD_pop_den.csv")

LSOA_WD_popden_obs <- LSOA_WD_popden_data %>% 
  group_by(`Lower layer Super Output Areas Code`)%>% 
  summarise(WD_pop_den = sum(`Population Density`))

# Joining with the LSOA map
LSOA_map <- left_join(LSOA_map, LSOA_WD_popden_obs, 
                      by = c("LSOA21CD" = "Lower layer Super Output Areas Code"))

# Plotting 
plot_descriptive_ldn(relevant_col = 'WD_pop_den', title = "Workday Population Density by LSOA", 
                     legend_title = "Residents per Sq Km")

# with exponential scale
ggplot() +
  geom_sf(data = LSOA_map, lwd=0, 
          aes(fill = WD_pop_den)) + 
  scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans(), limits = c(NA,100000)) +
  ggtitle(label = "Workday Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")

# Plotting both side by side
library(gridExtra)
plot1 = ggplot() +
    geom_sf(data = LSOA_map, lwd=0, 
            aes(fill = pop_den)) + 
    scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                          palette = "viridis", transform = scales::log10_trans()) +
    ggtitle(label = "Population Density by LSOA") +
  theme(legend.position="none")+
    xlab("Longitude") +
    ylab("Latitude")
  
plot2 = ggplot() +
    geom_sf(data = LSOA_map, lwd=0, 
            aes(fill = WD_pop_den)) + 
    scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                          palette = "viridis", transform = scales::log10_trans(), limits = c(NA,100000)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), aspect.ratio = 0.8) + 
    ggtitle(label = "Workday Population Density by LSOA") +
    xlab("Longitude") +
    ylab("Latitude")


grid.arrange(plot1, plot2, ncol=2, widths=c(0.355, 0.4) )


# Saving LSOA Map ---------------------------------------------------------

write_sf(LSOA_map, "data/LSOA_complete_map_July.shp")


# Gridding Data -----------------------------------------------------------

ldn_boundary_map <- read_sf("maps/gla")

LSOA_map <- read_sf("data/LSOA_complete_map_July.shp")

names(LSOA_map)[10] = "avg_dpr"


# Function to make a grid of london
make_ldn_grid = function(cell_meters){
  st_grid <- st_make_grid(x=ldn_boundary_map, cellsize=cell_meters)
  london_idx <- st_intersects(ldn_boundary_map, st_grid)[[1]]
  return(st_grid[london_idx])
}

ldn_300_grid = make_ldn_grid(300)
ldn_400_grid = make_ldn_grid(400)
ldn_500_grid = make_ldn_grid(500)

LSOA_map <- st_transform(LSOA_map, crs=st_crs(ldn_300_grid))

# with LSOA
plot(LSOA_map$geometry)
plot(ldn_300_grid, add=TRUE)

numeric_columns = c("pc_f", "pc_50_p", "pc_65_p", "pc_bd_g", "avg_dpr", "pop_den", "WD_pp_d")
ldn_grid_values = st_interpolate_aw(
  LSOA_map[c("geometry", numeric_columns)],
  to = ldn_300_grid,
  extensive=FALSE # mean is maintained 
)
plot(ldn_grid_values["pop_den"])


# Adding point data -------------------------------------------------------


## Sportsgrounds -----------------------------------------------------------

sportsgrounds_coords <-  read_csv("data/external_datasets/GIS_Active_Places_Power_Sites_7588440123797672972.csv", 
                                  col_types = cols_only(objectid = col_guess(), 
                                                        lat = col_guess(), long = col_guess()))

sports_sf = st_as_sf(sportsgrounds_coords, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
sports_sf = st_transform(sports_sf, crs=st_crs(ldn_boundary_map))

# Finding the sports coordinates that intersect London
london_idx <- st_contains(ldn_boundary_map, sports_sf)[[1]]
sports_sf_ldn <-sports_sf[london_idx,]

# plot to test output
ggplot() + 
  geom_sf(data = ldn_boundary_map) +
  geom_sf(data=sports_sf_ldn,
          size = 0.005,alpha = 0.5,
          colour="red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(label = "sports locations")

write_sf(sports_sf_ldn, "data/sports/ldn_sportss_map.shp")

# transforming to have the same crs 
sportsground_sf <- st_transform(sports_sf_ldn, crs=st_crs(ldn_grid_values))

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


## AEDs --------------------------------------------------------------------

ldn_boundary_map <- read_sf("maps/gla")
ldn_boundary_map = st_transform(ldn_boundary_map, crs=4283)

AED_data <- read_excel("data/external_datasets/defibrillator_data July 2026.xlsx", 
                       sheet = "data_extract_2026-07-01")
# changing lat to be a numeric
AED_data = transform(AED_data, lat = as.numeric(lat))
AED_data_sf = st_as_sf(AED_data, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
AED_data_sf = st_transform(AED_data_sf, crs=st_crs(ldn_boundary_map))

# Finding the AED coordinates that intersect London
london_idx <- st_contains(ldn_boundary_map, AED_data_sf)[[1]]
ldn_AED_data_sf <-AED_data_sf[london_idx,]

write_sf(ldn_AED_data_sf, "data/LDN_AEDs_July/ldn_AEDs_map.shp")


aed_map <- read_sf("data/LDN_AEDs_July")
# adding an ID column
ldn_grid_values$ID <- seq.int(nrow(ldn_grid_values))

# transforming to have the same crs 
ldn_grid_values <- st_transform(ldn_grid_values, crs=st_crs(aed_map))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the AEDs to the grid square they are within
aed_to_grid <- st_join(aed_map, ldn_grid_values, join = st_within)

# counting AEDs in each square
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


ggplot() +
  geom_sf(data = ldn_grid_values, lwd=0, 
          aes(fill = count_AEDs)) + 
  scale_fill_continuous(name = "Residents per \nSq Km", labels = scales::label_number(), 
                        palette = "viridis", transform = scales::log10_trans()) +
  ggtitle(label = "Population Density by LSOA") +
  xlab("Longitude") +
  ylab("Latitude")


## Care Homes --------------------------------------------------------------

care_facility_data <- read_excel("data/external_datasets/Care Facilities invididual sites.xlsx", 
                                 sheet = "Sheet1") %>% select(c("Name", "Postcode", "Region"))

install.packages("openairmaps")
library(openairmaps)

library(n)

care_lat = sapply(care_facility_data$Postcode, function(x){return(try(convertPostcode(x)$lat))})

care_long = sapply(care_facility_data$Postcode, function(x){return(try(convertPostcode(x)$lng))})

care_facility_data$lat = care_lat
care_facility_data$long = care_long

care_facility_data = transform(care_facility_data, lat = as.numeric(lat), long = as.numeric(long))

HA7_3JE = c(51.624468, -0.340734)
RM7_0XY = c(51.559053, 0.176637)
#https://findthatpostcode.uk/postcodes/RM7%200XY.html


care_facility_data[which(care_facility_data$Postcode == "HA7 3JE", arr.ind=TRUE),"lat"] =  HA7_3JE[1]
care_facility_data[which(care_facility_data$Postcode == "HA7 3JE", arr.ind=TRUE),"long"] =  HA7_3JE[2]
care_facility_data[which(care_facility_data$Postcode == "RM7 0XY", arr.ind=TRUE),"lat"] =  RM7_0XY[1]
care_facility_data[which(care_facility_data$Postcode == "RM7 0XY", arr.ind=TRUE),"long"] =  RM7_0XY[2]

sapply(care_facility_data, anyNA)

care_facility_sf = st_as_sf(care_facility_data, coords = c("long", "lat"), 
                       crs=st_crs(ldn_boundary_map))
care_facility_sf = st_transform(care_facility_sf, crs=st_crs(ldn_boundary_map))

plot(care_facility_sf)

# transforming to have the same crs 
ldn_grid_values <- st_transform(ldn_grid_values, crs=st_crs(care_facility_sf))

# required to use st_join st_within 
sf_use_s2(FALSE)

# joining the AEDs to the grid square they are within
CH_to_grid <- st_join(care_facility_sf, ldn_grid_values, join = st_within)

# counting AEDs in each square
count_CHs <- count(as_tibble(CH_to_grid), ID, name="count_CHs")

# Plotting resulting map
ldn_grid_values <- left_join(ldn_grid_values, count_CHs, 
                             by = c("ID" = "ID"))
# changing any NA to 0
ldn_grid_values <- mutate(ldn_grid_values, "count_CHs" = ifelse(is.na(count_CHs), 0, count_CHs))

plot(ldn_grid_values["count_CHs"])


# Saving Grid -------------------------------------------------------------

write_sf(ldn_grid_values, "data/grids/ldn_grid_300.shp")


# Regression --------------------------------------------------------------

LDN_grid_map = read_sf("data/grids/ldn_grid_300.shp")
LDN_grid_map = read_sf("data/grids/ldn_grid_values_500.shp")


## scaling -----------------------------------------------------------------


# Rescaling all relevant variables
LDN_grid_map$pc_f.scaled = scale(LDN_grid_map$pc_f)
LDN_grid_map$pc_50_p.scaled = scale(LDN_grid_map$pc_50_p)
LDN_grid_map$pc_65_p.scaled = scale(LDN_grid_map$pc_65_p)
LDN_grid_map$pc_bd_g.scaled = scale(LDN_grid_map$pc_bd_g)
LDN_grid_map$avg_dpr.scaled = scale(LDN_grid_map$avg_dpr)
LDN_grid_map$pop_den.scaled = scale(LDN_grid_map$pop_den)
LDN_grid_map$WD_pp_d.scaled = scale(LDN_grid_map$WD_pp_d)
LDN_grid_map$cnt_spr.scaled = scale(LDN_grid_map$cnt_spr)
LDN_grid_map$cnt_CHs.scaled = scale(LDN_grid_map$cnt_CHs)

plot(LDN_grid_map["avg_dpr.scaled"])

var(LDN_grid_map$pc_f) / mean (LDN_grid_map$pc_f)
var(LDN_grid_map$pc_50_p) / mean (LDN_grid_map$pc_50_p)
var(LDN_grid_map$pc_bd_g) / mean (LDN_grid_map$pc_bd_g)


moran.test(LDN_grid_map$pc_f.scaled, A)


## neighbours --------------------------------------------------------------


library(spdep)
# neighbors found from distance from centers - anything within 1km 
distance1km.nb <- dnearneigh(st_centroid(LDN_grid_map), d1=0, d2=1)
A.distance1km <- nb2listw(distance1km.nb,style="B", zero.policy = TRUE)

distance1.5km.nb <- dnearneigh(st_centroid(LDN_grid_map), d1=0, d2=1.5)
A.distance1.5km <- nb2listw(distance1.5km.nb,style="B", zero.policy = TRUE)

distance0.5km.nb <- dnearneigh(st_centroid(LDN_grid_map), d1=0, d2=0.5)
A.distance0.5km <- nb2listw(distance0.5km.nb,style="B", zero.policy = TRUE)

queen_shared_edge.nb <- poly2nb(LDN_grid_map,queen=TRUE)
A.queen_shared_edge <- nb2listw(queen_shared_edge.nb,style="B", zero.policy = TRUE)

rook_shared_edge.nb <- poly2nb(LDN_grid_map,queen=FALSE)
A.rook_shared_edge <- nb2listw(rook_shared_edge.nb,style="B", zero.policy = TRUE)

lag2.nb <- nblag(neighbours=queen_shared_edge.nb,maxlag=2)
A.lag2 <- nb2listw(nblag_cumul(lag2.nb),style="B", zero.policy = TRUE)

lag4.nb <- nblag(neighbours=queen_shared_edge.nb,maxlag=4)
A.lag4 <- nb2listw(nblag_cumul(lag4.nb),style="B", zero.policy = TRUE)


nearest4.nb <- knn2nb(knearneigh(st_centroid(LDN_grid_map),k=4), sym = TRUE)
A.nearest4 <- nb2listw(nearest4.nb,style="B", zero.policy = TRUE)

??knearneigh


image(nb2mat(distance.nb,zero.policy=TRUE, style="B"))


## regression --------------------------------------------------------------

library(spatialreg)

car.distance0.5km <- spautolm(formula = LDN_grid_map$cnt_AED ~
                               LDN_grid_map$pc_f.scaled + 
                               LDN_grid_map$pc_65_p.scaled +
                              LDN_grid_map$pc_50_p.scaled + 
                               LDN_grid_map$pc_bd_g.scaled +
                               LDN_grid_map$avg_dpr.scaled +
                               LDN_grid_map$pop_den.scaled+ 
                               LDN_grid_map$WD_pp_d.scaled + 
                               LDN_grid_map$cnt_spr.scaled +
                               LDN_grid_map$cnt_CHs.scaled, 
                           data = LDN_grid_map, listw=A.distance0.5km, family="CAR",
                           method = "Matrix_J")

test = summary(car.distance0.5km)
test$parameters
2 * 11 - (2*test)

save_result = function(output, modelname){
  filename = paste("regression_results/", modelname, ".csv", sep="")
  model_summary = summary(output)
  coefs = as.data.frame(model_summary$Coef)
  coefs$variable = names(model_summary$fit$coefficients)
  coefs$model = modelname
  
  model_ranking = list(modelname, model_summary$LL, output$fit$s2, 2 * model_summary$parameters - (2*model_summary$LL))
  
  write.csv(coefs, filename)
  write.table(model_ranking, "regression_results/model_ranking.csv",sep=",", append=TRUE, col.names = FALSE)
}

save_result(output = car.distance0.5km, modelname = "distance 0.5")

conduct_experiment = function(neighbour_matrix){
  print("starting next experiment")
  print( deparse(substitute(neighbour_matrix)))
  car.out <- spautolm(formula = LDN_grid_map$cnt_AED ~
                                  LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
                                  LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
                                  LDN_grid_map$pc_bd_g.scaled +
                                  LDN_grid_map$pc_65_p.scaled * LDN_grid_map$pop_den.scaled + 
                                  LDN_grid_map$avg_dpr.scaled +
                                  LDN_grid_map$WD_pp_d.scaled + 
                                  LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled, 
                                data = LDN_grid_map, listw=neighbour_matrix, family="CAR",
                      method = "Matrix_J")
  model_name = paste( "crossTerms4", "rmCHs", "rmPopDen", "bothAge", deparse(substitute(neighbour_matrix)), sep="_")
  
  print("saving result")
  save_result(output = car.out, modelname = model_name)
  summary(car.out)
}

conduct_experiment(A.queen_shared_edge)
conduct_experiment(A.rook_shared_edge)
conduct_experiment(A.lag2)
conduct_experiment(A.distance1km)
conduct_experiment(A.distance1.5km)
conduct_experiment(A.distance0.5km)
conduct_experiment(A.nearest4)
conduct_experiment(A.lag4)



## Final Model -------------------------------------------------------------

car.out <- spautolm(formula = LDN_grid_map$cnt_AED ~
                      LDN_grid_map$pc_f.scaled * LDN_grid_map$pop_den.scaled + 
                      LDN_grid_map$pc_50_p.scaled * LDN_grid_map$pop_den.scaled + 
                      LDN_grid_map$pc_bd_g.scaled + 
                      LDN_grid_map$avg_dpr.scaled +
                      LDN_grid_map$WD_pp_d.scaled + 
                      LDN_grid_map$cnt_spr.scaled * LDN_grid_map$pop_den.scaled+ 
                      LDN_grid_map$cnt_CHs.scaled, 
                    data = LDN_grid_map, listw=A.distance1km, family="CAR",
                    method = "Matrix_J")
summary(car.out)

car.out.dummy <- spautolm(formula = LDN_grid_map$cnt_AED ~ LDN_grid_map$pp_dn_s , 
                    data = LDN_grid_map, listw=A.distance1km, family="CAR",
                    method = "Matrix_J")
summary(car.out.dummy)

LDN_grid_map$fitted_vals = fitted(car.out)
LDN_grid_map$residuals = residuals(car.out)

write_sf(LDN_grid_map, "data/grids/ldn_grid_fitted.shp")

# RETIRED CODE ------------------------------------------------------------

## MSOA Map - Care Homes ---------------------------------------------------

ldn_boroughs = 'Havering|Barking and Dagenham|Barnet|Bexley|Brent|Bromley|Camden|City of London|Croydon|Ealing|Enfield|Greenwich|Hackney|Hammersmith and Fulham|Haringey|Harrow|Hillingdon|Hounslow|Islington|Kensington and Chelsea|Kingston upon Thames|Lambeth|Lewisham|Merton|Newham|Redbridge|Richmond upon Thames|Southwark|Sutton|Tower Hamlets|Waltham Forest|Wandsworth|Westminster'

# Creating london map of MSOAs
full_MSOA_map <- read_sf("maps/Middle_layer_Super_Output_Areas_December_2021")

full_MSOA_map <- select(full_MSOA_map, -c(MSOA21NMW, BNG_E, BNG_N))

st_crs(ldn_boundary_map)

# just taking the MSOA in London, using names
LDN_MSOA_map = dplyr::filter(full_MSOA_map, grepl(ldn_boroughs, MSOA21NM))
LDN_MSOA_map = dplyr::filter(LDN_MSOA_map, !grepl('Brentwood', MSOA21NM))


# plotting to check 
ggplot() + 
  geom_sf(data = ldn_boundary_map, alpha=0.2, colour="blue") +
  geom_sf(data = LDN_MSOA_map) + 
  geom_sf(data = ldn_boundary_map, alpha=0.2, colour="red") 

# saving
write_sf(LDN_MSOA_map, "maps/LDN_MSOA/ldn_MSOA_map.shp")



# Plots + Results -------------------------------------------------------------------

plot_descriptive_ldn(relevant_col = 'pop_den', 
                     map = LDN_grid_map,
                     title = "Population Density Interpolated", 
                     legend_title = "Residents per Sq Km")

plot_descriptive_ldn(relevant_col = 'avg_dpr', 
                     map = LDN_grid_map,
                     title = "Household Deprivation Interpolated", 
                     legend_title = "Average Dimensions \nof Deprivation")

queen_shared_edge.nb <- poly2nb(LDN_grid_map,queen=TRUE)
A.queen_shared_edge <- nb2listw(queen_shared_edge.nb,style="B", zero.policy = TRUE)

find_variable_stats = function(relevant_col, map = LDN_grid_map){
  stat.mean = mean(LDN_grid_map[[relevant_col]])
  stat.SD = sd(LDN_grid_map[[relevant_col]])
  moran_result =moran.test(LDN_grid_map[[relevant_col]], A.queen_shared_edge)
  stat.Istat = moran_result$estimate[1]
  stat.Ipval = moran_result$p.value
  
  print(paste("----- Stats for ", relevant_col, " ----- "))
  print(paste("Mean:", stat.mean))
  print(paste("SD:", stat.SD))
  print(paste("I Result:", stat.Istat))
  print(paste("I pval:", stat.Ipval))
}

find_variable_stats(relevant_col = 'pop_den')
find_variable_stats(relevant_col = 'pc_f')
find_variable_stats(relevant_col = 'pc_50_p')
find_variable_stats(relevant_col = 'pc_bd_g')
find_variable_stats(relevant_col = 'avg_dpr')
find_variable_stats(relevant_col = 'WD_pp_d')
find_variable_stats(relevant_col = 'cnt_spr')


## Reading in Coefficients -------------------------------------------------



read_coef_csv = function(filename){
  print(paste("---- Starting Import of", filename, "  ----"))
  import <- read_csv(filename)
  import <- import[-1]
  model_specs <- do.call(rbind,str_split(import$model,"_A."))
  import$variable_combination  <- model_specs[,1]
  import$distance_matrix  <- model_specs[,2]
  names(import) <- names(total_coef_matrix)
  total_coef_matrix <<- rbind(total_coef_matrix, import)
  print(paste("---- Completed Import of", filename, "  ----"))
}
total_coef_matrix = data.frame(test)

coef_files <- list.files(path="regression_results/to_import", pattern="*.csv", full.names=TRUE, recursive=FALSE)
for(file in coef_files){
  read_coef_csv(file)
}
unique(total_coef_matrix$model)

read_coef_csv("regression_results/age50_A.distance0.5km.csv")

import = read_csv("regression_results/age50_A.distance0.5km.csv")
import = import[-1]
model_specs = do.call(rbind,str_split(import$model,"_A."))
import$variable_combination  = model_specs[,1]
import$distance_matrix  = model_specs[,2]
names(import) = names(total_coef_matrix)
total_coef_matrix = rbind(total_coef_matrix, import)

total_coef_matrix$variable2 = sub("LDN_grid_map[$]", "", total_coef_matrix$variable)
total_coef_matrix$variable2 = sub("[.]scaled", "", total_coef_matrix$variable2)
as.character(total_coef_matrix$variable2)

total_coef_matrix %>% 
  arrange(length(as.character(total_coef_matrix$variable2))) %>%
  mutate(variable2 = factor(variable2, levels = unique(variable2))) %>%
  ggplot() +
    geom_point(aes(variable2, Estimate, 
                   size = p_val, colour=distance_matrix), 
               alpha=0.3) +
    scale_size_continuous(range = c(2,0.1)) +
  scale_colour_discrete(name = "Distance Matrix",  
                        palette = "hue")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Variable Name") +
  ylab("Coefficient Estimate") 


# Updating total coef thing -----------------------------------------------

total_coef_matrix2 = total_coef_matrix

total_coef_matrix2 = total_coef_matrix2[!(total_coef_matrix2$variable_combination == "crossTerms4_rmChs_bothAge"),]
total_coef_matrix2 = total_coef_matrix2[!(total_coef_matrix2$variable_combination == "crossTerms4_rmCHs_rmPopDen_bothAge"),]
total_coef_matrix2 = total_coef_matrix2[!(total_coef_matrix2$variable_combination == "full method_crossTermsAge_age50"),]

for (i in 1:length(unique(total_coef_matrix2$variable_combination))){
  current_combo = unique(total_coef_matrix2$variable_combination)[i]
  combo_name = paste("model",as.character(i))
  total_coef_matrix2$modelNum[total_coef_matrix2$variable_combination == current_combo] = combo_name
}

total_coef_matrix2[(total_coef_matrix2$distance_matrix == "queen_shared_edge"),]$distance_matrix = "Queen Adjacency"
total_coef_matrix2[(total_coef_matrix2$distance_matrix == "rook_shared_edge"),]$distance_matrix = "Rook Adjacency"


total_coef_matrix2$distance_matrix <- factor(total_coef_matrix2$distance_matrix, 
                                    levels=c("Queen Adjacency", "Rook Adjacency", "distance0.5km", "distance1km", "distance1.5km",
                                             "lag2", "lag4", "nearest4"))
total_coef_matrix2$modelNum <- factor(total_coef_matrix2$modelNum, 
                             levels=c("model 1", "model 2", "model 3", "model 4", "model 5",
                                      "model 6", "model 7", "model 8", "model 9", "model 10"))

total_coef_matrix2 %>% 
  arrange(length(as.character(total_coef_matrix2$variable2))) %>%
  mutate(variable2 = factor(variable2, levels = unique(variable2))) %>%
  ggplot() +
  geom_point(aes(variable2, Estimate, 
                 size = p_val, colour=distance_matrix), 
             alpha=0.3) +
  scale_size_continuous(range = c(2,0.1)) +
  scale_colour_discrete(name = "Distance Matrix",  
                        palette = "hue")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank()) +
  xlab("Variable Name") +
  ylab("Coefficient Estimate") 


total_coef_matrix2$variable2 = sub("cat", "Female Proportion", total_coef_matrix2$variable2)

total_coef_matrix2$variable2 = sub("pc_50_p","50+ Proportion", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("pc_bd_g","Bad General Health Proportion", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("avg_dpr","Average Household Deprivation", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("pop_den","Population Density", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("WD_pp_d","Workday Population Density", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("cnt_spr","Number of Sports Sites", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("cnt_CHs","Number of Care Facilities", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("pc_65_p","65+ Proportion", total_coef_matrix2$variable2)

total_coef_matrix2$variable2 = sub("Household","Dim.", total_coef_matrix2$variable2)
total_coef_matrix2$variable2 = sub("X"," x ", total_coef_matrix2$variable2)


total_coef_matrix2$distance_matrix = sub("distance","Distance", total_coef_matrix2$distance_matrix)


## Plotting AIC ------------------------------------------------------------



AIC_list <- read_csv("regression_results/model_ranking.csv", 
                     +     col_types = cols(...6 = col_skip(), ...7 = col_skip()), 
                     +     skip = 1)
AIC_list = AIC_list[-6:-7]
names(AIC_list)[5] = "AIC"
AIC_list = AIC_list[-1:-14,]
AIC_list$AIC <- as.numeric(AIC_list$AIC)
AIC_model_specs <- do.call(rbind,str_split(AIC_list$X.model.name.,"_A[.]"))
AIC_list$variable_combination  <- AIC_model_specs[,1]
AIC_list$distance_matrix  <- AIC_model_specs[,2]

AIC_list2 = AIC_list[5:7]

test_table = table(x=AIC_list2$distance_matrix,y=AIC_list2$variable_combination)

AIC_list2 = AIC_list2[!(AIC_list2$variable_combination == "crossTerms4_rmChs_bothAge"),]
AIC_list2 = AIC_list2[!(AIC_list2$variable_combination == "crossTerms4_rmCHs_rmPopDen_bothAge"),]
AIC_list2 = AIC_list2[!(AIC_list2$variable_combination == "full method_crossTermsAge_age50"),]

for (i in 1:length(unique(AIC_list2$variable_combination))){
  current_combo = unique(AIC_list2$variable_combination)[i]
  combo_name = paste("model",as.character(i))
  AIC_list2$modelNum[AIC_list2$variable_combination == current_combo] = combo_name
}

AIC_list2[(AIC_list2$distance_matrix == "queen_shared_edge"),]$distance_matrix = "Queen Adjacency"
AIC_list2[(AIC_list2$distance_matrix == "rook_shared_edge"),]$distance_matrix = "Rook Adjacency"

minimum <- AIC_list2[which.min(AIC_list2$AIC),]

AIC_list2$distance_matrix <- factor(AIC_list2$distance_matrix, 
<<<<<<< HEAD
                                    levels=c("Queen Adjacency", "Rook Adjacency", "Distance 0.5km", "Distance 1km", "Distance 1.5km",
                                             "Lag 2", "Lag 4", "Nearest 4"))
AIC_list2$modelNum <- factor(AIC_list2$modelNum, 
                                    levels=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5",
                                             "Model 6", "Model 7", "Model 8", "Model 9", "Model 10"))

ggplot(AIC_list2, aes(distance_matrix, modelNum, fill= AIC)) + 
  geom_tile() +
  geom_label(data = minimum, fill= "white", alpha=0.7, 
             size = 2.5, aes(label = "min AIC")) + 
  scale_fill_continuous(name = "AIC", 
                        palette = "viridis") +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Distance Matrix") +
  ylab("Regression Model") 

AIC_list2$distance_matrix = sub("distance","Distance ", AIC_list2$distance_matrix)
AIC_list2$distance_matrix = sub("lag","Lag ", AIC_list2$distance_matrix)
AIC_list2$distance_matrix = sub("nearest","Nearest ", AIC_list2$distance_matrix)
AIC_list2$modelNum = sub("model","Model", AIC_list2$modelNum) 
