# Barchart to see the number of defibrillators by deprivation 

library(sf)
library(dplyr)

# Reading the map file
LSOA_map <- read_sf("data/LSOA_map-pop_wd_hd_aed.shp")
LDN_grid_map.300 <- read_sf("data/grid_map_300.shp")


# Finding quartile
library(dplyr)
LSOA_map <- mutate(LSOA_map, dep_quartile = ntile(hos_dpr, 4))
LDN_grid_map.300 <- mutate(LDN_grid_map.300, dep_quartile = ntile(hos_dpr, 5))

plot(LDN_grid_map.300["dep_quartile"])

# Counting AEDs per quartile

AEDs_by_dpr <- LSOA_map %>%
  group_by(dep_quartile) %>%
  summarise(count_AEDs = sum(cnt_AED), 
            avg_dpr = mean(`hos_dpr`),
            population = sum(`M-2024P`), 
            WD_population = sum(WD_pp_d))

AEDs_by_dpr = st_drop_geometry(AEDs_by_dpr)


AEDs_by_dpr_grid <- LDN_grid_map.300 %>%
  group_by(dep_quartile) %>%
  summarise(count_AEDs = sum(cnt_AED), 
            population_density = mean(`ppltn_d`),
            avg_dpr = mean(`hos_dpr`))

AEDs_by_dpr_grid = st_drop_geometry(AEDs_by_dpr_grid)


# Creating a plot

library(ggplot2)
ggplot(AEDs_by_dpr) +
  geom_col(aes(x=dep_quartile, y=count_AEDs, fill=dep_quartile), show.legend = FALSE) + 
  geom_line(aes(x=dep_quartile, y=population / 1000), col="red", lwd = 1) +
  #geom_line(aes(x=dep_quartile, y=WD_population / 1000)) +
  scale_y_continuous(sec.axis = 
                       sec_axis(transform = ~ . * 1000, 
                                name = "Population", 
                                labels = scales::label_number()), 
                     name = "Number of AEDs") +
  labs(x = "Deprivation Quartile") + 
  ggtitle("Deprivation vs Number of AEDs") +
  scale_fill_steps(n.breaks = 4, limits = c(0,4))


library(ggrepel)
ggplot(AEDs_by_dpr_grid) +
  geom_col(aes(x=dep_quartile, y=count_AEDs, fill=dep_quartile), show.legend = FALSE) + 
  geom_line(aes(x=dep_quartile, y=population_density * (300/1000) / 1), col="red", lwd = 1) +
  scale_y_continuous(sec.axis = 
                       sec_axis(transform = ~ . * 1, 
                                name = "Estimated Population", 
                                labels = scales::label_number()), 
                     name = "Number of AEDs") +
  geom_label_repel(label = "Estimated Population",
                   nudge_x = 1,
                   na.rm = TRUE) + 
  labs(x = "Deprivation Quartile") + 
  ggtitle("Deprivation vs Number of AEDs") +
  scale_fill_steps(n.breaks = 4, limits = c(0,4))


ggplot(AEDs_by_dpr) +
  geom_col(aes(x=dep_quartile, y=count_AEDs, fill=dep_quartile), show.legend = FALSE) +
  labs(x = "Deprivation Quartile", y="Number of AEDs") + 
  scale_fill_steps(n.breaks = 4, limits = c(0,4))


