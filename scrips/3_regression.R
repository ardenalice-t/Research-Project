

# Regression --------------------------------------------------------------


# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(spdep)
library(spatialreg)
library(dplyr)

## Functions ---------------------------------------------------------------


## Files -------------------------------------------------------------------

# Reading in grid map from 2_interpolating_to_grid.R
LDN_grid <- read_sf("data/Grid_data_ldn_2026_07_31.gpkg")


# Scaling -----------------------------------------------------------------

# Scaling all relevant variables
regression_cols = c("pc_f", "pc_50_plus", "pc_65_plus", "pc_bad_gh", "avg_hos_dpr",
                    "pop_den", "WD_pop_den", "count_sports", "count_CHs")
scaled_cols = c()
for (col in regression_cols){
  scaled_col = paste(col, ".scaled", sep="")
  scaled_cols = append(scaled_cols, scaled_col)
  LDN_grid[scaled_col] = scale(st_drop_geometry(LDN_grid[col]))
}


# Testing results

plot(LDN_grid["avg_hos_dpr.scaled"])


# Testing spatial correlation

# queen neighborhood matrix 
queen_shared_edge.nb <- poly2nb(LDN_grid,queen=TRUE)
A.queen_shared_edge <- nb2listw(queen_shared_edge.nb,style="B", zero.policy = TRUE)


moran_I_results <- data.frame(var_name = character(),
                              estimate = numeric(),
                              p.value = numeric())

for (col in scaled_cols){
  moran_test <- moran.test((LDN_grid[[col]]), A.queen_shared_edge)
  moran_I_results[nrow(moran_I_results) + 1,]  <- list(col, 
                                                       moran_test$estimate[1],
                                                       moran_test$p.value)
}

# saving results
write.csv(moran_I_results, "results/scaled_cols_morans_I.csv")


# Linear Test -------------------------------------------------------------

linear_model = LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
                   LDN_grid$pc_65_plus.scaled +
                   LDN_grid$pc_50_plus.scaled + 
                   LDN_grid$pc_bad_gh.scaled +
                   LDN_grid$avg_hos_dpr.scaled +
                   LDN_grid$pop_den.scaled+ 
                   LDN_grid$WD_pop_den.scaled + 
                   LDN_grid$count_sports.scaled +
                   LDN_grid$count_CHs.scaled

linear.out <- lm(formula = linear_model, 
                 data = LDN_grid)

summary(linear.out)

head(linear.out$residuals)

linear_residual_moranI <- moran.test(linear.out$residuals, A.queen_shared_edge)

print(paste("Moran's I Statistic Estimate:",
            linear_residual_moranI$estimate[1]))
print(paste("Moran's I Statistic p value:",
            linear_residual_moranI$p.value))


# Neighborhood Matrices --------------------------------------------------

# Used in creating the neighbourhood matrices
LDN_grid_centroid <- st_centroid(LDN_grid)

# neighbors found from distance from centers
distance0.5km.nb <- dnearneigh(LDN_grid_centroid, d1=0, d2=500)
A.distance0.5km <- nb2listw(distance0.5km.nb,style="B", zero.policy = TRUE)

distance1km.nb <- dnearneigh(LDN_grid_centroid, d1=0, d2=1000)
A.distance1km <- nb2listw(distance1km.nb,style="B", zero.policy = TRUE)

distance1.5km.nb <- dnearneigh(LDN_grid_centroid, d1=0, d2=1500)
A.distance1.5km <- nb2listw(distance1.5km.nb,style="B", zero.policy = TRUE)

# Adjacency matrices

queen_shared_edge.nb <- poly2nb(LDN_grid,queen=TRUE)
A.queen_shared_edge <- nb2listw(queen_shared_edge.nb,style="B", zero.policy = TRUE)

rook_shared_edge.nb <- poly2nb(LDN_grid,queen=FALSE)
A.rook_shared_edge <- nb2listw(rook_shared_edge.nb,style="B", zero.policy = TRUE)

# Lag matrices

lag2.nb <- nblag(neighbours=queen_shared_edge.nb,maxlag=2)
A.lag2 <- nb2listw(nblag_cumul(lag2.nb),style="B", zero.policy = TRUE)

lag4.nb <- nblag(neighbours=queen_shared_edge.nb,maxlag=4)
A.lag4 <- nb2listw(nblag_cumul(lag4.nb),style="B", zero.policy = TRUE)

# Nearest K neighbors

nearest4.nb <- knn2nb(knearneigh(LDN_grid_centroid,k=4), sym = TRUE)
A.nearest4 <- nb2listw(nearest4.nb,style="B", zero.policy = TRUE)

A_matrices <- list(A.distance0.5km, A.distance1km, A.distance1.5km, 
                A.queen_shared_edge, A.rook_shared_edge,
                A.lag2, A.lag4, A.nearest4)

names(A_matrices) <- list("A.distance0.5km", "A.distance1km", "A.distance1.5km", 
                          "A.queen_shared_edge", "A.rook_shared_edge",
                          "A.lag2", "A.lag4", "A.nearest4")

# Visualizing one
#plot(LDN_grid$geom)
#plot.nb(distance1km.nb, LDN_grid$geom, add = TRUE, col='red', points=FALSE)  


# Models ------------------------------------------------------------------

model_1 <- LDN_grid$count_AEDs ~ 
  LDN_grid$pc_f.scaled + 
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled + 
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_2 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled

model_3 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
  LDN_grid$pc_65_plus.scaled * LDN_grid$pop_den.scaled+
  LDN_grid$pc_50_plus.scaled * LDN_grid$pop_den.scaled+ 
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_4 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_65_plus.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_bad_gh.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$avg_hos_dpr.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_5 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_65_plus.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_bad_gh.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$avg_hos_dpr.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$pop_den.scaled + 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled * LDN_grid$pop_den.scaled +
  LDN_grid$count_CHs.scaled * LDN_grid$pop_den.scaled

model_6 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled

model_7 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
  LDN_grid$pc_50_plus.scaled + 
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_8 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled + 
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled + 
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_9 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_50_plus.scaled * LDN_grid$pop_den.scaled + 
  LDN_grid$pc_bad_gh.scaled + 
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled * LDN_grid$pop_den.scaled+ 
  LDN_grid$count_CHs.scaled

model_10 <- LDN_grid$count_AEDs ~ LDN_grid$pc_f.scaled^2 + 
  LDN_grid$pc_65_plus.scaled^2+
  LDN_grid$pc_50_plus.scaled^2 + 
  LDN_grid$pc_bad_gh.scaled^2 +
  LDN_grid$avg_hos_dpr.scaled^2 +
  LDN_grid$pop_den.scaled+ 
  LDN_grid$WD_pop_den.scaled + 
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

models <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7, 
            model_8, model_9, model_10)
names(models) <- c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6", "model_7", 
                      "model_8", "model_9", "model_10")


# Regression --------------------------------------------------------------

# initializing a model ranking csv
write.csv(data.frame("Formula", "Neighbour Matrix", "Log Likelihood", "ML Residual Variance", "AIC"), 
          "results/regression_results/model_ranking.csv", col.names = FALSE)

for (n_model in 1:(length(models))){
  model_name <- names(models[n_model])
  model <- models[[model_name]]
  
  print(paste("----", "Starting investigation for:", model_name, "----"))
  
  for(n_Amat in 1:(length(A_matrices))){
    Amat_name <- names(A_matrices[n_Amat])
    Amat <- A_matrices[[Amat_name]]
    
    print(paste("Using neighbourhood matrix:", Amat_name))
    
    # performing regression
    car_output <- spautolm(formula = model, 
                        data = LDN_grid, listw=Amat, 
                        family="CAR",
                        method = "Matrix_J")
    
    # saving result
    unique_model_name = paste(model_name, Amat_name, sep="_")
    filename = paste("results/regression_results/", unique_model_name,
                     ".csv", sep="")
    
    model_summary = summary(car_output)
    coefs = as.data.frame(model_summary$Coef)
    coefs$variable = names(model_summary$fit$coefficients)
    coefs$formula = model_name
    coefs$neighbours = Amat_name
    
    model_ranking = list(model_name, 
                         Amat_name,
                         model_summary$LL, 
                         car_output$fit$s2, 
                         2 * model_summary$parameters - (2*model_summary$LL))
    
    print("Saving Files")
    
    write.csv(coefs, filename)
    write.table(model_ranking, "results/regression_results/model_ranking.csv",
                sep=",", append=TRUE, col.names = FALSE)
    
    print(paste("Finished Investigation for:", model_name, Amat_name, "!"))
  }
 
}


