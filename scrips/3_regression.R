

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

# saving results
moran_I_results <- data.frame(var_name = character(),
                              estimate = numeric(),
                              p.value = numeric())

for (col in scaled_cols){
  moran_test <- moran.test((LDN_grid[[col]]), A.queen_shared_edge)
  moran_I_results[nrow(moran_I_results) + 1,]  <- list(col, 
                                                       moran_test$estimate[1],
                                                       moran_test$p.value)
}

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


# Neighbourhood Matrices --------------------------------------------------

# neighbors found from distance from centers
distance0.5km.nb <- dnearneigh(st_centroid(LDN_grid), d1=0, d2=0.5)
A.distance0.5km <- nb2listw(distance0.5km.nb,style="B", zero.policy = TRUE)

distance1km.nb <- dnearneigh(st_centroid(LDN_grid), d1=0, d2=1)
A.distance1km <- nb2listw(distance1km.nb,style="B", zero.policy = TRUE)

distance1.5km.nb <- dnearneigh(st_centroid(LDN_grid), d1=0, d2=1.5)
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

nearest4.nb <- knn2nb(knearneigh(st_centroid(LDN_grid),k=4), sym = TRUE)
A.nearest4 <- nb2listw(nearest4.nb,style="B", zero.policy = TRUE)

A_matrices <- list(A.distance0.5km, A.distance1km, A.distance1.5km, 
                A.queen_shared_edge, A.rook_shared_edge,
                A.lag2, A.lag4, A.nearest4)


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


# Regression --------------------------------------------------------------

# initializing a model ranking csv
write.csv(c("Model Name", "Log Likelihood", "ML Residual Variance", "AIC"), 
          "results/regression_results/model_ranking.csv", col.names=FALSE)

for (n_model in 1:10){
  if(n_model == 1) {model = model_1; model_chr = "model_1"}
  if(n_model == 2) {model = model_2; model_chr = "model_2"}
  if(n_model == 3) {model = model_3; model_chr = "model_3"}
  if(n_model == 4) {model = model_4; model_chr = "model_4"}
  if(n_model == 5) {model = model_5; model_chr = "model_5"}
  if(n_model == 6) {model = model_6; model_chr = "model_6"}
  if(n_model == 7){ model = model_7; model_chr = "model_7"}
  if(n_model == 8) {model = model_8; model_chr = "model_8"}
  if(n_model == 9) {model = model_9; model_chr = "model_9"}
  if(n_model == 10) {model = model_10; model_chr = "model_10"}
  
  print(paste("Starting investigation for:", model_chr))
  
  for(a_matrix_num in 1:8){
    if(a_matrix_num == 1) {a_matrix = A.distance0.5km; A_chr = "A.distance0.5km"}
    if(a_matrix_num == 2) {a_matrix = A.distance1km; A_chr = "A.distance1km"}
    if(a_matrix_num == 3) {a_matrix = A.distance1.5km; A_chr = "A.distance1.5km"}
    if(a_matrix_num == 4){ a_matrix = A.queen_shared_edge; A_chr = "A.queen_shared_edge"}
    if(a_matrix_num == 5) {a_matrix = A.rook_shared_edge; A_chr = "A.rook_shared_edge"}
    if(a_matrix_num == 6) {a_matrix = A.lag2; A_chr = "A.lag2"}
    if(a_matrix_num == 7) {a_matrix = A.lag4; A_chr = "A.lag4"}
    if(a_matrix_num == 8) {a_matrix = A.nearest4; A_chr = "A.nearest4"}
    
    print(paste("Starting investigation for:", A_chr))
    
    # performing regression
    car_output <- spautolm(formula = model, 
                        data = LDN_grid, listw=a_matrix, 
                        family="CAR",
                        method = "Matrix_J")
    
    # saving result
    unique_model_name = paste(model_chr, A_chr, sep="_")
    print("step 3")
    filename = paste("results/regression_results/", unique_model_name,
                     ".csv", sep="")
    
    model_summary = summary(car_output)
    coefs = as.data.frame(model_summary$Coef)
    coefs$variable = names(model_summary$fit$coefficients)
    coefs$model = unique_model_name
    
    model_ranking = list(unique_model_name, 
                         model_summary$LL, 
                         car_output$fit$s2, 
                         2 * model_summary$parameters - (2*model_summary$LL))
    
    print("Saving Files")
    
    write.csv(coefs, filename)
    write.table(model_ranking, "results/regression_results/model_ranking.csv",
                sep=",", append=TRUE, col.names = FALSE)
    
    print(paste("Finished Investigation for:", model_chr, A_chr))
  }
 
}


