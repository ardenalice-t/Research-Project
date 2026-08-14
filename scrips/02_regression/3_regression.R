

# Regression --------------------------------------------------------------

# Set Up ------------------------------------------------------------------


## Packages ----------------------------------------------------------------

library(sf)
library(reshape)
library(ggplot2)
library(spdep)
library(spatialreg)
library(dplyr)

## Functions ---------------------------------------------------------------

source("src/plot_descriptive_ldn.R")
source("src/clean_coef_names.R")
source("src/plotRegression.R")

## Files -------------------------------------------------------------------

# Reading in grid map from 2_interpolating_to_grid.R
LDN_grid <- read_sf("outputs/01_interpolation/data/interpolated_LDN_grid.gpkg")


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
                                                       moran_test$statistic, # not sure if this is supposed to be statistic or estimate
                                                       moran_test$p.value)
}

# saving results
write.csv(moran_I_results, "outputs/02_regression/data/scaled_cols_morans_I.csv")
write_sf(LDN_grid, "outputs/02_regression/data/scaled_data_ldn.gpkg")

# Testing Correlation -----------------------------------------------------

variable_correlation <-cor(st_drop_geometry(LDN_grid[scaled_cols]))
variable_correlation <- melt(variable_correlation)

ggplot(variable_correlation, aes(X1, X2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "red", high = "darkgreen", mid="white") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0))

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

linear_summary <- summary(linear.out); linear_summary

linear_AIC <- 2 * linear.out$rank - (2*logLik(linear.out))

print(paste("Linear Model AIC:",
            round(linear_AIC)))

head(linear.out$residuals)

linear_residual_moranI <- moran.test(linear.out$residuals, A.queen_shared_edge)

print(paste("Moran's I Statistic Estimate:",
            linear_residual_moranI$statistic))
print(paste("Moran's I Statistic p value:",
            linear_residual_moranI$p.value))


# Neighborhood Matrices --------------------------------------------------

# Used in creating the neighborhood matrices
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

model_2 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  #LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_3 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  #LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_4 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  #LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_5 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled
  #LDN_grid$count_CHs.scaled

model_6 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  #LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled

model_7 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_f.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pop_den.scaled

model_8 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_f.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_sports.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_CHs.scaled : LDN_grid$pop_den.scaled

model_9 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  #LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_f.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pop_den.scaled +
  LDN_grid$count_sports.scaled : LDN_grid$pop_den.scaled

# Could make this into a non-linear model
model_10 <- LDN_grid$count_AEDs ~
  LDN_grid$pc_f.scaled +
  LDN_grid$pc_65_plus.scaled +
  LDN_grid$pc_50_plus.scaled +
  LDN_grid$pc_bad_gh.scaled +
  LDN_grid$avg_hos_dpr.scaled +
  LDN_grid$pop_den.scaled+
  LDN_grid$WD_pop_den.scaled +
  LDN_grid$count_sports.scaled +
  LDN_grid$count_CHs.scaled +
  LDN_grid$pc_50_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_65_plus.scaled : LDN_grid$pc_f.scaled  +
  LDN_grid$pc_bad_gh.scaled : LDN_grid$pc_f.scaled

models <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7,
            model_8, model_9, model_10)

names(models) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                   "model_6", "model_7", "model_8", "model_9", "model_10")


# Regression --------------------------------------------------------------

# initializing a model ranking csv
# write.csv(data.frame("Formula", "Neighbor Matrix", "Log Likelihood",
#                      "ML Residual Variance", "AIC"),
#           "outputs/02_regression/data/model_ranking.csv", col.names = FALSE)

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

    # Creating a dataframe of coefficient data
    unique_model_name = paste(model_name, Amat_name, sep="_")
    filename = paste("outputs/02_regression/data/regression_results/", unique_model_name,
                     ".csv", sep="")

    model_summary = summary(car_output)
    coefs = as.data.frame(model_summary$Coef)
    coefs$variable = names(model_summary$fit$coefficients)
    coefs$formula = model_name
    coefs$neighbours = Amat_name

    # Creating a summary of model fit
    model_ranking = list(model_name,
                         Amat_name,
                         model_summary$LL,
                         car_output$fit$s2,
                         2 * model_summary$parameters - (2*model_summary$LL))

    # Saving model files
    print("Saving Files")

    write.csv(coefs, filename)
    write.table(model_ranking, "outputs/02_regression/data/model_ranking.csv",
                sep=",", append=TRUE, col.names = FALSE)

    print(paste("Finished Investigation for: ", model_name, Amat_name))
  }

}


# Final Model -------------------------------------------------------------

final_model <- model_9
final_matrix <- A.rook_shared_edge

car_output <- spautolm(formula = final_model,
                       data = LDN_grid, listw=final_matrix,
                       family="CAR",
                       method = "Matrix_J")

# Saving final result
unique_model_name = "selected_model"
filename = paste("outputs/02_regression/data/", unique_model_name,
                 ".csv", sep="")

model_summary = summary(car_output)
coefs = as.data.frame(model_summary$Coef)
coefs$variable = names(model_summary$fit$coefficients)
coefs$formula = "model_9"
coefs$neighbours = "A.rook_shared_edge"
write.csv(coefs, filename)

logLik(car_output) - logLik(linear.out)
final_AIC <- 2 * model_summary$parameters - (2*model_summary$LL)

final_AIC - linear_AIC


# Comparing residual spatial correlation to linear test

CAR_residual_moranI <- moran.test(car_output$fit$residuals, A.queen_shared_edge)

print(paste("CAR Moran's I Statistic:",
              CAR_residual_moranI$statistic))
print(paste("CAR Moran's I Statistic p value:",
            CAR_residual_moranI$p.value))

print(paste("Difference Moran's I Statistic:",
            linear_residual_moranI$statistic -
              CAR_residual_moranI$statistic))

print(paste("Percentage Change:",
            round((CAR_residual_moranI$statistic -
              linear_residual_moranI$statistic) /
              linear_residual_moranI$statistic, 4) * 100, "%"))


# Looking at coefficient correlation
coef_cov <- car_output$fit$imat
coef_cor <- cov2cor(coef_cov)
rownames(coef_cor) <- clean_coef_names(rownames(coef_cor), abbreviations = TRUE)
colnames(coef_cor) <- clean_coef_names(colnames(coef_cor), abbreviations = TRUE)


coef_cor <- melt(coef_cor)
ggplot(coef_cor, aes(X1, X2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "red", high = "darkgreen", mid="white", midpoint=0, limits=c(-1,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# viewing signal non spatial and spatial
LDN_grid$signal_trend <- car_output$fit$signal_trend
LDN_grid$signal_stochastic <- car_output$fit$signal_stochastic

plot_descriptive_ldn("signal_trend", "Non-Spatial Demand", "Signal Trend",
                     LDN_grid)
plot_descriptive_ldn("signal_stochastic", "Spatial Demand", "Signal Stochastic",
                     LDN_grid)


# Attaching fitted values
LDN_grid$AED_demand <- car_output$fit$fitted.values


plot_descriptive_ldn("AED_demand",title = "AED Demand",
                     legend_title = "Number of AEDs", map = LDN_grid,
                     cap=TRUE, max_val = 3, bins=5)
plotRegresssion("AED_demand",title = "AED Demand", map = LDN_grid,
                max_relevant_val = 6)


# Saving Model ------------------------------------------------------------

write_sf(LDN_grid, "outputs/02_regression/data/regressed_data_ldn.gpkg")
