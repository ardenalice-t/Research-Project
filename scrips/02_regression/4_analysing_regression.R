
# Analyzing Regression Results --------------------------------------------

# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(readr)
library(ggplot2) # for plotting
library(dplyr) # for select

## Functions ---------------------------------------------------------------

read_coef_csv = function(filename){
  print(paste("---- Starting Import of", filename, "  ----"))

  import <- read_csv(filename, col_types = cols())
  import <- import[-1]

  ifelse(exists("total_coef_matrix"),
         total_coef_matrix <<- rbind(total_coef_matrix, import),
         assign("total_coef_matrix", import, envir = .GlobalEnv))

  print(paste("---- Completed Import of", filename, "  ----"))
}

plot_coef_estimates = function(dataframe){
  dataframe %>%
    arrange(length(as.character(dataframe$variable))) %>%
    mutate(variable = factor(variable, levels = unique(variable))) %>%
    ggplot() +
    geom_point(aes(variable, Estimate,
                   size = .data[["Pr(>|z|)"]], colour=neighbours),
               alpha=0.3) +
    geom_hline(yintercept = 0, color = "darkgray") +
    scale_size_continuous(range = c(2,0.01), name = "p value",
                          breaks =c(0.2, 0.4, 0.6, 0.8, 1), limits = c(1, 0)) +
    scale_colour_discrete(name = "Distance Matrix",
                          palette = "hue")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Variable Name") +
    ylab("Coefficient Estimate")
}

source("src/clean_coef_names.R")

## Files -------------------------------------------------------------------

# Reading in Coefficients -------------------------------------------------

# Importing saved coefficients
coef_files <- list.files(path="outputs/02_regression/data/regression_results", pattern="*.csv",
                         full.names=TRUE, recursive=FALSE)

for(file in coef_files){
  read_coef_csv(file)
}

# Check import finished correctly
print(length(coef_files) == length(unique(total_coef_matrix$formula)) *
  length(unique(total_coef_matrix$neighbours)))

# Cleaning variable names
total_coef_matrix$variable <- clean_coef_names(total_coef_matrix$variable,
                                               pc_symbol = TRUE,
                                               abbreviations = TRUE)


# Plotting Regression Coefficients ----------------------------------------

# Organizing according to variable name length
total_coef_matrix = total_coef_matrix %>%
  arrange(length(as.character(total_coef_matrix$variable))) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

larger_coefs = unique(total_coef_matrix$variable)[1:9]

larger_coef_matrix = total_coef_matrix[total_coef_matrix$variable %in% larger_coefs,]
smaller_coef_matrix = total_coef_matrix[!total_coef_matrix$variable %in% larger_coefs,]

# Plotting results
plot_coef_estimates(larger_coef_matrix)
plot_coef_estimates(smaller_coef_matrix)
plot_coef_estimates(total_coef_matrix)


# Calculating Correlation -------------------------------------------------

by_model_coefs = total_coef_matrix %>% group_by(formula)
nModels = max(group_indices(by_model_coefs))

correlations =  data.frame(Model1=character(),
                   Model2=character(),
                   Correlation=numeric(),
                   p_value=numeric())


for(model1_idx in 2:nModels){
  for(model2_idx in 1:(model1_idx-1)){
    model1 = ungroup(total_coef_matrix[group_indices(by_model_coefs)==model1_idx,])
    model2 =  ungroup(total_coef_matrix[group_indices(by_model_coefs)==model2_idx,])

    model1_chr = group_keys(by_model_coefs)[model1_idx,][[1]]
    model2_chr = group_keys(by_model_coefs)[model2_idx,][[1]]

    model1 = model1[model1$variable %in% model2$variable,]
    model2 = model2[model2$variable %in% model1$variable,]

    model1 = model1 %>%  arrange(variable)
    model2 = model2 %>%  arrange(variable)

    cor = cor(model1$Estimate, model2$Estimate)
    cor_test <- cor.test(model1$Estimate, model2$Estimate,alternative = "greater")
    correlations[nrow(correlations) + 1,] = list(model1_chr, model2_chr,
                                                 cor, cor_test$p.value)

    print(paste("Testing correlation for models:", model1_chr,
                "and", model2_chr))

    print(paste("Correlation value:", cor))
  }
}

print(paste("Mean correlation across all models:", mean(correlations$Correlation)))
print(paste("SD of correlation across all models:", sd(correlations$Correlation)))

print(paste("Mean p value across all models:", mean(correlations$p_value)))
print(paste("SD of p value across all models:", sd(correlations$p_value)))

print(paste("Combination with minimum correlation:") )
print(tibble(correlations[which.min(correlations$Correlation),]))

# Realised that the combination rows, and def any squared rows are no longer standardised.
# so maybe i have to add them in earlier and standardise them then ?


# AIC Plot ----------------------------------------------------------------

# Reading file
model_rankings <- read_csv("outputs/02_regression/data/model_ranking.csv",
                     skip = 1)[-1]

# Cleaning names
model_rankings$`Neighbor Matrix`  = sub("A[.]","", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("lag","Lag ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix`  = sub("distance","Distance ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("nearest","Nearest ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("km"," km", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("_shared_edge"," Adjacency", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("queen","Queen", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("rook","Rook", model_rankings$`Neighbor Matrix`)
model_rankings$Formula = sub("model_","Model ", model_rankings$Formula)


# Ordering Combinations
model_rankings$`Neighbor Matrix` <- factor(model_rankings$`Neighbor Matrix`,
                                    levels=
                                      c("Queen Adjacency", "Rook Adjacency",
                                        "Lag 2", "Lag 4",  "Nearest 4",
                                        "Distance 0.5 km", "Distance 1 km",
                                        "Distance 1.5 km" ))
model_rankings$Formula<- factor(model_rankings$Formula,
                             levels=c("Model 1","Model 2","Model 3","Model 4",
                                      "Model 5","Model 6","Model 7","Model 8",
                                      "Model 9","Model 10"))


# Finding minimum AIC
minimum <- model_rankings[which.min(model_rankings$AIC),]

# Plotting heatmap
ggplot(model_rankings, aes(`Neighbor Matrix`, `Formula`, fill= AIC)) +
  geom_tile() +
  geom_label(data = minimum, fill= "white", alpha=0.7,
             size = 2.5, aes(label = "min AIC")) +
  scale_fill_continuous(name = "AIC",
                        palette = "viridis") +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Neighbourhood Matrix") +
  ylab("Regression Model")



# REPEAT FOR GRADATED -----------------------------------------------------


# Importing saved coefficients
coef_files <- list.files(path="outputs/02_regression/data/regression_results_gradated", pattern="*.csv",
                         full.names=TRUE, recursive=FALSE)

for(file in coef_files){
  read_coef_csv(file)
}

# Check import finished correctly
print(length(coef_files) == length(unique(total_coef_matrix$formula)) *
        length(unique(total_coef_matrix$neighbours)))

# Cleaning variable names
total_coef_matrix$variable <- clean_coef_names(total_coef_matrix$variable,
                                               pc_symbol = TRUE,
                                               abbreviations = TRUE)


# Plotting Regression Coefficients ----------------------------------------

# Organizing according to variable name length
total_coef_matrix = total_coef_matrix %>%
  arrange(length(as.character(total_coef_matrix$variable))) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

larger_coefs = unique(total_coef_matrix$variable)[1:9]

larger_coef_matrix = total_coef_matrix[total_coef_matrix$variable %in% larger_coefs,]
smaller_coef_matrix = total_coef_matrix[!total_coef_matrix$variable %in% larger_coefs,]

# Plotting results
plot_coef_estimates(larger_coef_matrix)
plot_coef_estimates(smaller_coef_matrix)
plot_coef_estimates(total_coef_matrix)


# Calculating Correlation -------------------------------------------------

by_model_coefs = total_coef_matrix %>% group_by(formula)
nModels = max(group_indices(by_model_coefs))

correlations =  data.frame(Model1=character(),
                           Model2=character(),
                           Correlation=numeric(),
                           p_value=numeric())


for(model1_idx in 2:nModels){
  for(model2_idx in 1:(model1_idx-1)){
    model1 = ungroup(total_coef_matrix[group_indices(by_model_coefs)==model1_idx,])
    model2 =  ungroup(total_coef_matrix[group_indices(by_model_coefs)==model2_idx,])

    model1_chr = group_keys(by_model_coefs)[model1_idx,][[1]]
    model2_chr = group_keys(by_model_coefs)[model2_idx,][[1]]

    model1 = model1[model1$variable %in% model2$variable,]
    model2 = model2[model2$variable %in% model1$variable,]

    model1 = model1 %>%  arrange(variable)
    model2 = model2 %>%  arrange(variable)

    cor = cor(model1$Estimate, model2$Estimate)
    cor_test <- cor.test(model1$Estimate, model2$Estimate,alternative = "greater")
    correlations[nrow(correlations) + 1,] = list(model1_chr, model2_chr,
                                                 cor, cor_test$p.value)

    print(paste("Testing correlation for models:", model1_chr,
                "and", model2_chr))

    print(paste("Correlation value:", cor))
  }
}

print(paste("Mean correlation across all models:", mean(correlations$Correlation)))
print(paste("SD of correlation across all models:", sd(correlations$Correlation)))

print(paste("Mean p value across all models:", mean(correlations$p_value)))
print(paste("SD of p value across all models:", sd(correlations$p_value)))

print(paste("Combination with minimum correlation:") )
print(tibble(correlations[which.min(correlations$Correlation),]))

# Realised that the combination rows, and def any squared rows are no longer standardised.
# so maybe i have to add them in earlier and standardise them then ?


# AIC Plot ----------------------------------------------------------------

# Reading file
model_rankings <- read_csv("outputs/02_regression/data/model_ranking_gradated.csv",
                           skip = 1)[-1]

# Cleaning names
model_rankings$`Neighbor Matrix`  = sub("A[.]","", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("lag","Lag ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix`  = sub("distance","Distance ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("nearest","Nearest ", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("km"," km", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("_shared_edge"," Adjacency", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("queen","Queen", model_rankings$`Neighbor Matrix`)
model_rankings$`Neighbor Matrix` = sub("rook","Rook", model_rankings$`Neighbor Matrix`)
model_rankings$Formula = sub("model_","Model ", model_rankings$Formula)


# Ordering Combinations
model_rankings$`Neighbor Matrix` <- factor(model_rankings$`Neighbor Matrix`,
                                           levels=
                                             c("Queen Adjacency", "Rook Adjacency",
                                               "Lag 2", "Lag 4",  "Nearest 4",
                                               "Distance 0.5 km", "Distance 1 km",
                                               "Distance 1.5 km" ))
model_rankings$Formula<- factor(model_rankings$Formula,
                                levels=c("Model 1","Model 2","Model 3","Model 4",
                                         "Model 5","Model 6","Model 7","Model 8",
                                         "Model 9","Model 10"))


# Finding minimum AIC
minimum <- model_rankings[which.min(model_rankings$AIC),]

# Plotting heatmap
ggplot(model_rankings, aes(`Neighbor Matrix`, `Formula`, fill= AIC)) +
  geom_tile() +
  geom_label(data = minimum, fill= "white", alpha=0.7,
             size = 2.5, aes(label = "min AIC")) +
  scale_fill_continuous(name = "AIC",
                        palette = "viridis") +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Neighbourhood Matrix") +
  ylab("Regression Model")



