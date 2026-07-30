
# Analysing Regression Results --------------------------------------------



# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------

library(readr)
library(stringr) #to split strings
library(ggplot2) # for plotting
library(dplyr) # for select

## Functions ---------------------------------------------------------------

read_coef_csv = function(filename){
  print(paste("---- Starting Import of", filename, "  ----"))
  
  import <- read_csv(filename, col_types = cols())
  import <- import[-1]
  
  model_specs <- do.call(rbind,str_split(import$model,"_A."))
  import$variable_combination  <- model_specs[,1]
  import$distance_matrix  <- model_specs[,2]
  
  ifelse(exists("total_coef_matrix"), 
         total_coef_matrix <<- rbind(total_coef_matrix, import),
         assign("total_coef_matrix", import, envir = .GlobalEnv))

  print(paste("---- Completed Import of", filename, "  ----"))
}

plot_coef_estimates = function(dataframe){
  dataframe %>% 
    arrange(length(as.character(dataframe$clean_var_name))) %>%
    mutate(clean_var_name = factor(clean_var_name, levels = unique(clean_var_name))) %>%
    ggplot() +
    geom_point(aes(clean_var_name, Estimate, 
                   size = .data[["Pr(>|z|)"]], colour=distance_matrix), 
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

## Files -------------------------------------------------------------------

# Reading in Coefficients -------------------------------------------------

# Importing saved coefficients
coef_files <- list.files(path="regression_results/to_import", pattern="*.csv", 
                         full.names=TRUE, recursive=FALSE)

for(file in coef_files){
  read_coef_csv(file)
}

# Check import finished correctly
print(length(coef_files) == length(unique(total_coef_matrix$model)))

# Cleaning variable names
total_coef_matrix$clean_var_name = gsub("LDN_grid_map[$]", "", total_coef_matrix$variable)
total_coef_matrix$clean_var_name = gsub("[.]scaled", "", total_coef_matrix$clean_var_name)

total_coef_matrix$clean_var_name = gsub("[(]Intercept[)]","Intercept", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("pc_f","Female Proportion", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("pc_50_p","50+ Proportion", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("pc_bd_g","Bad General Health Proportion", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("avg_dpr","Average Household Deprivation", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("pop_den","Population Density", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("WD_pp_d","Workday Population Density", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("cnt_spr","Number of Sports Sites", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("cnt_CHs","Number of Care Facilities", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("pc_65_p","65+ Proportion", total_coef_matrix$clean_var_name)

total_coef_matrix$clean_var_name = gsub("Proportion","%", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("Average","Avg.", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("Household","Hos.", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("Population Density","Pop. Den.", total_coef_matrix$clean_var_name)
total_coef_matrix$clean_var_name = gsub("Number of","#", total_coef_matrix$clean_var_name)

total_coef_matrix$clean_var_name = gsub(":"," x ", total_coef_matrix$clean_var_name)


# Plotting Regression Coefficients ----------------------------------------

# Organizing according to variable name length
total_coef_matrix = total_coef_matrix %>% 
  arrange(length(as.character(total_coef_matrix$clean_var_name))) %>%
  mutate(clean_var_name = factor(clean_var_name, levels = unique(clean_var_name)))

larger_coefs = unique(total_coef_matrix$clean_var_name)[1:8]

larger_coef_matrix = total_coef_matrix[total_coef_matrix$clean_var_name %in% larger_coefs,]
smaller_coef_matrix = total_coef_matrix[!total_coef_matrix$clean_var_name %in% larger_coefs,]

# Plotting results
plot_coef_estimates(larger_coef_matrix)
plot_coef_estimates(smaller_coef_matrix)
plot_coef_estimates(total_coef_matrix)


# Calculating Correlation -------------------------------------------------

by_model_coefs = total_coef_matrix %>% group_by(model)
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
    
    model1 = model1[model1$clean_var_name %in% model2$clean_var_name,]
    model2 = model2[model2$clean_var_name %in% model1$clean_var_name,]
    
    model1 = model1 %>%  arrange(clean_var_name)
    model2 = model2 %>%  arrange(clean_var_name)
    
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

    