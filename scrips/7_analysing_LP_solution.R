# Set Up ------------------------------------------------------------------

## Packages ----------------------------------------------------------------



## Functions ---------------------------------------------------------------
solution_to_plot = function(facility_solution, facility_object, map, demand=TRUE,
                            num_bins=6, max_relevant_val=3, legend_title="demand"){
  chosen_1facilities = facility_object$geometry[facility_solution==1]
  chosen_2facilities = facility_object$geometry[facility_solution==2]
  chosen_3facilities = facility_object$geometry[facility_solution==3]
  if (demand==TRUE){
    ggplot() + 
      geom_sf(data = ldn_boundary_map) +
      geom_sf(data = map, lwd=0.0001, 
              aes(fill = F1_scld)) +
      scale_fill_steps(breaks = seq(0, max_relevant_val, length = num_bins),
                       limit = c(0,10000),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)), 
                       name = legend_title) + 
      geom_sf(data=chosen_1facilities,
              size = 0.0001,alpha = 0.5,
              colour="yellow") +
      geom_sf(data=chosen_2facilities,
              size = 0.0001,alpha = 0.5,
              colour="orange") +
      geom_sf(data=chosen_3facilities,
              size = 0.0001,alpha = 0.5,
              colour="red") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(ldn_boundary_map))
  }
  else{
    ggplot() + 
      geom_sf(data = ldn_boundary_map) +
      geom_sf(data=chosen_1facilities,
              size = 0.0001,alpha = 0.5,
              colour="yellow") +
      geom_sf(data=chosen_2facilities,
              size = 0.0001,alpha = 0.5,
              colour="orange") +
      geom_sf(data=chosen_3facilities,
              size = 0.0001,alpha = 0.5,
              colour="red") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_sf(crs = st_crs(ldn_boundary_map))
  }
  
}

process_lp_result = function(lp_sol, map, print_sol_vec=FALSE){
  print(lp_sol)
  solution_vector <- lp_sol$solution
  if(print_sol_vec == TRUE){print(cat("Solution vector ", solution_vector))}
  
  facility_solution <- solution_vector[1:Nlocations]
  
  initial_demand <- map$F1_mutated
  remaining_demand <- solution_vector[(Nlocations + 1):(2 *Nlocations)]
  
  pc_demand_covered = (sum(initial_demand) - sum(remaining_demand)) / sum(initial_demand) * 100
  print(paste("% Demand covered: ", pc_demand_covered))
  
  cat("Facility solution:", facility_solution)
  return(facility_solution)
}

sf_to_latlong_matix <- function(sf_object){
  result = as.data.frame(st_coordinates(sf_object))
  names(result)= c("long","lat")
  return(result)
}
