#' plot_descriptive_fill_ldn
#'
#' @param relevant_col - a string of the relevant column to fill based on
#' @param title title for the plot
#' @param legend_title title for the legend
#' @param map map containing the data to use
#'
#' @returns N/A - plots a map of London filled based on the relevant column
#' @export 
#'
#' @examples
#' 
#' plot_descriptive_ldn(relevant_col = 'avg_hos_dpr', 
#' title = "Household Deprivation by LSOA", 
#' legend_title = "Average Number of \ n Deprivation Dimensions", map = LSOA_map)
#' 
#' plot_descriptive_ldn(relevant_col = 'WD_pop_den', title = "Workday Population Density by LSOA", 
#' legend_title = "Residents per sq km", map = LSOA_map)
#' 
#' 
plot_descriptive_ldn <- function(relevant_col, 
                                 title, legend_title, 
                                 map){
  ggplot() +
    geom_sf(data = map, lwd=0, 
            aes(fill = .data[[relevant_col]])) + 
    scale_fill_continuous(name = legend_title, 
                          labels = scales::label_number(), 
                          palette = "viridis") +
    ggtitle(label = title) +
    xlab("Longitude") +
    ylab("Latitude") 
}