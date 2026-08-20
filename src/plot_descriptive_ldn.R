#' plot_descriptive_fill_ldn
#'
#' @param relevant_col - a string of the relevant column to fill based on
#' @param legend_title title for the legend
#' @param map map containing the data to use
#'
#' @returns N/A - plots a map of London filled based on the relevant column
#' @export
#'
#' @examples
#'
#' plot_descriptive_ldn(relevant_col = 'avg_hos_dpr',
#' legend_title = "Average Number of \ n Deprivation Dimensions", map = LSOA_map)
#'
#' plot_descriptive_ldn(relevant_col = 'WD_pop_den',
#' legend_title = "Residents per sq km", map = LSOA_map)
#'
#'
plot_descriptive_ldn <- function(relevant_col, legend_title,
                                 map, cap=FALSE, max_val=5, bins=6){
  stopifnot(is.character(relevant_col) &
              is.character(legend_title))

  if(cap){
    stopifnot(is.numeric(max_val) & is.numeric(bins))
    final_plot <- ggplot() +
      geom_sf(data = map, lwd=0,
              aes(fill = .data[[relevant_col]])) +
      scale_fill_steps(breaks = seq(min(map[[relevant_col]]), max_val, length = bins),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_val)),
                       labels = scales::label_number(0.1),
                       name = legend_title)  +
      xlab("Longitude") +
      ylab("Latitude")
  }
  if(!cap){
    final_plot <- ggplot() +
      geom_sf(data = map, lwd=0,
              aes(fill = .data[[relevant_col]])) +
      scale_fill_continuous(name = legend_title,
                            labels = scales::label_number(),
                            palette = "viridis") +
      xlab("Longitude") +
      ylab("Latitude")
  }
  final_plot
}
