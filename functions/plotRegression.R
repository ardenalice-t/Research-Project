
#' Plotting regression plots
#'
#' @param relevant_col a string of the relevant column to fill based on
#' @param title title for the plot
#' @param max_relevant_val if you want bins with an open top bin, 
#' what is the maximum divider value you would like, if null then does a typical plot
#' @param num_bins number of bins of colour for fill
#' @param legend_title legend title, assumed to be number of AEDs
#' @param map the map used, assumed to be LSOA_map
#'
#' @returns N/A - plots a map of London filled based on the relevant column
#' @export
#'
#' @examples
#' plotRegresssion("F1", "AEDs using CAR model (n=1)")
#' plotRegresssion("F1", "AEDs using CAR model (n=1)", max_relevant_val = 30)
#' 
#' 
plotRegresssion <- function(relevant_col, title, 
                            max_relevant_val = NULL, num_bins = 6,
                            legend_title = "Number of AEDs", map = LSOA_map){
  if(is.null(max_relevant_val)){
    plot(map[relevant_col], main=title, lwd=0.001)}
  else{
    ggplot() +
      geom_sf(data = map, lwd=0.001, 
              aes(fill = .data[[relevant_col]])) +
      scale_fill_steps(breaks = seq(0, max_relevant_val, length = num_bins),
                       limit = c(0,10000),
                       na.value = "light blue",
                       rescaler = ~ scales::rescale_max(.x, from =c(0,max_relevant_val)), 
                       name = legend_title) + 
      ggtitle(label = title)}
}