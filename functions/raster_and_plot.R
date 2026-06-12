#' Raster and Plot
#'
#' @param column the column you are wanting to plot 
#' @param title title for the produced plot
#' @param fun function used to interpolate over the plots 
#' @param map map used, defaults to LSOA_map
#' @param boxmeters number of meters you want each grid square to be
#'
#' @returns returns the data used to plot the diagram, and produces a plot 
#' @export
#'
#' @examples
#' raster_and_plot(LSOA_map$cnt_AED, title = "Average AED count")
#' raster_and_plot(LSOA_map$cnt_AED, title = "Average AED count", boxmeters = NA)
#' 
raster_and_plot <- function(column,title, fun=mean, map=LSOA_map, boxmeters=200){
  #install.packages("roxygen2")
  # cmd + option + shift + “r” or crtl + option + shift + “r”.
  r <- raster()
  crs(r) = crs(map)
  extent(r) <- extent(map) 
  if (is.na(boxmeters)){
    res(rast) <- 0.005
  }
  else{
    ncol(r) <- 50 * 1000 / boxmeters
    nrow(r) <- ncol(r)
  }
  
  rast_data <- rasterize(map, r, column, fun)
  plot(rast_data, main=title)
  return (rast_data)
}