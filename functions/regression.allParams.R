
#' Performing a CAR model regression on all relevant parameters of LSOA_map
#'
#' @param neighbour_matrix the A matrix of neighbours, symmetric
#' @param map the LSOA_map
#'
#' @returns the regression output
#' @export
#'
#' @examples
#' A <- nb2listw(poly2nb(LSOA_map), style="B")
#' car.out <- regression.all_params(A)
#' 
regression.all_params <- function(neighbour_matrix, map = LSOA_map){
  stopifnot(class(neighbour_matrix) %in% c("matrix", "listw", "nb"))
  
  car.out <- spautolm(formula = map$cnt_AED ~
                        map$f_pr + 
                        map$ovr_50_ + 
                        map$bd_gh_p +
                        map$workday_population+ 
                        map$population + 
                        map$hos_dpr, 
                      data = map, listw=neighbour_matrix, family="CAR")
  return (car.out)
}
