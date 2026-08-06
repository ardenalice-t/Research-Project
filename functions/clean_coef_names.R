#' Clean Coefficient Names
#'
#' @param column - column of a dataframe containing all the coefficient names 
#' to clean
#' @param pc_symbol - whether or not 'proportion' should be displayed as %
#' @param abbreviations - whether or not additional abbreviations should be 
#' applied e.g. average to avg.
#'
#' @returns the column with clean names
#' @export
#'
#' @examples
#' rownames(coef_cor) <- clean_coef_names(rownames(coef_cor))
clean_coef_names <- function(column, pc_symbol=FALSE, abbreviations=FALSE){
  stopifnot(is.logical(pc_symbol)) 
  stopifnot(is.logical(abbreviations))
    stopifnot(is.character(column))
  
  # general symbols
  column = gsub("LDN_grid[$]", "", column)
  column = gsub("[.]scaled", "", column)
  column = gsub(":"," x ", column)
  
  # coefficient names
  column = gsub("[(]Intercept[)]","Intercept", column)
  column = gsub("pc_f","Female Proportion", column)
  column = gsub("pc_50_plus","50+ Proportion", column)
  column = gsub("pc_65_plus","65+ Proportion", column)
  column = gsub("pc_bad_gh","Bad General Health Proportion", column)
  column = gsub("avg_hos_dpr","Average Household Deprivation", column)
  column = gsub("WD_pop_den","Workday Population Density", column)
  column = gsub("pop_den","Population Density", column)
  column = gsub("count_sports","Number of Sports Sites", column)
  column = gsub("count_CHs","Number of Care Facilities", column)
  column = gsub("count_AEDss","Number of AEDs", column)
 
  # options
  if(pc_symbol) column = gsub("Proportion","%", column)
  if(abbreviations){
    column = gsub("Average","Avg.", column)
    column = gsub("Household","Hos.", column)
    column = gsub("Population Density","Pop. Den.", column)
    column = gsub("Number of","#", column)
  }
  
  return(column)
}