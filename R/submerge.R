
# TODO - when moderator names the same it does not work well

#' @title submerge
#' @description Merge two model results tables (orchard objects).
#' @param object1  object of class orchard
#' @param object2  object of class orchard
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a data frame
#' @export
#' 
submerge <- function(object1, object2, ...){
  orchard_list <- list(object1, object2, ...)
  
  # merging tables
  tables <- lapply(orchard_list, function(x) x$mod_table)
  tables <- do.call("rbind", tables)
  # merging data
  datas <- lapply(orchard_list, function(x) x$data)
  datas <- do.call("rbind", datas)
  
  model_results <- list(mod_table = tables, data = datas)
  
  class(model_results) <- "orchard"
  
  return(model_results)
  
}
