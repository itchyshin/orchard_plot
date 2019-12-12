
# Functions taken from Shins phylogeny MS

#' @title: get_est
#' @description Function gets estimates from rma objects (metafor)
#' @param model: rma.mv object 
#' @param mod: the name of a moderator 
#' @export

get_est <- function (model, mod = " ", name) {
  
	# Check model class
	if(all(class(model) %in% c("rma.mv", "rma")) == FALSE) {stop("Sorry, you need to fit a metafor model of class rma.mv or rma")}

      name <- as.factor(stringr::str_replace(row.names(model$beta), mod, ""))
  estimate <- as.numeric(model$beta)
   lowerCL <- model$ci.lb
   upperCL <- model$ci.ub 
  
  table <- tibble::tibble(name = name, estimate = estimate, lowerCL = lowerCL, upperCL = upperCL)

  return(table)
}


#' @title Function to get prediction intervals (crediblity intervals) from rma objects (metafor)
#' @param model: rma.mv object 
#' @param mod: the name of a moderator 
#' @export

get_pred <- function (model, mod = " ", name) {

	# Check model class
	if(all(class(model) %in% c("rma.mv", "rma")) == FALSE) {stop("Sorry, you need to fit a metafor model of class rma.mv or rma")}


  name <- as.factor(stringr::str_replace(row.names(model$beta), mod, ""))
  len <- length(name)
  
  if(len != 1){
  newdata <- matrix(NA, ncol = len, nrow = len)
  for(i in 1:len) {
    # getting the position of unique case from X (design matrix)
    pos <- which(model$X[,i] == 1)[[1]]
    newdata[, i] <- model$X[pos,]
    }
  pred <- metafor::predict.rma(model, newmods = newdata)
  }
  else {
    pred <- metafor::predict.rma(model)
    }
  lowerPR <- pred$cr.lb
  upperPR <- pred$cr.ub 
  
  table <- tibble::tibble(name = name, lowerPR = lowerPR, upperPR = upperPR)
  return(table)


}

#Here are links for how to do confidence regions for rma.mv regression lines
#https://www.rdocumentation.org/packages/metafor/versions/1.9-9/topics/predict.rma
#https://stackoverflow.com/questions/50804464/out-of-sample-prediction-for-rma-object-in-metafor


#' @title Contrast name geneator
#' @description Creates all combinations of names
#' @param name: a vector of character strings
cont_gen <- function (name) {
  combination <- combn(name,2)
     name_dat <- t(combination)
        names <- paste(name_dat[ ,1], name_dat[, 2], sep = "-")
  return(names)
}


mod_results <- function(model, mod = "")

	name <- as.factor(stringr::str_replace(row.names(model$beta), mod, ""))
