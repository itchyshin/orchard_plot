
# Functions taken from Shins phylogeny MS

#' @title get_est
#' @description Function gets estimates from rma objects (metafor)
#' @param model rma.mv object 
#' @param mod the name of a moderator 
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @export

get_est <- function (model, mod) {
      name <- as.factor(stringr::str_replace(row.names(model$beta), {{mod}}, ""))
  estimate <- as.numeric(model$beta)
   lowerCL <- model$ci.lb
   upperCL <- model$ci.ub 
  
  table <- tibble::tibble(name = name, estimate = estimate, lowerCL = lowerCL, upperCL = upperCL)

  return(table)
}

#get_est(lim_MR, "Phylum")

#' @title get_pred
#' @description Function to get prediction intervals (crediblity intervals) from rma objects (metafor)
#' @param model rma.mv object 
#' @param mod the name of a moderator 
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @export

get_pred <- function (model, mod) {

  name <- as.factor(stringr::str_replace(row.names(model$beta), {{mod}}, ""))
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


#get_pred(lim_MR, "Phylum")

#Here are links for how to do confidence regions for rma.mv regression lines
#https://www.rdocumentation.org/packages/metafor/versions/1.9-9/topics/predict.rma
#https://stackoverflow.com/questions/50804464/out-of-sample-prediction-for-rma-object-in-metafor


#' @title cont_gen
#' @description Creates all combinations of moderator level names
#' @param name a vector of character strings
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a character string with all combinations of the moderator level names
#' @export
#' 
cont_gen <- function (name) {
  combination <- utils::combn(name,2)
     name_dat <- t(combination)
        names <- paste(name_dat[ ,1], name_dat[, 2], sep = "-")
  return(names)
}


#' @title mod_results
#' @description Using a metafor model object of class rma or rma.mv it creates a table of model results containing the mean effect size estimates for all levels of a given categorical moderator, their corresponding confidence intervals and prediction intervals
#' @param model rma.mv object 
#' @param mod the name of a moderator 
#' @return A data frame containing all the model results including mean effect size estimate, confidence and prediction intervals
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @examples
#' \dontrun{data(eklof) 
#' eklof<-metafor::escalc(measure="ROM", n1i=N_control, sd1i=SD_control,
#' m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment,
#' data=eklof)
#' # Add the unit level predictor
#' eklof$Datapoint<-as.factor(seq(1, dim(eklof)[1], 1))
#' # fit a MLMR - accouting for some non-independence
#' eklof_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1, random=list(~1|ExptID,
#' ~1|Datapoint), data=eklof)
#' results <- mod_results(eklof_MR, mod = "Grazer.type")
#' }
#' @export
#' 
mod_results <- function(model, mod) { 

	if(all(class(model) %in% c("rma.mv", "rma")) == FALSE) {stop("Sorry, you need to fit a metafor model of class rma.mv or rma")}

	# Get confidence intervals
	CI <- get_est(model, mod)

	# Get prediction intervals
	PI <- get_pred(model, mod)

	model_results <- cbind(CI, PI[,-1])

	class(model_results) <- c("data.frame", "orchard")

	return(model_results)

}









