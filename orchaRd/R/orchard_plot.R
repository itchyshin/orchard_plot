#' @title Zr_to_r
#' @description Converts Zr back to r (Pearson's correlation coefficient)
#' @param df data frame of results of class 'orchard'
#' @return A data frame containing all the model results including mean effect size estimate, confidence and prediction intervals with estimates converted back to r
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au 
#' @export

Zr_to_r <- function(df){
	return(sapply(df, tanh))
}

#' @title orchard_plot
#' @description Using a metafor model object of class rma or rma.mv or a results table of class orchard, it creates a an orchard plot from mean effect size estimates for all levels of a given categorical moderator, their corresponding confidence intervals and prediction intervals
#' @param object object of class 'rma.mv', 'rma' or 'orchard '
#' @param mod the name of a moderator 
#' @param N  The vector of sample size
#' @param es_type the type of effect size used in the model, z-transformed Persons correlation coefficient (Zr) or standardised mean difference (i.e., Hedges/Cohen's d, g) (d)
#' @return Orchard plot
#' @authors Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @authors Daniel Noble - daniel.noble@anu.edu.au
#' @examples
#' \dontrun{
#' data(eklof) 
#' eklof<-metafor::escalc(measure="ROM", n1i=N_control, sd1i=SD_control,
#' m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment,
#' data=eklof)
#' # Add the unit level predictor
#' eklof$Datapoint<-as.factor(seq(1, dim(eklof)[1], 1))
#' # fit a MLMR - accouting for some non-independence
#' eklof_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1, random=list(~1|ExptID,
#' ~1|Datapoint), data=eklof)
#' results <- mod_results(eklof_MR, mod = "Grazer.type")
#' orchard_plot(results, data = eklof, mod = Grazer.type, es_type = "Zr")
#' # or
#' orchard_plot(eklof_MR, data = eklof, mod = Grazer.type, es_type = "Zr")
#' 
#' # Example 2
#' data(lim)
#' lim$vi<-(1/sqrt(lim$N - 3))^2
#' lim_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~Phylum-1, random=list(~1|Article,
#' ~1|Datapoint), data=lim)
#' orchard_plot(lim_MR, data = lim, mod = "Phylum", es_type = "Zr")
#' }
#' @export

orchard_plot <- function(object, mod, es_type = c("d", "Zr", "lnRR", "lnCVR"), alpha = 0.8, N = NULL) {

	if(any(class(object) %in% c("rma.mv", "rma"))){
		object <- mod_results(object, mod)
	}

	      data <- object$data
	data$scale <- (1/sqrt(data[,"vi"]))

	if(es_type == "Zr"){

		cols <- sapply(object$mod_table, is.numeric)
		object$mod_table[,cols] <- Zr_to_r(object$mod_table[,cols])
		data$yi <- Zr_to_r(data$yi)
		label <- "Correlation (r)"
		#lim = c(-1.2,1.2)

	}else{
		label <- es_type
		#lim = c(min(data$yi)-(min(data$yi)*0.1), max(data$yi)+(max(data$yi)*0.5))
	}

	 object$mod_table$K <- as.vector(by(data, data[,"moderator"], function(x) length(x[,"yi"])))

	# Make the orchard plot
	  plot <- ggplot2::ggplot(data = object$mod_table, aes(x = estimate, y = name)) +
	
	  	ggbeeswarm::geom_quasirandom(data = data, aes(x = yi, y = moderator, size = scale, colour = moderator), groupOnX = FALSE, alpha=alpha) + 
	  	#ggplot2::scale_x_continuous(limits=lim) + 
	  	# 95 %prediction interval (PI)
	  	ggplot2::geom_errorbarh(aes(xmin = object$mod_table$lowerPR, xmax = object$mod_table$upperPR),  height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
	  	# 95 %CI
	  	ggplot2::geom_errorbarh(aes(xmin = object$mod_table$lowerCL, xmax = object$mod_table$upperCL),  height = 0, show.legend = FALSE, size = 1.2) +
	  	ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = alpha) +
	  	# creating dots and different size (bee-swarm and bubbles)
	  	ggplot2::geom_point(aes(fill = object$mod_table$name), size = 3, shape = 21) + #
	  	# setting colours
	  	ggplot2::annotate('text', x = 0.93, y = (seq(1, dim(object$mod_table)[1], 1)+0.3), label= paste("italic(k)==", object$mod_table$K), parse = TRUE, hjust = "left", size = 3.5) +
	  	ggplot2::theme_bw() +
	  	
	  	if(N == NULL){
	  		ggplot2::labs(x = label, y = "", size = "Inverse of Sampling Variance")
	  	}else{
	  		ggplot2::labs(x = label, y = "", size = expression(paste(italic(N), "(# of effects)")) )
	  	} +

  		ggplot2::guides(fill = "none", colour = "none") +  theme(legend.position= c(0.1, 0.98), legend.justification = c(0,1)) +
  		ggplot2::theme(legend.direction="horizontal")

	  return(plot)

}


#orchard_plot(eklof_MR, data = dat.eklof2012, mod = "Grazer.type", es_type = "lnRR", alpha = 0.8)


 #colour_ls <- c("#000000", "#E69F00", "#56B4E9", "#009E73",  "#F0E422",  "#0072B2",  "#D55E00", "#CC79A7", "#00008B", "#8B0A50", "#54FF9F", "#999999")
	 #cols <- sample(colour_ls, unique(data_comlte[,mod]))