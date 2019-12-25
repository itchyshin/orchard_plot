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
#' @param model model object of class 'rma.mv', 'rma' or 'orchard '
#' @param mod the name of a moderator . Otherwise, "Int" for intercept only model.
#' @param xlab The effect size measure label.
#' @param N  The vector of sample size which an effect size is based on. If defult, we use precision (the inverse of sampling standard error)
#' @param alpha The level of transparency for pieces of frust (effec size)
#' @param angle The angle of y labels. The defult is 90 degreee
#' @param cb If TRUE, it uses 8 colour blind friendly colors (7 colours plus grey)
#' @param es_type If set to "Zr", Zr will be converted to correlation
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

orchard_plot <- function(object, mod = "Int", xlab, N = "none", alpha = 0.5, angle = 90, cb = TRUE, es_type = c("else", "Zr")) {

	if(any(class(object) %in% c("rma.mv", "rma"))){
		if(mod != "Int"){
			object <- mod_results(object, mod)
		} else{
			object <- mod_results(object, mod = "Int")
		}
	}
	        data <- object$data
	  data$scale <- (1/sqrt(data[,"vi"]))
	      legend <- "Precision"

	if(N != "none" & is.numeric(N)){
		  data$scale <- N
		      legend <- "Sample Size (N)"
	}

	if(es_type == "Zr"){

		cols <- sapply(object$mod_table, is.numeric)
		object$mod_table[,cols] <- Zr_to_r(object$mod_table[,cols])
		data$yi <- Zr_to_r(data$yi)
		  label <- xlab

	}else{
		label <- xlab
	}

	 object$mod_table$K <- as.vector(by(data, data[,"moderator"], function(x) length(x[,"yi"])))
	 
	# colour blind friendly colours with grey
	 cbpl <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

	# Make the orchard plot
	  plot <- ggplot2::ggplot(data = object$mod_table, aes(x = estimate, y = name)) +
	    # pieces of fruit (bee-swarm and bubbles)
	  	ggbeeswarm::geom_quasirandom(data = data, aes(x = yi, y = moderator, size = scale, colour = moderator), groupOnX = FALSE, alpha=alpha) + 
	  	# 95 %prediction interval (PI): twigs
	  	ggplot2::geom_errorbarh(aes(xmin = object$mod_table$lowerPR, xmax = object$mod_table$upperPR),  height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
	  	# 95 %CI: branches
	  	ggplot2::geom_errorbarh(aes(xmin = object$mod_table$lowerCL, xmax = object$mod_table$upperCL),  height = 0, show.legend = FALSE, size = 1.2) +
	  	ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = alpha) +
	  	# creating dots for truncks
	  	ggplot2::geom_point(aes(fill = object$mod_table$name), size = 3, shape = 21) + 
	  	# setting colours
	  	ggplot2::annotate('text', x = (max(data$yi) + (max(data$yi)*0.10)), y = (seq(1, dim(object$mod_table)[1], 1)+0.3), label= paste("italic(k)==", object$mod_table$K), parse = TRUE, hjust = "right", size = 3.5) +
	  	ggplot2::theme_bw() +
	    ggplot2::guides(fill = "none", colour = "none") + 
	    ggplot2::theme(legend.position= c(1, 0), legend.justification = c(1, 0)) +
	    ggplot2::theme(legend.title = element_text(size = 9)) +
	    ggplot2::theme(legend.direction="horizontal") +
	    ggplot2::theme(legend.background = element_blank()) +
	  	ggplot2::labs(x = label, y = "", size = legend) +
	    ggplot2::theme(axis.text.y = element_text(size = 10, colour ="black", 
	                                              hjust = 0.5, 
	                                              angle = angle))
	  # putting colours in
	  if(cb == TRUE){
	    data$scale <- N
	    plot <- plot + 
	      scale_fill_manual(values=cbpl) +
	      scale_colour_manual(values=cbpl)
	  }

	  return(plot)
}
