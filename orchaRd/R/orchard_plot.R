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


get_K <- function(data, mod){
 	data.frame(data %>% dplyr::group_by( {{mod}}) %>% dplyr::summarise(K = n()))$K
}

#' @title orchard_plot
#' @description Using a metafor model object of class rma or rma.mv or a results table of class orchard, it creates a an orchard plot from mean effect size estimates for all levels of a given categorical moderator, their corresponding confidence intervals and prediction intervals
#' @param object object of class 'rma.mv', 'rma' or 'orchard '
#' @param mod the name of a moderator 
#' @param data the data used to fit the rma.mv or rma model
#' @param es_type the type of effect size used in the model, z-transformed Persons correlation coefficient (Zr) or standardised mean difference (i.e., Hedges/Cohen's d, g) (d)
#' @return Orchard plot
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
#' orchard_plot(results, data = eklof, mod = Grazer.type, es_type = "Zr")
#' # or
#' orchard_plot(eklof_MR, data = eklof, mod = Grazer.type, es_type = "Zr")
#' 
#' # Example 2
#' mod_est <- mod_results(lim_MR, "Phylum")
#' orchard_plot(mod_est, data = dat.lim2014.1, mod = "Phylum", es_type = "Zr")
#' orchard_plot(lim_MR, data = dat.lim2014.1, mod = "Phylum", es_type = "Zr")
#' }
#' @export


orchard_plot <- function(object, data, mod, es_type = c("d", "Zr")) {

	if(any(class(object) %in% c("rma.mv", "rma"))){
		object <- mod_results(object, mod)
	}

	if(es_type == "Zr"){

		cols <- sapply(object, is.numeric)
		object[,cols] <- Zr_to_r(object[,cols])
		data$yi <- Zr_to_r(data$yi)
		label <- "Correlation (r)"
		lim = c(-1.1,1.1)

	}else{
		label <- "Hedge's d"
		lim = c(min(data$yi), max(data$yi))
	}

	 object$K <- as.vector(by(data, data[,mod], function(x) length(x[,"yi"])))

	# Make the orchard plot
	  ggplot2::ggplot(data = object, aes(x = estimate, y = name)) +
		ggplot2::scale_x_continuous(limits=lim) +
	  	ggbeeswarm::geom_quasirandom(data = data[complete.cases(data[,mod]),], 
	                   aes(x = yi, y = data[,mod], size = (N), colour = data[,mod]), groupOnX = FALSE, alpha=0.2) + 
	  	
	  	# 95 %prediction interval (PI)
	  	ggplot2::geom_errorbarh(aes(xmin = object$lowerPR, xmax = object$upperPR),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
	  	# 95 %CI
	  	ggplot2::geom_errorbarh(aes(xmin = object$lowerCL, xmax = object$upperCL),  height = 0, show.legend = F, size = 1.2) +
	  	ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
	  	# creating dots and different size (bee-swarm and bubbles)
	  	ggplot2::geom_point(aes(fill = object$name), size = 3, shape = 21) + #
	  	# setting colours
	  	ggplot2::annotate('text', x = 0.93, y = (seq(1, dim(object)[1], 1)+0.3), label= paste("italic(k)==", object$K), parse = TRUE, hjust = "left", size = 3.5) +
	  	ggplot2::labs(x = label, y = "", tag = "") +
	  	ggplot2::guides(fill = "none", colour = "none") +
	  	ggplot2::theme_bw() +
	  	ggplot2::theme(legend.position="none") +
	  	ggplot2::theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))

}






###########################################
plot_orchard <- function(object, mod, lim, data){
	ggplot2::ggplot(data = object, aes(x = estimate, y = name)) +
		ggplot2::scale_x_continuous(limits=lim) +
	  	ggbeeswarm::geom_quasirandom(data = data %>% filter(!is.na({{mod}})), 
	                   aes(x = yi, y = {{mod}}, size = (N), colour = {{mod}}), groupOnX = FALSE, alpha=0.2) + 
	  	# 95 %precition interval (PI)
	  	ggplot2::geom_errorbarh(aes(xmin = object$lowerPR, xmax = object$upperPR),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
	  	# 95 %CI
	  	ggplot2::geom_errorbarh(aes(xmin = object$lowerCL, xmax = object$upperCL),  height = 0, show.legend = F, size = 1.2) +
	  	ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
	  	# creating dots and different size (bee-swarm and bubbles)
	  	ggplot2::geom_point(aes(fill = object$name), size = 3, shape = 21) + #
	  	# setting colours
	  	ggplot2::annotate('text', x = 0.93, y = (seq(1, dim(object)[1], 1)+0.3), label= paste("italic(k)==", object$K), parse = TRUE, hjust = "left", size = 3.5) +
	  	ggplot2::labs(x = label, y = "", tag = "") +
	  	ggplot2::guides(fill = "none", colour = "none") +
	  	ggplot2::theme_bw() +
	  	ggplot2::theme(legend.position="none") +
	  	ggplot2::theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))
}

plot_orchard(lim_MR, Phylum)





############################
dat_test <- data.frame(group = rep(c("yes", "no"), each =10), y = rnorm(20,0,1))

# Normally
dat_test %>% group_by(group) %>% summarise(n = n())

fun <- function(data, mod){
	mod <- rlang::enquo(mod)
	tmp <- data %>% group_by(!!mod) %>% summarise(n = length(y))
	return(tmp)
}

fun(dat_test, group) ## Doesn't work

fun_2 <- function(data, mod){
	tmp <- data %>% group_by({{mod}}) %>% summarise(n = length(y))
	return(tmp)
}

fun_2(dat_test, group) ## Doesn't work