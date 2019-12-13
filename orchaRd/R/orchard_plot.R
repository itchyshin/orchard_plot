

Zr_to_r <- function(df){
	return(sapply(df, tanh))
}




orchard_plot(object, data, mod, es_type = c("d", "Zr"), ...) {

	if(any(class(object) %in% c("rma.mv", "rma"))){
		object <- mod_results(object, mod = mod)
	}

	if(match.arg(es_type) == "Zr"){

		cols <- sapply(object, is.numeric)
		object[,cols] <- Zr_to_r(object[,cols])
		data$yi <- Zr_to_r(data$yi)
		label <- "Correlation (r)"
		lim = c(-1,1)

	} else{
		label <- "Hedge's d"
		lim = c(min(data$yi), max(data$yi))
	}

	data[,mod] <- as.factor(data[,mod])
	  object$K <- as.vector(by(data, data[,mod], function(x) length(x[,"yi"])))

	# Make the orchard plot
	ggplot2::ggplot(data = object, aes(x = estimate, y = name)) +
		ggplot2::scale_x_continuous(limits=lim) +
	  	geom_quasirandom(data = data[!is.na(mod),], 
	                   aes(x = yi, y = {{mod}}, size = (N), colour = {{mod}}), groupOnX = FALSE, alpha=0.2) + 
	  	# 95 %precition interval (PI)
	  	ggplot2::geom_errorbarh(aes(xmin = object$lowerPR), xmax = object$upperPR)),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
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

test <- function(object, mod){
	mod <- rlang::enquo(mod)
	ggplot2::ggplot(data = object, aes(x = estimate, y = name)) +
			ggplot2::scale_x_continuous(limits=lim) +
		  	geom_quasirandom(data = data %>% filter(!is.na(!! mod)),  
		                   aes(x = yi, y =!! mod, size = (N), colour =!! mod), groupOnX = FALSE, alpha=0.2) 
}

test(data, "Phylum")

fun <- function(data, mod){
	mod <- rlang::enquo(mod)
	tmp <- data %>% group_by(!!mod) %>% summarise(n = length(yi))
	return(tmp)
}

fun(data, "Phylum")
