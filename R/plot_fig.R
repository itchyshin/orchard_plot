
p1.1 <- orchaRd::orchard_plot(english_MR_int, xlab = "Hedges' d") + 
	guides(fill = "none", colour = "none") +  
  	theme(legend.position= c(0.0, 0.9999), legend.justification = c(0,1)) +
  	theme(legend.direction="horizontal", legend.title = element_text(size =7),legend.text = element_text(size = 7))+
  	scale_x_continuous(expand = c(0.2,0.2))


p1 <- orchaRd::orchard_plot(english_MR, mod="ManipType", xlab = "Hedges' d") + 
	guides(fill = "none", colour = "none") +  
  	theme(legend.position= c(0.0, 0.9999), legend.justification = c(0,1)) +
  	theme(legend.direction="horizontal", legend.title = element_text(size =7),legend.text = element_text(size = 7))+
  	scale_x_continuous(expand = c(0.2,0.2))


p2 <- orchard_plot(senior_MR, mod = "ManipType", xlab = "log Coefficient of Variation (ln CVR)", alpha = 0.8) + 
	guides(fill = "none", colour = "none") +  
  	theme(legend.position= c(0.05, 0.99), legend.justification = c(0,1), legend.key.size = unit(1, "mm")) +
  	theme(legend.direction="vertical", legend.title = element_text(size =8),legend.text = element_text(size = 8)) +
  	scale_x_continuous(expand = c(0.2,0.2))

p3 <- orchard_plot(eklof_MR, mod = "Grazer.type", xlab = "log Response Ratio (lnRR)") + 
	guides(fill = "none", colour = "none") +  
  	theme(legend.position= c(0.05, 0.99), legend.justification = c(0,1), legend.key.size = unit(1, "mm")) +
  	theme(legend.direction="horizontal", legend.title = element_text(size =8),legend.text = element_text(size = 8)) + 
scale_x_continuous(expand = c(0.2,0.2))

p4 <- orchard_plot(lim_MR, mod = "Phylum", xlab = "Correlation (r)", alpha = 0.60) + 
  	guides(fill = "none", colour = "none") +  
  	theme(legend.position= c(0.05, 0.99), legend.justification = c(0,1), legend.key.size = unit(1, "mm")) +
  	theme(legend.direction="horizontal", legend.title = element_text(size =8),legend.text = element_text(size = 8)) + 
  	scale_x_continuous(expand = c(0.2,0.2))


library(patchwork)



##
pdf(file = "figure1.pdf", width = 11.5, height =  14)
path1 <- (p1 + p1.1)
path2 <- (p1 / p2 / p3) | p4
path1 / path2  + plot_layout(heights = c(1,3.5))
dev.off()
