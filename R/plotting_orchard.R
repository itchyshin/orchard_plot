# B

pdf("./fig/B.pdf", height=3.5, width=6)

orchard_plot(model_results, mod="Int", xlab = "Standardised mean difference") +
  scale_fill_manual(values="grey") +
  scale_colour_manual(values="grey")  
dev.off()  


# C
pdf("./fig/C.pdf", height=3.5, width=6)

orchard_plot(english_MR, mod="ManipType", xlab = "Standardised mean difference")
  
dev.off()  

# D
pdf("./fig/D.pdf", height=3.5, width=6)

orchard_plot(senior_MR, mod = "ManipType", xlab = "log(CV ratio) (lnCVR)")  +
  scale_fill_manual(values=  c("#F0E442", "#0072B2")) +
  scale_colour_manual(values= c("#F0E442", "#0072B2"))
  
dev.off()  

# E
pdf("./fig/E.pdf", height=3.5, width=6)

orchard_plot(eklof_MR, mod = "Grazer.type", xlab = "log(Response ratio) (lnRR)") +
  scale_fill_manual(values=  c("#D55E00", "#CC79A7")) +
  scale_colour_manual(values= c("#D55E00", "#CC79A7"))
  
dev.off()  

# F

pdf("./fig/F.pdf", height=10.5, width=6)

	orchard_plot(lim_MR, mod = "Phylum", xlab = "Correlaiton coefficent", es_type = "Zr")
  
dev.off()  