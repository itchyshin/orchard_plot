# Orchard plot

## B

pdf("./fig/B2.pdf", height=3.5, width=6)

orchard_plot(model_results, mod="Int", xlab = "Standardised mean difference") +
  scale_fill_manual(values="grey") +
  scale_colour_manual(values="grey")  
dev.off()  


## C
pdf("./fig/C2.pdf", height=3.5, width=6)

orchard_plot(english_MR, mod="ManipType", xlab = "Standardised mean difference")
  
dev.off()  

## D
pdf("./fig/D2.pdf", height=3.5, width=6)

orchard_plot(senior_MR, mod = "ManipType", xlab = "log(CV ratio) (lnCVR)")  +
  scale_fill_manual(values=  c("#F0E442", "#0072B2")) +
  scale_colour_manual(values= c("#F0E442", "#0072B2"))
  
dev.off()  

# E
pdf("./fig/E2.pdf", height=3.5, width=6)

orchard_plot(eklof_MR, mod = "Grazer.type", xlab = "log(Response ratio) (lnRR)") +
  scale_fill_manual(values=  c("#D55E00", "#CC79A7")) +
  scale_colour_manual(values= c("#D55E00", "#CC79A7"))
  
dev.off()  

# F
pdf("./fig/F2.pdf", height=10.5, width=6)

orchard_plot(lim_MR, mod = "Phylum", xlab = "Correlaiton coefficent", transfm = "tanh")
  
dev.off()  

# Caterpillars plot
# Orchard plot

## B

pdf("./fig/B3.pdf", height=5, width=6)

caterpillars(model_results, mod="Int", xlab = "Standardised mean difference")
dev.off()  


## C
pdf("./fig/C3.pdf", height=5, width=6)

caterpillars(senior_MR, mod="ManipType", xlab = "log(CV ratio) (lnCVR)")

dev.off()  

## D
pdf("./fig/D3.pdf", height=15, width=6)

caterpillars(lim_MR, mod = "Phylum", xlab = "Fisher's Zr")#paste("Fisher's", "italic(Zr)"))  

dev.off()  

## D
pdf("./fig/D4.pdf", height=15, width=6)

caterpillars(lim_MR, mod = "Phylum", xlab = "Correlaiton coefficent", transfm = "tanh") #paste("Fisher's", "italic(Zr)"))  

dev.off()  