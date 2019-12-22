
# Clean up
rm(list=ls())

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd")
# Load packages
pacman::p_load(metafor, ggplot2, stringr, tidyverse, plyr, ggbeeswarm, patchwork)


################### Example: Zr effect size

load("./data/lim.rda")
head(dat.lim2014.1)

# The dataset comes from Lim et al. 2014. Evolution
# The effect size (column yi) is the correlation (Zr) between maternal size and offspring/propagule size, N is the number of observations (could vary from mothers to offspring depending on the paper)

# There are many different species with different life histories
# Lim et al found the correlation is positive and significant and relatively consistent across different types of species (they used phylogenetic meta-analysis/regression)

# We need to calculate the sampling varaince
dat.lim2014.1$vi<-(1/sqrt(dat.lim2014.1$N - 3))^2

# Lets sfit a meta-regression - I will do Article non-independence. The phylogenetic model found ohylogenetic effects - instead we could fit Phylum as a fixed effect and explore them with an Orchard Plot
lim_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~Phylum-1, random=list(~1|Article, ~1|Datapoint), data=dat.lim2014.1)
summary(lim_MR)

# Get the results
lim_results <- mod_results(lim_MR, "Phylum")

# Create a pdf of the orchard pplot
pdf("Zr.pdf", height=7, width=7)

  orchard_plot(lim_MR, data = dat.lim2014.1, mod = "Phylum", es_type = "Zr", alpha = 0.8)
  
dev.off()  
   
## Could also feed in the results table

pdf("Zr.pdf", height=7, width=7)

  orchard_plot(lim_results, data = dat.lim2014.1, mod = "Phylum", es_type = "Zr", alpha = 0.9)
  
dev.off()  

# ################### Example: d and lnCVR effect sizes

load("./data/english.rda")
head(dat.english2016.2)

# The dataset comes from English and Uller 2016. Biology Letters, and re-analysed in Senior et al. 2017. Biology Letters for lnCVR
# The data are on the effects of dietary restriction on age at death - the effect sizes used were d and lnCVR, which need to be calculated

# DR does not substantially alter the mean (d), regardless of type of restriction (quantity of quality), but seems to alter the variance (lnCVR)

# Lets start with d

# We need to calculate the effect sizes
english<-metafor::escalc(measure="SMD", n1i=NStartControl, sd1i=SD_C, m1i=MeanC, n2i=NStartExpt, sd2i=SD_E, m2i=MeanE, data=english)

# Need to add N in for the scaling of each effect size
english$N <- rowSums(english[,c("NStartControl","NStartExpt")])

# Lets fit a meta-regression - I am modelling non-independence here (article).
english_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ManipType-1, random=list(~1|EffectID), data=english)
summary(english_MR)

# creating a table of results
english_results <- mod_results(english_MR, "ManipType")
print(english_results)

# Open a pdf
pdf("d.pdf", height=4, width=7)
  
  orchard_plot(english_MR, mod = "ManipType", es_type = "Hedge's d", alpha = 0.7)

dev.off()  

# Alternatively with results
  orchard_plot(english_results, mod = "ManipType", es_type = "lnRR", alpha = 0.2)

# Now lets compare to lnCVR as in Senior et al. 2017

# We need to calculate the effect sizes
english<-metafor::escalc(measure="CVR", n1i=NStartControl, sd1i=SD_C, m1i=MeanC, n2i=NStartExpt, sd2i=SD_E, m2i=MeanE, data=english)


# Lets fit a meta-regression - I am modelling non-independence here (article).
senior_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ManipType-1, random=list(~1|EffectID), data=english)
summary(senior_MR)

# creating a table of results
senior_results <- mod_results(senior_MR, "ManipType")
print(senior_results)

# Open a pdf
pdf("lnCVR.pdf", height=4, width=7)
  
  #p1 <- orchard_plot(english_results, data = english, mod = "ManipType", es_type = "lnRR", alpha = 0.2)
  orchard_plot(senior_MR, mod = "ManipType", es_type = "log Coefficient of Variation (ln CVR)", alpha = 0.8)

  #p1+p2

dev.off()

# ################### Example: lnRR effect size

load("./data/eklof.rda")
head(dat.eklof2012)

# The lnRR example comes from Eklof et al 2012. Ecology Letters
# It is on the effects of predation (present vs absent) on invertebrate communities - the outcome is biomass/abundance - the effect is moderated by whether the invertebrate is an amphipod or gastropod

# Calculate the effect size
dat.eklof2012<-escalc(measure="ROM", n1i=N_control, sd1i=SD_control, m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment, data=dat.eklof2012)

# Add the unit level predictor
dat.eklof2012$Datapoint<-as.factor(seq(1, dim(dat.eklof2012)[1], 1))

dat.eklof2012$N <- rowSums(dat.eklof2012[,c("N_control", "N_treatment")])

# fit a MLMR - accouting for some non-independence
eklof_MR<-rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1, random=list(~1|ExptID, ~1|Datapoint), data=dat.eklof2012)
summary(eklof_MR)

# creating a table of results

eklof_results <- mod_results(eklof_MR, "Grazer.type")

# Open a pdf
pdf("lnRR.pdf", height=4, width=7)

  orchard_plot(eklof_results, data = dat.eklof2012, mod = "Grazer.type", es_type = "lnRR")
  
dev.off()

