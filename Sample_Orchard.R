
# Clean up
rm(list=ls())

# Load packages
library(metafor)
library(ggplot2)
library(stringr)
library(tidyverse)
library(plyr)
library(ggbeeswarm)

# Functions taken from Shins phylogeny MS

#' Title: the function to get estimates from rma objects (metafor)
#'
#' @param model: rma.mv object 
#' @param mod: the name of a moderator 
get_est <- function (model, mod = " ") {
  
  name <- as.factor(str_replace(row.names(model$beta), mod, ""))
  estimate <- as.numeric(model$beta)
  lowerCL <- model$ci.lb
  upperCL <- model$ci.ub 
  
  table <- tibble(name = name, estimate = estimate, lowerCL = lowerCL, upperCL = upperCL)
}


#' Title: the function to get prediction intervals (crediblity intervals) from rma objects (metafor)
#'
#' @param model: rma.mv object 
#' @param mod: the name of a moderator 
get_pred <- function (model, mod = " ") {
  name <- as.factor(str_replace(row.names(model$beta), mod, ""))
  len <- length(name)
  
  if(len != 1){
  newdata <- matrix(NA, ncol = len, nrow = len)
  for(i in 1:len) {
    # getting the position of unique case from X (design matrix)
    pos <- which(model$X[,i] == 1)[[1]]
    newdata[, i] <- model$X[pos,]
    }
  pred <- predict.rma(model, newmods = newdata)
  }
  else {
    pred <- predict.rma(model)
    }
  lowerPR <- pred$cr.lb
  upperPR <- pred$cr.ub 
  
  table <- tibble(name = name, lowerPR = lowerPR, upperPR = upperPR)
}

#Here are links for how to do confidence regions for rma.mv regression lines
#https://www.rdocumentation.org/packages/metafor/versions/1.9-9/topics/predict.rma
#https://stackoverflow.com/questions/50804464/out-of-sample-prediction-for-rma-object-in-metafor


#' Title: Contrast name geneator
#'
#' @param name: a vector of character strings
cont_gen <- function (name) {
  combination <- combn(name,2)
  name_dat <- t(combination)
  names <- paste(name_dat[ ,1], name_dat[, 2], sep = "-")
  return(names)
}

################### Example: Zr effect size

load("dat.lim2014.1.rda")
head(dat.lim2014.1)

# The dataset comes from Lim et al. 2014. Evolution
# The effect size (column yi) is the correlation (Zr) between maternal size and offspring/propagule size, N is the number of observations (could vary from mothers to offspring depending on the paper)

# There are many different species with different life histories
# Lim et al found the correlation is positive and significant and relatively consistent across different types of species (they used phylogenetic meta-analysis/regression)

# We need to calculate the sampling varaince
dat.lim2014.1$vi<-(1/sqrt(dat.lim2014.1$N - 3))^2

# Lets sfit a meta-regression - I will do Article non-independence. The phylogenetic model found ohylogenetic effects - instead we could fit Phylum as a fixed effect and explore them with an Orchard Plot
lim_MR<-rma.mv(yi=yi, V=vi, mods=~Phylum-1, random=list(~1|Article, ~1|Datapoint), data=dat.lim2014.1)
summary(lim_MR)

# creating a table of results
pred<-get_pred(lim_MR, mod="Phylum") 
effect<-get_est(lim_MR)
res<-cbind(effect, pred[,-1])
K<-ddply(dat.lim2014.1, .(Phylum), summarise, length(yi))
res$name<-K[,1]
res$K<-K[,2]

# Create a pdf
pdf("Zr.pdf", height=7, width=7)

# # Make the orchard plot
ggplot(data = res, aes(x = tanh(estimate), y = name)) +
	scale_x_continuous(limits=c(-1, 1), breaks = seq(-1, 1, by = 0.2) ) +
  	geom_quasirandom(data = dat.lim2014.1 %>% filter(!is.na(Phylum)), 
                   aes(x = tanh(yi), y = Phylum, size = (N), colour = Phylum), groupOnX = FALSE, alpha=0.2) + 
  	# 95 %precition interval (PI)
  	geom_errorbarh(aes(xmin = tanh(lowerPR), xmax = tanh(upperPR)),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
  	# 95 %CI
  	geom_errorbarh(aes(xmin = tanh(lowerCL), xmax = tanh(upperCL)),  height = 0, show.legend = F, size = 1.2) +
  	geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
  	# creating dots and different size (bee-swarm and bubbles)
  	geom_point(aes(fill = name), size = 3, shape = 21) + #
  	# setting colours
  	annotate('text', x = 0.93, y = (seq(1, dim(res)[1], 1)+0.3), label= paste("italic(k)==", res$K), parse = TRUE, hjust = "left", size = 3.5) +
  	labs(x = "Correlation", y = "", tag = " ") +
  	guides(fill = "none", colour = "none") +
  	theme_bw() +
  	theme(legend.position="none") +
  	theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))
  
dev.off()  
   
# ################### Example: d and lnCVR effect sizes

load("dat.english2016.2.rda")
head(dat.english2016.2)

# The dataset comes from English and Uller 2016. Biology Letters, and re-analysed in Senior et al. 2017. Biology Letters for lnCVR
# The data are on the effects of dietary restriction on age at death - the effect sizes used were d and lnCVR, which need to be calculated

# DR does not substantially alter the mean (d), regardless of type of restriction (quantity of quality), but seems to alter the variance (lnCVR)

# Lets start with d

# We need to calculate the effect sizes
dat.english2016.2<-escalc(measure="SMD", n1i=NStartControl, sd1i=SD_C, m1i=MeanC, n2i=NStartExpt, sd2i=SD_E, m2i=MeanE, data=dat.english2016.2)


# Lets fit a meta-regression - I am modelling non-independence here (article).
english_MR<-rma.mv(yi=yi, V=vi, mods=~ManipType-1, random=list(~1|EffectID), data=dat.english2016.2)
summary(english_MR)

# creating a table of results
pred<-get_pred(english_MR, mod="ManipType") 
effect<-get_est(english_MR)
res<-cbind(effect, pred[,-1])
K<-ddply(dat.english2016.2, .(ManipType), summarise, length(yi))
res$name<-K[,1]
res$K<-K[,2]

# Open a pdf
pdf("d.pdf", height=4, width=7)

# # Make the orchard plot
ggplot(data = res, aes(x = estimate, y = name)) +
	scale_x_continuous(limits=c(-2, 2), breaks = seq(-2, 2, by = 0.5)) +
  	geom_quasirandom(data = dat.english2016.2 %>% filter(!is.na(ManipType)), 
                   aes(x = yi, y = ManipType, size = (1/sqrt(vi)), colour = ManipType), groupOnX = FALSE, alpha=0.2) + 
  	# 95 %precition interval (PI)
  	geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
  	# 95 %CI
  	geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL),  height = 0, show.legend = F, size = 1.2) +
  	geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
  	# creating dots and different size (bee-swarm and bubbles)
  	geom_point(aes(fill = name), size = 3, shape = 21) + #
  	# setting colours
  	annotate('text', x = 1.8, y = (seq(1, dim(res)[1], 1)+0.3), label= paste("italic(k)==", res$K), parse = TRUE, hjust = "left", size = 3.5) +
  	labs(x = "Standardised Mean Difference", y = "", tag = " ") +
  	guides(fill = "none", colour = "none") +
  	theme_bw() +
  	theme(legend.position="none") +
  	theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))

dev.off()  

# Now lets compare to lnCVR as in Senior et al. 2017

# We need to calculate the effect sizes
dat.english2016.2<-escalc(measure="CVR", n1i=NStartControl, sd1i=SD_C, m1i=MeanC, n2i=NStartExpt, sd2i=SD_E, m2i=MeanE, data=dat.english2016.2)


# Lets fit a meta-regression - I am modelling non-independence here (article).
senior_MR<-rma.mv(yi=yi, V=vi, mods=~ManipType-1, random=list(~1|EffectID), data=dat.english2016.2)
summary(senior_MR)

# creating a table of results
pred<-get_pred(senior_MR, mod="ManipType") 
effect<-get_est(senior_MR)
res<-cbind(effect, pred[,-1])
K<-ddply(dat.english2016.2, .(ManipType), summarise, length(yi))
res$name<-K[,1]
res$K<-K[,2]

# Open a pdf
pdf("lnCVR.pdf", height=4, width=7)

# # Make the orchard plot
ggplot(data = res, aes(x = estimate, y = name)) +
	scale_x_continuous(limits=c(-2, 2), breaks = seq(-2, 2, by = 0.5)) +
  	geom_quasirandom(data = dat.english2016.2 %>% filter(!is.na(ManipType)), 
                   aes(x = yi, y = ManipType, size = (1/sqrt(vi)), colour = ManipType), groupOnX = FALSE, alpha=0.2) + 
  	# 95 %precition interval (PI)
  	geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
  	# 95 %CI
  	geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL),  height = 0, show.legend = F, size = 1.2) +
  	geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
  	# creating dots and different size (bee-swarm and bubbles)
  	geom_point(aes(fill = name), size = 3, shape = 21) + #
  	# setting colours
  	annotate('text', x = 1.8, y = (seq(1, dim(res)[1], 1)+0.3), label= paste("italic(k)==", res$K), parse = TRUE, hjust = "left", size = 3.5) +
  	labs(x = "Log Coefficient of Variation Ratio", y = "", tag = " ") +
  	guides(fill = "none", colour = "none") +
  	theme_bw() +
  	theme(legend.position="none") +
  	theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))
  
dev.off()

# ################### Example: lnRR effect size

dat.eklof2012<-read.csv("Eklof-2012-Experimental climate.csv")
head(dat.eklof2012)

# The lnRR example comes from Eklof et al 2012. Ecology Letters
# It is on the effects of predation (present vs absent) on invertebrate communities - the outcome is biomass/abundance - the effect is moderated by whether the invertebrate is an amphipod or gastropod

# Calculate the effect size
dat.eklof2012<-escalc(measure="ROM", n1i=N_control, sd1i=SD_control, m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment, data=dat.eklof2012)

# Add the unit level predictor
dat.eklof2012$Datapoint<-as.factor(seq(1, dim(dat.eklof2012)[1], 1))

# fit a MLMR - accouting for some non-independence
eklof_MR<-rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1, random=list(~1|ExptID, ~1|Datapoint), data=dat.eklof2012)
summary(eklof_MR)


# creating a table of results
pred<-get_pred(eklof_MR, mod="Grazer.type") 
effect<-get_est(eklof_MR)
res<-cbind(effect, pred[,-1])
K<-ddply(dat.eklof2012, .(Grazer.type), summarise, length(yi))
res$name<-K[,1]
res$K<-K[,2]

# Open a pdf
pdf("lnRR.pdf", height=4, width=7)

# # Make the orchard plot
ggplot(data = res, aes(x = estimate, y = name)) +
	scale_x_continuous(limits=c(-3.5, 3.5), breaks = seq(-3.5, 3.5, by = 0.5)) +
  	geom_quasirandom(data = dat.eklof2012 %>% filter(!is.na(Grazer.type)), 
                   aes(x = yi, y = Grazer.type, size = (1/sqrt(vi)), colour = Grazer.type), groupOnX = FALSE, alpha=0.2) + 
  	# 95 %precition interval (PI)
  	geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR),  height = 0, show.legend = F, size = 0.5, alpha = 0.6) +
  	# 95 %CI
  	geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL),  height = 0, show.legend = F, size = 1.2) +
  	geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.3) +
  	# creating dots and different size (bee-swarm and bubbles)
  	geom_point(aes(fill = name), size = 3, shape = 21) + #
  	# setting colours
  	annotate('text', x = 1.8, y = (seq(1, dim(res)[1], 1)+0.3), label= paste("italic(k)==", res$K), parse = TRUE, hjust = "left", size = 3.5) +
  	labs(x = "Log Response Ratio", y = "", tag = " ") +
  	guides(fill = "none", colour = "none") +
  	theme_bw() +
  	theme(legend.position="none") +
  	theme(axis.text.y = element_text(size = 10, colour ="black",hjust = 0.5, angle = 45))
  
dev.off()

