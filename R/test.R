# key webpage
# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/



# test

install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
install.packages("patchwork")
devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE,
                         build_vignettes = TRUE)
library(orchaRd)
library(patchwork) 
library(tidyverse) 
library(metafor)

english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC,
                  n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("SMD", "vSMD"), data = english)
english_MA_int <- rma.mv(yi = SMD, V = vSMD, random = list(~1 | StudyNo, ~1 | EffectID), data = english)
summary(english_MA_int)


english <- escalc(measure = "CVR", n1i = NStartControl, sd1i = SD_C, m1i = MeanC, n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("lnCVR", "vlnCVR"), data = english)
# Now we can fit the meta-regression model (contrast)
english_MR <- rma.mv(yi = SMD, V = vSMD, mods = ~ManipType - 1, random = list(~1 |StudyNo, ~1 | EffectID), data = english) 
summary(english_MR)

res2 <- mod_results(english_MR, mod = "ManipType")
print(res2)

senior_MR <- rma.mv(yi = lnCVR, V = vlnCVR, mods = ~ManipType - 1, random = list(~1 | StudyNo, ~1 | EffectID), data = english)
summary(senior_MR)

res3 <- mod_results(senior_MR, "ManipType") 
print(res3)


#########
install.packages("metaviz")
library(metaviz)
viz_forest()

##############
p = ggplot(data=RR_data,
           aes(x = Group,y = RiskRatio, ymin = LowerLimit, ymax = UpperLimit ))+
  geom_pointrange(aes(col=Group))+
  geom_hline(aes(fill=Group),yintercept =1, linetype=2)+
  xlab('Group')+ ylab("Risk Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=LowerLimit, ymax=UpperLimit,col=Group),width=0.5,cex=1)+ 
  facet_wrap(~Condition,strip.position="left",nrow=9,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
p


############

#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p <- ggplot(dat, aes(y=cite, x=yi, xmin=lowerci, xmax=upperci, shape = tester))+
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-2,2), name='Standardized Mean Difference (d)')+
  #Give y-axis a meaningful label
  ylab('Reference')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(setting~., scales= 'free', space='free') #+
  #Apply my APA theme
  #apatheme
p
###########
library(metafor)
data(lim)
# Add in the sampling variance
lim$vi <- 1/(lim$N - 3)
# Let's fit a meta-regression. The phylogenetic model found phylogenetic effects, # however, instead we could fit Phylum as a fixed effect and explore them with an # Orchard Plot
lim_MR <- rma.mv(yi = yi, V = vi, mods = ~Phylum - 1, random = list(~1 | Article, ~1 | Datapoint), data = lim) 
summary(lim_MR)

res4 <- mod_results(lim_MR, "Phylum") 
print(res4)
