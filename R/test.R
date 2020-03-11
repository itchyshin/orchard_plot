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
