# key webpage
# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

# test

# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("metafor")
# install.packages("patchwork")
# install.packages("R.rsp")

#devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

library(orchaRd)
library(patchwork)
library(tidyverse)
library(metafor)

english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC,
                  n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("SMD", "vSMD"), data = english)
english_MA_int <- rma.mv(yi = SMD, V = vSMD, random = list(~1 | StudyNo, ~1 | EffectID), data = english)
summary(english_MA_int)


english <- escalc(measure = "CVR", n1i = NStartControl, sd1i = SD_C, m1i = MeanC, n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("lnCVR", "vlnCVR"), data = english)


english_MA <- rma.mv(yi = SMD, V = vSMD, random = list(~1 |StudyNo, ~1 | EffectID), data = english) 
summary(english_MR)

res1 <- mod_results(english_MA, mod = "Int")
print(res1)

# Now we can fit the meta-regression model (contrast)
english_MR <- rma.mv(yi = SMD, V = vSMD, mods = ~ManipType - 1, random = list(~1 |StudyNo, ~1 | EffectID), data = english) 
summary(english_MR)

res2 <- mod_results(english_MR, mod = "ManipType")
print(res2)

res12<-submerge(res1, res2)
print(res12)


senior_MR <- rma.mv(yi = lnCVR, V = vlnCVR, mods = ~ManipType - 1, random = list(~1 | StudyNo, ~1 | EffectID), data = english)
summary(senior_MR)

res3 <- mod_results(senior_MR, "ManipType") 
print(res3) 





data(lim)
# Add in the sampling variance
lim$vi <- 1/(lim$N - 3)
# Let's fit a meta-regression. The phylogenetic model found phylogenetic effects,
# however, instead we could fit Phylum as a fixed effect and explore them with an
# Orchard Plot
lim_MR <- rma.mv(yi = yi, V = vi, mods = ~Phylum - 1, random = list(~1 | Article,
                                                                    ~1 | Datapoint), data = lim)
summary(lim_MR)

res4 <- mod_results(lim_MR, "Phylum") 
print(res4)

######################### TODO 
################################
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

#######

##


####### forest plot
#'Internal helper function of viz_forest to create a classic forest plot

#'

#'Creates a classic forest plot. Called by viz_forest for type = "classic"

#'@keywords internal

internal_viz_classicforest <- function(plotdata, madata,
                                       
                                       type = "standard",
                                       
                                       study_labels = NULL, summary_label = NULL,
                                       
                                       study_table = NULL, summary_table = NULL, annotate_CI = FALSE,
                                       
                                       confidence_level = 0.95, col = "Blues", summary_col = "Blues", tick_col = "firebrick",
                                       
                                       text_size = 3, xlab = "Effect", x_limit = NULL,
                                       
                                       x_trans_function = NULL, x_breaks = NULL) {
  
  n <- nrow(plotdata)
  
  k <- length(levels(plotdata$group))
  
  
  
  # weight of each study used to scale the height of each raindrop
  
  if(type %in% c("standard", "study_only")) {
    
    weight <- 1/(plotdata$se^2 + madata$summary_tau2[as.numeric(plotdata$group)])
    
  } else {
    
    weight <- 1/plotdata$se^2
    
  }
  
  plotdata$rel_weight <- weight/sum(weight)
  
  
  
  if(type %in% c("cumulative", "sensitivity")) {
    
    tick_size <- max(plotdata$rel_weight/(6*max(plotdata$rel_weight)))
    
    tickdata <- data.frame(x = c(plotdata$x, plotdata$x), ID = c(plotdata$ID, plotdata$ID),
                           
                           y = c(plotdata$ID + tick_size,
                                 
                                 plotdata$ID - tick_size))
    
  }
  
  
  
  
  
  # set limits and breaks for the y axis and construct summary diamond (for type standard and sensitivity)
  
  if(type %in% c("standard", "sensitivity", "cumulative")) {
    
    y_limit <- c(min(plotdata$ID) - 3, max(plotdata$ID) + 1.5)
    
    y_tick_names <- c(as.vector(study_labels), as.vector(summary_label))[order(c(plotdata$ID, madata$ID), decreasing = T)]
    
    y_breaks <- sort(c(plotdata$ID, madata$ID), decreasing = T)
    
    summarydata <- data.frame("x.diamond" = c(madata$summary_es - stats::qnorm(1 - (1 - confidence_level) / 2, 0, 1) * madata$summary_se,
                                              
                                              madata$summary_es,
                                              
                                              madata$summary_es + stats::qnorm(1 - (1 - confidence_level) / 2, 0, 1) * madata$summary_se,
                                              
                                              madata$summary_es),
                              
                              "y.diamond" = c(madata$ID,
                                              
                                              madata$ID + 0.3,
                                              
                                              madata$ID,
                                              
                                              madata$ID - 0.3),
                              
                              "diamond_group" = rep(1:k, times = 4)
                              
    )
    
  } else {
    
    y_limit <- c(min(plotdata$ID) - 1, max(plotdata$ID) + 1.5)
    
    y_tick_names <- plotdata$labels[order(plotdata$ID, decreasing = T)]
    
    y_breaks <- sort(plotdata$ID, decreasing = T)
    
  }
  
  
  
  # set limits for the x axis if none are supplied
  
  if(is.null(x_limit)) {
    
    x_limit <- c(range(c(plotdata$x_min, plotdata$x_max))[1] - diff(range(c(plotdata$x_min, plotdata$x_max)))*0.05,
                 
                 range(c(plotdata$x_min, plotdata$x_max))[2] + diff(range(c(plotdata$x_min, plotdata$x_max)))*0.05)
    
  }
  
  
  
  # Set Color palette for shading
  if(type != "summary_only") {
    
    if(all(col %in% c("Blues", "Greys", "Oranges", "Greens", "Reds", "Purples"))) {
      
      col <- unlist(lapply(col, function(x) RColorBrewer::brewer.pal(n = 9, name = x)[9]))
      
    }
    
  }
  
  if(type != "study_only") {
    
    if(all(summary_col %in% c("Blues", "Greys", "Oranges", "Greens", "Reds", "Purples"))) {
      
      summary_col <- unlist(lapply(summary_col, function(x) RColorBrewer::brewer.pal(n = 9, name = x)[9]))
      
    }
    
    if(type == "summary_only") {
      
      col <- summary_col
      
    } else {
      
      if(length(summary_col) > 1) summary_col <- rep(summary_col, times = 4)
      
    }
    
  }
  
  
  
  
  
  # Set plot margins. If table is aligned on the left, no y axus breaks and ticks are plotted
  
  l <- 5.5
  
  r <- 11
  
  if(annotate_CI == TRUE) {
    
    r <- 1
    
  }
  
  if(!is.null(study_table) || !is.null(summary_table)) {
    
    l <- 1
    
    y_tick_names <- NULL
    
    y_breaks <- NULL
    
  }
  
  # workaround for "Undefined global functions or variables" Note in R CMD check while using ggplot2.
  
  x.diamond <- NULL
  
  y.diamond <- NULL
  
  diamond_group <- NULL
  
  ID <- NULL
  
  x <- NULL
  
  y <- NULL
  
  x_min <- NULL
  
  x_max <- NULL
  
  
  
  # create classic forest plot
  
  p <-
    
    ggplot(data = plotdata, aes(y = ID, x = x)) +
    
    geom_vline(xintercept = 0, linetype = 2) +
    
    geom_errorbarh(data = plotdata, col = "black", aes(xmin = x_min, xmax = x_max, y = ID, height = 0))
  
  
  
  if(type %in% c("cumulative", "sensitivity")) {
    
    p <- p + geom_line(data = tickdata, aes(x = x, y = y, group = ID), col = col, size = 1)
    
  } else {
    
    p <- p + geom_point(aes(size = weight), shape = 22, col = "black", fill = col)
    
  }
  
  
  
  if(type %in% c("standard", "sensitivity", "cumulative")) {
    
    p <- p + geom_polygon(data = summarydata, aes(x = x.diamond, y = y.diamond, group = diamond_group), color= "black", fill = summary_col, size = 0.1)
    
  }
  
  p <- p +
    
    scale_y_continuous(name = "",
                       
                       breaks = y_breaks,
                       
                       labels = y_tick_names) +
    
    coord_cartesian(xlim = x_limit, ylim = y_limit, expand = F)
  
  if(!is.null(x_trans_function)) {
    
    if(is.null(x_breaks)) {
      
      p <- p +
        
        scale_x_continuous(name = xlab,
                           
                           labels = function(x) {round(x_trans_function(x), 3)})
      
    } else {
      
      p <- p +
        
        scale_x_continuous(name = xlab,
                           
                           labels = function(x) {round(x_trans_function(x), 3)},
                           
                           breaks = x_breaks)
      
    }
    
  } else {
    
    if(is.null(x_breaks)) {
      
      p <- p +
        
        scale_x_continuous(name = xlab)
      
    } else {
      
      p <- p +
        
        scale_x_continuous(breaks = x_breaks,
                           
                           name = xlab)
      
    }
    
  }
  
  p <- p +
    
    scale_size_area(max_size = 3) +
    
    theme_bw() +
    
    theme(text = element_text(size = 1/0.352777778*text_size),
          
          legend.position = "none",
          
          panel.grid.major.y = element_blank(),
          
          panel.grid.minor.y = element_blank(),
          
          panel.grid.major.x = element_line("grey"),
          
          panel.grid.minor.x = element_line("grey"),
          
          plot.margin = margin(t = 5.5, r = r, b = 5.5, l = l, unit = "pt"))
  
  p
}
#######

