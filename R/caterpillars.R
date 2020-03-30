# to work on

#' @title caterpillars
#' @description Using a metafor model object of class rma or rma.mv or a results table of class orchard, it creates a an orchard plot from mean effect size estimates for all levels of a given categorical moderator, their corresponding confidence intervals and prediction intervals
#' @param object model object of class 'rma.mv', 'rma' or 'orchard' table of model results
#' @param mod the name of a moderator. Otherwise, "Int" for intercept only model.
#' @param xlab The effect size measure label.
#' @param angle The angle of y labels. The default is 90 degrees
#' @param cb If TRUE, it uses 8 colour blind friendly colors (7 colours plus grey)
#' @param transfm If set to "tanh", a tanh transformation will be applied to effect sizes, converting Zr will to a correlation or pulling in extreme values for other effect sizes (lnRR, lnCVR, SMD). If "none" is chosen then it will default to 
#' @return Orchard plot
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @examples
#' @export

caterpillars <- function(object, mod = "Int", xlab,  angle = 90, transfm = c("none", "tanh")) {
  
  if(any(class(object) %in% c("rma.mv", "rma"))){
    if(mod != "Int"){
      object <- mod_results(object, mod)
    } else{
      object <- mod_results(object, mod = "Int")
    }
  }
  data <- object$data
  data$lower <- data$yi - stats::qnorm(0.975)*sqrt(data$vi)
  data$upper <- data$yi + stats::qnorm(0.975)*sqrt(data$vi)
  data$moderator <- factor(data$moderator, labels = res4$mod_table$name)
  
  
  # data frame for the meta-analytic results
  mod_table <- object$mod_table
  mod_table$K <- as.vector(by(data, data[,"moderator"], function(x) length(x[,"yi"])))
  mod_table$moderator <- mod_table$name
  # the number of groups in a moderator
  group_no <- nrow(mod_table)
  data_no <- nrow(data)
  #groups <- 
  
  # use dplyr here - need to change.... 
  # Dan can you make this basic R code - maybe I got it
  # data <- data[order(data$moderator, -data$yi),]
  data <- data %>% group_by(moderator) %>% arrange(moderator, desc(yi)) %>%  
    ungroup() %>% 
    mutate(Y = 1:data_no + 
             unlist(mapply(function(x, y) rep(x*6 , y) , x = 1:group_no, y = mod_table$K))
    ) %>% 
    data.frame()
  
  # mod ID
  mod_table$Y <- data %>% group_by(moderator) %>% 
    summarise(Y = first(Y)) %>% 
    select(Y) %>% t() %>% as.vector() -2
  
  # preparing for diamons for summary - we need to prep y axis 
  # copying from internal_viz_classicforest() from R package 
  sum_data <- data.frame("x.diamond" = c(mod_table$lowerCL,
                                         mod_table$estimate ,
                                         mod_table$upperCL,
                                         mod_table$estimate ),
                         "y.diamond" = c(mod_table$Y,
                                         mod_table$Y + 1.2,
                                         mod_table$Y,
                                         mod_table$Y - 1.2),
                         "moderator" = rep(mod_table$name, times = 4)
  )
  
  # Make - 
  plot <- ggplot2::ggplot(data = data, aes(x = yi, y = Y)) +
    # pieces of fruit (bee-swarm and bubbles)
    
    # 95 % CI
    ggplot2::geom_errorbarh(aes(xmin = lower, xmax = upper), 
                            colour = "#00CD00", height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
    # 95 %CI: branches
    
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
    # creating dots for truncks
    ggplot2::geom_point(colour = "#FFD700", size = 1) +
    # creating 95% prediction intervals
    ggplot2::geom_segment(data = mod_table, aes(x = lowerPR, y = Y, xend = upperPR, yend = Y, group = moderator)) +
    # creating diamonsts (95% CI)
    ggplot2::geom_polygon(data = sum_data, aes(x = x.diamond, y = y.diamond, group = moderator), fill = "red") +
    #ggplot2::facet_wrap(~moderator, scales = "free_y", nrow = GN,  strip.position = "left") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y = element_text(angle = 0, size = 8),# margin = margin(t=15, r=15, b=15, l=15)), 
                   strip.background = element_rect(colour = NULL,
                                                   linetype = "blank",
                                                   fill = "gray90"),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::labs(x = "Effect Size (SMD)", y = "") +
    # putting k
    ggplot2::annotate('text', x = max(data$upper)*0.9, y = mod_table$Y, label= paste("italic(k)==", mod_table$K), parse = TRUE, hjust = "right", size = 3.5) +
    ggplot2::annotate('text', x = min(data$lower)*0.9, y = mod_table$Y, label= mod_table$moderator, hjust = "left", size = 3.5) +
    coord_cartesian(xlim = c(min(data$lower)*1.05, max(data$upper)*1.05),
                    ylim = c((min(data$Y)-10), (max(data$Y)+4))
                    , expand = F)
  
  return(plot)
}


# test 3
###### test    
# data frame for the effect size level data set
# we need ID for y axis (should sort them out according to yi group_by moderator)

# to do - y_limit <- c(min(plotdata$ID) - 1, max(plotdata$ID) + 1.5)
# labelling - ggplot2::geom_plabel(ase())

data <- res4$data
data$lower <- data$yi - stats::qnorm(0.975)*sqrt(data$vi)
data$upper <- data$yi + stats::qnorm(0.975)*sqrt(data$vi)
data$moderator <- factor(data$moderator, labels = res4$mod_table$name)


# data frame for the meta-analytic results
mod_table <- res4$mod_table
mod_table$K <- as.vector(by(data, data[,"moderator"], function(x) length(x[,"yi"])))
mod_table$moderator <- mod_table$name
# the number of groups in a moderator
group_no <- nrow(mod_table)
data_no <- nrow(data)
#groups <- 

# use dplyr here - need to change.... 
# Dan can you make this basic R code - maybe I got it
# data <- data[order(data$moderator, -data$yi),]
data <- data %>% group_by(moderator) %>% arrange(moderator, desc(yi)) %>%  
  ungroup() %>% 
  mutate(Y = 1:data_no + 
           unlist(mapply(function(x, y) rep(x*6 , y) , x = 1:group_no, y = mod_table$K))
  ) %>% 
  data.frame()

# mod ID
mod_table$Y <- data %>% group_by(moderator) %>% 
  summarise(Y = first(Y)) %>% 
  select(Y) %>% t() %>% as.vector() -2

# preparing for diamons for summary - we need to prep y axis 
# copying from internal_viz_classicforest() from R package 
sum_data <- data.frame("x.diamond" = c(mod_table$lowerCL,
                                       mod_table$estimate ,
                                       mod_table$upperCL,
                                       mod_table$estimate ),
                       "y.diamond" = c(mod_table$Y,
                                       mod_table$Y + 1.2,
                                       mod_table$Y,
                                       mod_table$Y - 1.2),
                       "moderator" = rep(mod_table$name, times = 4)
)

# Make - 
plot <- ggplot2::ggplot(data = data, aes(x = yi, y = Y)) +
  # pieces of fruit (bee-swarm and bubbles)
  
  # 95 % CI
  ggplot2::geom_errorbarh(aes(xmin = lower, xmax = upper), 
                          colour = "#00CD00", height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  # 95 %CI: branches
  
  ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  # creating dots for truncks
  ggplot2::geom_point(colour = "#FFD700", size = 1) +
  # creating 95% prediction intervals
  ggplot2::geom_segment(data = mod_table, aes(x = lowerPR, y = Y, xend = upperPR, yend = Y, group = moderator)) +
  # creating diamonsts (95% CI)
  ggplot2::geom_polygon(data = sum_data, aes(x = x.diamond, y = y.diamond, group = moderator), fill = "red") +
  #ggplot2::facet_wrap(~moderator, scales = "free_y", nrow = GN,  strip.position = "left") +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text.y = element_text(angle = 0, size = 8),# margin = margin(t=15, r=15, b=15, l=15)), 
                 strip.background = element_rect(colour = NULL,
                                                 linetype = "blank",
                                                 fill = "gray90"),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank()) +
  ggplot2::labs(x = "Effect Size (SMD)", y = "") +
  # putting k
  ggplot2::annotate('text', x = max(data$upper)*0.9, y = mod_table$Y, label= paste("italic(k)==", mod_table$K), parse = TRUE, hjust = "right", size = 3.5) +
  ggplot2::annotate('text', x = min(data$lower)*0.9, y = mod_table$Y, label= mod_table$moderator, hjust = "left", size = 3.5) +
  coord_cartesian(xlim = c(min(data$lower)*1.05, max(data$upper)*1.05),
                  ylim = c((min(data$Y)-10), (max(data$Y)+4))
                  , expand = F)

plot

(g <- ggplotGrob(plot))  
