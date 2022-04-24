

library(readxl)
library(writexl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(scoringRules)
library(grid)      # combining plots
library(gridExtra) # combining plots
library(ggpubr)    # combining plots
library(patchwork) # combining plots


#load excel files with validation data from the multivariate and (weighted) bivariate model 
load("8.validation+comparison_discrete_models/Update_data_bivariate.RData")

load("8.validation+comparison_discrete_models/Update_data_multivariate.RData")
update_data_multivariate <- update_list_multivariate


make_plot_function <- function(nr_year) {
  
  
  #create matrix for bivariate model: 
  
  update_data_bivariate_for_plot <- matrix(nrow  = ncol(update_data_bivariate[[nr_year]]), ncol = 3) %>% as.data.frame()
  rownames(update_data_bivariate_for_plot) = c(-28, 
                                               -1*as.numeric(colnames(update_data_bivariate[[nr_year]])[2:length(colnames(update_data_bivariate[[nr_year]]))]))
  
  colnames(update_data_bivariate_for_plot) = c("Forecast value", "Resulting mean", "Resulting standard deviation")
  
  for (i in 1: (update_data_bivariate[[nr_year]] %>% ncol())) {
    update_data_bivariate_for_plot[i,1] <- update_data_bivariate[[nr_year]][1,i] #forecast 
    update_data_bivariate_for_plot[i,2] <- update_data_bivariate[[nr_year]][2,i] #mean 
    update_data_bivariate_for_plot[i,3] <- update_data_bivariate[[nr_year]][3,i] #sd
  }
  
  
  
  #create matrix for multivariate model 
  
  update_data_multivariate_for_plot <- matrix(nrow  = ncol(update_data_multivariate[[nr_year]]), ncol = 3) %>% as.data.frame()
  rownames(update_data_multivariate_for_plot) = c(-28, 
                                                  -1*as.numeric(colnames(update_data_multivariate[[nr_year]])[2:length(colnames(update_data_multivariate[[nr_year]]))]))
  colnames(update_data_multivariate_for_plot) = c("Forecast value", "Resulting mean", "Resulting standard deviation")
  
  for (i in 1: (update_data_multivariate[[nr_year]] %>% ncol())) {
    update_data_multivariate_for_plot[i,1] <- update_data_multivariate[[nr_year]][1,i] #forecast 
    update_data_multivariate_for_plot[i,2] <- update_data_multivariate[[nr_year]][2,i] #mean 
    update_data_multivariate_for_plot[i,3] <- update_data_multivariate[[nr_year]][3,i] #sd
  }
  
  
  
  
  
  plot_update_data <- ggplot() +
    scale_x_continuous(breaks = rownames(update_data_bivariate_for_plot) %>% as.numeric() %>% unique()) + 
    theme(axis.text.x = element_text(size=06, angle=45), axis.text.y = element_text(size=06)) +
    scale_y_continuous(breaks= pretty_breaks(n=12)) + 
    labs(x = "Forecast horizon in months", y = "GDP growth in percent", title = (2001+nr_year)) + 
    theme(axis.title = element_text(size = 8)) +
    geom_point(mapping = aes(x = rownames(update_data_bivariate_for_plot) %>% as.numeric(), y = update_data_bivariate_for_plot[,1] %>% as.numeric(),
                             color = "black"),
               data = update_data_bivariate_for_plot,
               position = position_nudge(x = 0), size = 3, shape = 19) + #adding the weighted bivariate model in blue
    geom_point(mapping = aes(x = rownames(update_data_bivariate_for_plot) %>% as.numeric(), y = update_data_bivariate_for_plot[,2] %>% as.numeric(),
                             color = "blue"),
               data = update_data_bivariate_for_plot,
               position = position_nudge(x = -0.1), size = 2, shape = 22) + #adding the updated point estimation of the bivariate model 
    geom_linerange(mapping = aes(x = rownames(update_data_bivariate_for_plot) %>% as.numeric(), 
                                 ymin = update_data_bivariate_for_plot[,2] %>% as.numeric() - 2 * update_data_bivariate_for_plot[,3] %>% as.numeric(),
                                 ymax = update_data_bivariate_for_plot[,2] %>% as.numeric() + 2 * update_data_bivariate_for_plot[,3] %>% as.numeric(),
                                 color = "blue"),
                   data = update_data_bivariate_for_plot,
                   position = position_nudge(x = -0.1), size = 0.3) +
    geom_point(mapping = aes(x = rownames(update_data_multivariate_for_plot) %>% as.numeric(), y = update_data_multivariate_for_plot[,2] %>% as.numeric(),
                             color = "red"),
               data = update_data_multivariate_for_plot,
               position = position_nudge(x = 0.1), size = 2, shape = 23) + #adding the updated point estimation of the multivariate model 
    geom_linerange(mapping = aes(x = rownames(update_data_multivariate_for_plot) %>% as.numeric(), 
                                 ymin = update_data_multivariate_for_plot[,2] %>% as.numeric() - 2 * update_data_multivariate_for_plot[,3] %>% as.numeric(),
                                 ymax = update_data_multivariate_for_plot[,2] %>% as.numeric() + 2 * update_data_multivariate_for_plot[,3] %>% as.numeric(),
                                 color = "red"),
                   data = update_data_multivariate_for_plot,
                   position = position_nudge(x = 0.1), size = 0.3) +
    scale_color_identity(name='Model',
                         breaks=c('black','blue', 'red'),
                         labels=c("Forecast","Bivariate", "Multivariate"),
                         guide = "legend") + #adding legend 
    #geom_hline(yintercept=0, color = "black", size = 0.2) +
    geom_hline(yintercept=update_data_bivariate[[nr_year]][1,10], linetype = "dashed", color = "black", size = 0.2)
    
  
  
  
  return(plot_update_data)
  
  
}


show(make_plot_function(1))


#combine individual graphics

ggarrange(
  make_plot_function(1),
  make_plot_function(2),
  make_plot_function(3),
  make_plot_function(4),
  make_plot_function(5),
  make_plot_function(6),
  make_plot_function(7),
  make_plot_function(8),
  make_plot_function(9),
  make_plot_function(10),
  nrow = 5, ncol = 2,  
  common.legend = TRUE, 
  legend="bottom")


ggarrange(
  make_plot_function(11),
  make_plot_function(12),
  make_plot_function(13),
  make_plot_function(14),
  make_plot_function(15),
  make_plot_function(16),
  make_plot_function(17),
  make_plot_function(18),
  make_plot_function(19),
  nrow = 5, ncol = 2,  
  common.legend = TRUE, 
  legend="bottom")


