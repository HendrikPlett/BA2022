

library(readxl)
library(writexl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(scoringRules)

#load excel files with validation data from the multivariate and (weighted) bivariate model 
validation_data_bivariate <- as.data.frame(
  read_excel("8.validation+comparison_discrete_models/validation_data_bivariat.xlsx"))
validation_data_multivariate <- as.data.frame(
  read_excel("8.validation+comparison_discrete_models/validation_data_multivariat.xlsx"))


#seperate validation data from mean values
validation_data_bivariate <- validation_data_bivariate[-nrow(validation_data_bivariate),]
validation_data_multivariate <- validation_data_multivariate[-nrow(validation_data_multivariate),]


#25
t.test(validation_data_bivariate[,1], validation_data_multivariate[,1], var.equal = FALSE, alternative = "less")      

#19
t.test(validation_data_bivariate[,2], validation_data_multivariate[,2], var.equal = FALSE, alternative = "less")      

#18
t.test(validation_data_bivariate[,3], validation_data_multivariate[,3], var.equal = FALSE, alternative = "less")      

#13
t.test(validation_data_bivariate[,4], validation_data_multivariate[,4], var.equal = FALSE, alternative = "less")      

#12
t.test(validation_data_bivariate[,5], validation_data_multivariate[,5], var.equal = FALSE, alternative = "less")      

#7
t.test(validation_data_bivariate[,6], validation_data_multivariate[,6], var.equal = FALSE, alternative = "greater")      


#6
t.test(validation_data_bivariate[,7], validation_data_multivariate[,7], var.equal = FALSE, alternative = "greater")      

#1
t.test(validation_data_bivariate[,8], validation_data_multivariate[,8], var.equal = FALSE, alternative = "greater")      





validation_comparison <- matrix(nrow = 4, ncol = ncol(validation_data_bivariate))
colnames(validation_comparison) <- colnames(validation_data_bivariate)
rownames(validation_comparison) <- c("Mean Bivariate", "Mean Multivariate", "t-test") 



#transformation of data for plots 

validation_data_bivariate_for_plot <- matrix(nrow = ncol(validation_data_bivariate) * nrow(validation_data_bivariate), 
                                             ncol = 2)

for(i in 1 : ncol(validation_data_bivariate)) {
  
  for(j in 1:nrow(validation_data_bivariate)) {#HIER KORRIGIEREN
    
    validation_data_bivariate_for_plot[(i-1) * nrow(validation_data_bivariate) + j,1] <- -1 * as.numeric(colnames(validation_data_bivariate)[i])
    validation_data_bivariate_for_plot[(i-1) * nrow(validation_data_bivariate) + j,2] <- validation_data_bivariate[j,i]
    
  }
  
}
validation_data_bivariate_for_plot <- as.data.frame(validation_data_bivariate_for_plot)


validation_data_multivariate_for_plot <- matrix(nrow = ncol(validation_data_multivariate) * nrow(validation_data_multivariate), 
                                             ncol = 2)

for(i in 1 : ncol(validation_data_multivariate)) {
  
  for(j in 1:nrow(validation_data_multivariate)) {
    
    validation_data_multivariate_for_plot[(i-1) * nrow(validation_data_multivariate) + j,1] <- -1*as.numeric(colnames(validation_data_multivariate)[i])
    validation_data_multivariate_for_plot[(i-1) * nrow(validation_data_multivariate) + j,2] <- validation_data_multivariate[j,i]
    
  }
  
}
validation_data_multivariate_for_plot <- as.data.frame(validation_data_multivariate_for_plot)



#t-test over all forecast horizons 
t.test(validation_data_bivariate_for_plot[1:152,2], validation_data_multivariate_for_plot[1:152,2], var.equal = FALSE, alternative = "greater")      




plot_validation_data <- ggplot() +
  scale_x_continuous(breaks = validation_data_bivariate_for_plot[,1] %>% as.numeric() %>% unique()) + 
  scale_y_continuous(breaks= pretty_breaks(n=12)) + 
  theme(axis.text.x = element_text(angle=45)) +
  labs(x = "Forecast horizon in months", y = "Negative LogS validation score ") + 
  #labs(title = "LogS validation score for different models") +
  geom_point(mapping = aes(x = validation_data_bivariate_for_plot[,1] %>% as.numeric(), y = validation_data_bivariate_for_plot[,2] %>% as.numeric(),
                           color = "blue"),
             data = validation_data_bivariate_for_plot,
             position = position_nudge(x = -0.15), size = 0.4) + #adding the weighted bivariate model in blue
  geom_point(mapping = aes(x = validation_data_multivariate_for_plot[,1] %>% as.numeric(), y = validation_data_multivariate_for_plot[,2] %>% as.numeric(),     
                           color = "red"),
             data = validation_data_multivariate_for_plot,
             position = position_nudge(x = +0.15), size = 0.4) + #adding the multivariate model in red
  scale_color_identity(name='Model',
                     breaks=c('blue', 'red'),
                     labels=c("Bivariate", "Multivariate"),
                     guide = "legend") #adding legend 
  

  
show(plot_validation_data)


