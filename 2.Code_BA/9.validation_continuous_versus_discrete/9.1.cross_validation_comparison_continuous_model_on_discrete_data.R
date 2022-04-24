
library(readxl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(tidyverse) # plotting and manipulation
library(grid)      # combining plots
library(gridExtra) # combining plots
library(ggpubr)    # combining plots
library(patchwork) # combining plots


#load validation data from the two discrete models 
load("9.validation_continuous_versus_discrete/Validation_scores_discrete_models.RData")




#load excel file with transformed forecasts (including realization and naive prediction)
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))

prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Unconditional prediction', INSTITUTION)) #remove Naive prediction from data set 


prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set


for(i in 1:(nrow(prediction_data_transformed))){ #make predicted year a number
  prediction_data_transformed$PREDICTED_YEAR[i] <- as.numeric(unlist(strsplit(prediction_data_transformed$PREDICTED_YEAR[i], split='_', fixed=TRUE))[2])
}
prediction_data_transformed$PREDICTED_YEAR <- as.numeric(prediction_data_transformed$PREDICTED_YEAR) #make numeric 




#delete data according to discrete data set of table 6. 

prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '0') 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-2.5') 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-5') 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-8.5') 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-10')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-14.5')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-17')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-20.5')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!LAG_in_months == '-22')

prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!PREDICTED_YEAR == '1999')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!PREDICTED_YEAR == '2000')
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!PREDICTED_YEAR == '2001')




#define logistical likelihood function
likelihood_logistic <- function(b, data) {
  alpha <- b[1]
  beta <- b[2]
  gamma <- b[3]
  likeli <- 0
  for(i in 1:nrow(data)) {
    #define standart deviation
    sd <- alpha / (1 + exp(-beta * (data[i,1] - gamma)))
    #define likelihood value 
    likeli <- likeli - 1/2 * log(2 * pi * (sd)^2) - (data[i,2]^2) / (2 * (sd)^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}




counter <- 0
#for calculating average Loglikelihood (performance comparison)
logistical_loglikeli_sum <- 0
linear_loglikeli_sum <- 0 
constantLinear_loglikeli_sum <- 0

#for plotting Loglikelihood for every data point
logistical_Loglikelihood_data <- rep(0, times = nrow(prediction_data_transformed))
linear_Loglikelihood_data <- rep(0, times = nrow(prediction_data_transformed))
constantLinear_Loglikelihood_data <- rep(0, times = nrow(prediction_data_transformed))

#for calculating average b-vector 
logistical_b_vector_sum <- rep(0, times = 3)
linear_b_vector_sum <- rep(0, times = 2)
constantLinear_b_vector_sum <- rep(0, times = 3)


for (j in 0 : (prediction_data_transformed$PREDICTED_YEAR %>% n_distinct()-1)) {
  
  
  
  training_prediction_data_transformed <- prediction_data_transformed %>% 
    filter(!grepl((min(prediction_data_transformed$PREDICTED_YEAR) + j), PREDICTED_YEAR)) #remove true GDP values from data set
  
  validation_prediction_data_transformed <- prediction_data_transformed %>% 
    filter(grepl((min(prediction_data_transformed$PREDICTED_YEAR) + j), PREDICTED_YEAR)) #remove true GDP values from data set
  
  
  #get training data needed for ML 
  training_data_temp <- select(training_prediction_data_transformed, LAG_in_months, PREL_ERROR) 
  training_data_temp <- cbind(as.numeric(training_data_temp[,1]), training_data_temp[,2])
  #order rows of data matrix in ascending order regarding forecasting horizon
  training_data_temp <- training_data_temp[order(training_data_temp[,1], decreasing = FALSE),]
  
  
  #get validation data needed for ML 
  validation_data_temp <- select(validation_prediction_data_transformed, LAG_in_months, PREL_ERROR) 
  validation_data_temp <- cbind(as.numeric(validation_data_temp[,1]), validation_data_temp[,2])
  #order rows of data matrix in ascending order regarding forecasting horizon
  validation_data_temp <- validation_data_temp[order(validation_data_temp[,1], decreasing = FALSE),]
  
  

  #for showing which data points influence b-vector optimization most 
  logistical_b_vector_data <- matrix(0, nrow = nrow(prediction_data_transformed), ncol = 3)
  linear_b_vector_data <- matrix(0, nrow = nrow(prediction_data_transformed), ncol = 2)
  constantLinear_b_vector_data <- matrix(0, nrow = nrow(prediction_data_transformed), ncol = 3)
  
  
  #training and validation of the three models
  #logistical 
  logistical_b_vector_temp <- optim(par = c(2.79,-0.27,-9.5), fn = likelihood_logistic, data = training_data_temp)
  print(logistical_b_vector_temp$par)
  logistical_b_vector_data[j+1,] <- logistical_b_vector_temp$par
  logistical_b_vector_sum <- logistical_b_vector_sum + logistical_b_vector_temp$par
  
  print(validation_data_temp)
  print(nrow(validation_data_temp))
  for(p in 1 : nrow(validation_data_temp)){
    validation_data_temp <- as.data.frame(validation_data_temp)
    counter <- counter + 1
    print(counter)
    print(validation_data_temp %>% slice(p))
    logistical_likelihood_temp <- -1 * likelihood_logistic(logistical_b_vector_temp$par, validation_data_temp %>% slice(p))
    print(logistical_likelihood_temp)
    logistical_Loglikelihood_data[counter] <- logistical_likelihood_temp
    logistical_loglikeli_sum <- logistical_loglikeli_sum + logistical_likelihood_temp
  }
  

  
  
  
}



#calculating means 
#logistical
logistical_loglikeli_mean <- logistical_loglikeli_sum / nrow(prediction_data_transformed)
logistical_b_vector_mean <- logistical_b_vector_sum / nrow(prediction_data_transformed) 


#printing means 
#logistical
print(logistical_loglikeli_mean)
print(logistical_b_vector_mean)




#T-test for difference between discrete and continiuous model when continuous model is trained on reduced data set 
t.test(validation_data_bivariate_for_plot[1:152,2], logistical_Loglikelihood_data, var.equal = FALSE, alternative = "less")      
t.test(validation_data_multivariate_for_plot[1:152,2], logistical_Loglikelihood_data, var.equal = FALSE, alternative = "less")      
t.test(validation_data_bivariate_for_plot[1:152,2], validation_data_multivariate_for_plot[1:152,2], var.equal = FALSE, alternative = "greater")      


