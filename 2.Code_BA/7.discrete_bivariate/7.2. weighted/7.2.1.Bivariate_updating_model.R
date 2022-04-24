

library(readxl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)

#load excel file with transformed forecasts (including realization and naive prediction)
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))

prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Unconditional prediction', INSTITUTION)) #remove Naive prediction from data set 



#make all lag-values whole numbers 
prediction_data_transformed$LAG_in_months <- as.numeric(
  ceiling(prediction_data_transformed$LAG_in_months))

#make predicted year a number 
for (i in 1:nrow(prediction_data_transformed)){
  prediction_data_transformed$PREDICTED_YEAR[i] <- as.numeric(
    unlist(strsplit(prediction_data_transformed$PREDICTED_YEAR[i], split='_', fixed=TRUE))[2])
  
} 


#create data matrix with necessary values  
BuM_data <- matrix(nrow = prediction_data_transformed$PREDICTED_YEAR %>% n_distinct(), 
                   ncol = prediction_data_transformed$LAG_in_months %>% n_distinct())

rownames(BuM_data) <- as.numeric(prediction_data_transformed$PREDICTED_YEAR) %>% unique() %>% sort()
colnames(BuM_data) <- -1 * (as.numeric(prediction_data_transformed$LAG_in_months) %>% unique() %>% sort() %>% rev())



#fill matrix with prediction values 
for (counter in 1:nrow(prediction_data_transformed)) {
  BuM_data[toString(prediction_data_transformed[counter,1]), 
           toString(-1*prediction_data_transformed[counter,2])] <- as.numeric(prediction_data_transformed[counter,4])
}


#remove empty rows/columns from data matrix --> HIGHLY PROBLEMATIC 
BuM_data <- BuM_data[-1:-3,]
delete_vector1 <- c(4,5,8,9,12,13,16,16,17) #eliminate columns that have missing data
BuM_data <- BuM_data[,-delete_vector1] #eliminate columns that have missing data

delete_vector2 <- c(2) 
BuM_data <- BuM_data[,-delete_vector2] #eliminate columns to ensure that the covariance matrice can be inverted  



#calculate bivariate distribution of 0-column (true GDP) with every forecast horizon 

#variance of true GDP, i.e. unconditional variance 
variance_unconditional <- var(BuM_data[,1])
mean_unconditional <- mean(BuM_data[,1])


covariance_matrizes <- list()
mean_vector <- list() 


for (i in ncol(BuM_data):1) {
  
  #mean vector of bivariate distribution 
  mean_vector[[ncol(BuM_data) - i + 1]] <- c(mean_unconditional, mean(BuM_data[,i]))
  names(mean_vector)[ncol(BuM_data) - i + 1] <- colnames(BuM_data)[i] 
  
  #covariance matrix of bivariate distribution 
  covariance_matrix_temp <- matrix(nrow = 2, ncol = 2) 
  covariance_matrix_temp[1,1] <- variance_unconditional
  covariance_matrix_temp[1,2] <- covariance_matrix_temp[2,1] <- cov(BuM_data[,1], BuM_data[,i]) 
  covariance_matrix_temp[2,2] <- var(BuM_data[,i])
  
  covariance_matrizes[[ncol(BuM_data) - i + 1]] <- covariance_matrix_temp
  names(covariance_matrizes)[ncol(BuM_data) - i + 1] <- colnames(BuM_data)[i]
  
}


+
#matrix to store the weights according to which the mean-vector and covariance matrix will be weighted
weights_matrix <- matrix(0, nrow = ncol(BuM_data), ncol=ncol(BuM_data))
colnames(weights_matrix) <- rownames(weights_matrix) <- rev(colnames(BuM_data))


#Weighting the mean vector and covariance matrix according to theta for all forecast horizons 
weights_vector <- rep(0, times = length(mean_vector))
for (p in 1 : length(mean_vector)) {
  
  #weights_vector[p] <- exp(theta * (25 - as.numeric(names(mean_vector)[p])))
  weights_vector[p] <- exp(0.5206536 *  - as.numeric(names(mean_vector)[p]))
  
}


for(p in 1 : ncol(weights_matrix)) {
  for(q in 1 : p) {
    weights_matrix[q,p] <- weights_vector[q] / sum(weights_vector[1:p])
  }
}


#Update mean-vector and covariance lists according to weights 

covariance_matrizes_weighted <- list() 
mean_vector_weighted <- list() 


for (i in 1:ncol(BuM_data)) { #iterate trough forecast horizons
  
  #mean vector of bivariate distribution 
  mean_vector_weighted_temp <- c(0,0)
  for (k in 1:i) {
    mean_vector_weighted_temp <- mean_vector_weighted_temp + weights_matrix[k,i] * mean_vector[[k]]
  }
  mean_vector_weighted[[i]] <- mean_vector_weighted_temp
  names(mean_vector_weighted)[i] <- names(mean_vector)[i]
  
  
  
  #covariance matrix of bivariate distribution 
  covariance_matrix_weighted_temp <- matrix(0, nrow = 2, ncol = 2) 
  
  for (k in 1:i) {
    covariance_matrix_weighted_temp <- covariance_matrix_weighted_temp + weights_matrix[k,i] * covariance_matrizes[[k]]
  }
  
  covariance_matrizes_weighted[[i]] <- covariance_matrix_weighted_temp
  names(covariance_matrizes_weighted)[i] <- names(covariance_matrizes)[i]
  
}



