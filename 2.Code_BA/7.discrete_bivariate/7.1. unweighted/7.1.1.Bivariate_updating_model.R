

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


#update year --> to be changed 
updating_data_year <- BuM_data[7,]


#Updating 

distribution_over_time <- matrix(nrow = 4, ncol= ncol(BuM_data) + 1)
rownames(distribution_over_time) <- c("Time of forecast", "Forecast value", "Resulting mean", "Resulting standart deviation")
colnames(distribution_over_time) <- c("unconditional distrubution", colnames(BuM_data) %>% rev())


#unconditional 
distribution_over_time[3,1] <- mean_unconditional
distribution_over_time[4,1] <- sqrt(variance_unconditional)


#conditioning

for(i in 1:ncol(BuM_data)) {
  
  distribution_over_time[2, i + 1] <- rev(updating_data_year)[i]
  
  distribution_over_time[3,i + 1] <- mean_unconditional + covariance_matrizes[[i]][1,2] / covariance_matrizes[[i]][2,2] * 
    (rev(updating_data_year)[i] - mean_vector[[i]][2])
  
  
  distribution_over_time[4, i + 1] <- sqrt(variance_unconditional - covariance_matrizes[[i]][1,2]^2/covariance_matrizes[[i]][2,2])
  
  
}

distribution_over_time <- distribution_over_time[-1,]


