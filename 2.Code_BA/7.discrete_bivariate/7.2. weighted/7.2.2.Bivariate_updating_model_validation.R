

library(readxl)
library(writexl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(scoringRules)

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
BuM_data_original <- BuM_data #make copy of original data 



#matrix to store the weights according to which the mean-vector and covariance matrix will be weighted
weights_matrix <- matrix(0, nrow = ncol(BuM_data), ncol=ncol(BuM_data))
colnames(weights_matrix) <- rownames(weights_matrix) <- rev(colnames(BuM_data))



k_cross_validation <- function(theta) {
  
  
  #start k-cross validation 
  
  #create validation matrix to store the validation values 
  validation_data <- matrix(nrow = nrow(BuM_data), ncol= ncol(BuM_data))
  rownames(validation_data) <- rownames(BuM_data)
  colnames(validation_data) <- rev(colnames(BuM_data))
  
  #create list to store the updated normal distributions
  update_list_bivariate <- list()
  
  
  
  for (j in 1 : nrow(BuM_data_original)) {#iterate through years 
    
    #restore BuM_data to original version 
    BuM_data <- BuM_data_original
    
    #update year
    updating_data_year <- c(BuM_data[j,])
    BuM_data <- BuM_data[-j,]
    
    
    #calculate bivariate distribution of 0-column (true GDP) with every forecast horizon 
    
    #variance of true GDP, i.e. unconditional variance 
    variance_unconditional <- var(BuM_data[,1])
    mean_unconditional <- mean(BuM_data[,1])
    
    
    
    covariance_matrizes <- list() #list containing covariance matrizes of all forecast horizons 
    mean_vector <- list() #list containing mean vectors of all forecast horizons 
    
    
    for (i in ncol(BuM_data):1) { #filling the two lists with empirical data 
      
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
    
    
    #Weighting the mean vector and covariance matrix according to theta for all forecast horizons 
    weights_vector <- rep(0, times = length(mean_vector))
    for (p in 1 : length(mean_vector)) {
      
      #weights_vector[p] <- exp(theta * (25 - as.numeric(names(mean_vector)[p])))
      weights_vector[p] <- exp(theta *  - as.numeric(names(mean_vector)[p]))
      
    }
    

    
    for(p in 1 : ncol(weights_matrix)) {
      for(q in 1 : p) {
        weights_matrix[q,p] <- weights_vector[q] / sum(weights_vector[1:p])
      }
    }
    
    #print(weights_matrix)
    

    #Update mean-vector and covariance lists according to weights 
    
    covariance_matrizes_weighted <- list() 
    mean_vector_weighted <- list() 
    
    
    for (i in 1:ncol(BuM_data_original)) { #iterate trough forecast horizons
      
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
    
    
    
    
    
    
    
    #matrix to store the updates   
    distribution_over_time <- matrix(nrow = 4, ncol= ncol(BuM_data) + 1)
    rownames(distribution_over_time) <- c("Time of forecast", "Forecast value", "Resulting mean", "Resulting standart deviation")
    colnames(distribution_over_time) <- c("unconditional distrubution", colnames(BuM_data) %>% rev())
    
    
    #store unconditional mean,variance in the distribution_over_time matrix 
    distribution_over_time[3,1] <- mean_unconditional
    distribution_over_time[4,1] <- sqrt(variance_unconditional)
    
    
    #conditioning
    for(i in 1:ncol(BuM_data)) {
      
      distribution_over_time[2, i + 1] <- rev(updating_data_year)[i]
      
      distribution_over_time[3,i + 1] <- mean_unconditional + covariance_matrizes_weighted[[i]][1,2] / covariance_matrizes_weighted[[i]][2,2] * 
        (rev(updating_data_year)[i] - mean_vector_weighted[[i]][2])
      
      
      distribution_over_time[4, i + 1] <- sqrt(variance_unconditional - covariance_matrizes_weighted[[i]][1,2]^2/covariance_matrizes_weighted[[i]][2,2])
      
      
    }
    
    #store distribution over time matrix in list and reformat it 
    distribution_over_time <- distribution_over_time[-1,]
    update_list_bivariate[[j]] <- distribution_over_time
    
    
    #validation 
    
    for (k in 1 : length(updating_data_year)) {
      
      score_temp <- -1*logs_norm(updating_data_year[1], mean = distribution_over_time[2, k + 1], sd = distribution_over_time[3, k + 1])
      
      
      validation_data[j, k] <- score_temp
      
    }
    
    
    
  }
  
  
  names(update_list_bivariate) <- rownames(BuM_data_original)
  
  
  mean_validation_vector <- vector("numeric", length = ncol(validation_data))
  
  for(i in 1:ncol(validation_data)) {
    
    
    mean_validation_vector[i] <- mean(validation_data[,i])
    
  }
  
  
  validation_data <- rbind(validation_data, mean_validation_vector)
  
  
  
  #calculation of mean for all forecast horizons 
  
  sum <- 0
  
  
  for (i in 1: (ncol(validation_data) - 1)) {#-1 to exclude validation data points for h=2
    
    for (j in 1:nrow(validation_data)) {
      
      sum <- sum + validation_data[j,i]
      print(validation_data[j,i])
    }
  }
  
  average_validation_score_bivariat <- sum / (nrow(validation_data) * (ncol(validation_data)-1))
  
  returnlist <- list("validation data" = validation_data, "avg. validation score" = average_validation_score_bivariat, "updating_data_bivariat" = update_list_bivariate)
  
  return(returnlist)
  
  #end k-cross validation 
  
}


k_cross_validation_avgscore <- function(theta) {
  return(k_cross_validation(theta)[[2]])
}

k_cross_validation_validation_data <- function(theta) {
  return(k_cross_validation(theta)[[1]])
}

k_cross_validation_updating_data <- function(theta){
  return(k_cross_validation(theta)[[3]])
}

#optimization of average validation

optimal_theta <- optimize(k_cross_validation_avgscore, interval = c(0,10), maximum = TRUE) 

print(optimal_theta)



#plot function 
upper_plot_bound <- 10
lower_plot_bound <- 0

plot_data <- matrix(nrow = 1000, ncol = 2) 
colnames(plot_data) <- c("theta", "average validation score")
for (i in 1:nrow(plot_data)) {
  
  plot_data[i,1] <- (upper_plot_bound - lower_plot_bound)/1000 * i 
  plot_data[i,2] <- k_cross_validation_avgscore(plot_data[i,1])
  
}

plot <- ggplot() + 
  aes(x = plot_data[,"theta"], y = plot_data[,"average validation score"]) + 
  xlab("theta") + ylab("average validation score") +
  geom_point(size=0.1) + 
  scale_x_continuous(breaks = seq(0, 10, 1))

show(plot)



validation_data_bivariat <- as.data.frame(k_cross_validation_validation_data(optimal_theta[[1]]))

write_xlsx(validation_data_bivariat, 
           "8.validation+comparison_discrete_models/validation_data_bivariat.xlsx")


#recover updating data 


update_data_bivariate <- k_cross_validation_updating_data(optimal_theta[[1]])
validation_data <- k_cross_validation_validation_data(optimal_theta[[1]])
avg_score <- k_cross_validation_avgscore(optimal_theta[[1]])

