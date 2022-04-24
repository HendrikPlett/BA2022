

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
colnames(BuM_data) <- -1* (as.numeric(prediction_data_transformed$LAG_in_months) %>% unique() %>% sort() %>% rev())



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



#Implement parametric covariance function dependent on sigma and alpha 


#Define function that calculates the exponents vector "exponents" 
#based on the the row/column in the covariance matrix
exponents_vector <- function(forecast_horizon_one, forecast_horizon_two) {
  
  #define sets of known months for each forecast horizon 
  set_of_all_months <- c(0:11)
  
  if (forecast_horizon_one > 11) {
    set_of_known_months_one <- c() 
  } else if (forecast_horizon_one >= 0) {
    set_of_known_months_one <- c(forecast_horizon_one : 11)
  } else if (forecast_horizon_one < 0) {
    set_of_known_months_one <- c(0:11)
  }
  
  
  if (forecast_horizon_two > 11) {
    set_of_known_months_two <- c() 
  } else if (forecast_horizon_two >= 0) {
    set_of_known_months_two <- c(forecast_horizon_two : 11)
  } else if (forecast_horizon_two < 0) {
    set_of_known_months_two <- c(0:11)
  }
  
  #vector to store all the exponents
  exponents_vector <- c()
  
  
  #fill vector with exponents 
  for (i in set_of_known_months_one) {
    for (k in set_of_known_months_two) {
      exponents_vector <- append(exponents_vector, abs(i-k))
    }
  }

  for(i in set_of_known_months_one) {
    for (l in setdiff(set_of_all_months, set_of_known_months_two)) {
      exponents_vector <- append(exponents_vector, abs(l-forecast_horizon_two) + abs(i - forecast_horizon_two))
    }
  }

  for(j in setdiff(set_of_all_months, set_of_known_months_one)) {
    for (k in set_of_known_months_two) {
      exponents_vector <- append(exponents_vector, abs(j-forecast_horizon_one) + abs(k-forecast_horizon_one))
    }
  }
  
  for(j in setdiff(set_of_all_months, set_of_known_months_one)) {
    for (l in setdiff(set_of_all_months, set_of_known_months_two)) {
      exponents_vector <- append(exponents_vector, abs(j-forecast_horizon_one) + 
                                   abs(l-forecast_horizon_two) + abs(forecast_horizon_one - forecast_horizon_two))
    }
    
    
  }
  

  return(exponents_vector)
  
}



#Define function to calculate covariance from alpha, sigma and the two forecasthorizons
covariance <- function(variables, forecast_horizons) { 
  exponents <- exponents_vector(forecast_horizons[1], forecast_horizons[2])
  nu <- variables[1]
  alpha <- variables[2]
  sigma <- variables[3]
  polynomial = 0
  for (i in 1: length(exponents)) {
    polynomial = polynomial + alpha^exponents[i]
  }
  covariance <- sigma^2/(1-alpha^2) * polynomial
  return (covariance)
}


#test
covariance(c(0.0188,0.8836,0.1024), c(5,-2))






#optim(par = c(0,0), fn = covariance, e = c(2,3,4))



#define covariance matrix as a function of alpha and sigma 
  
covariance_matrix_function <- function(parameters, BuM_data) {

  
  Covariance_matrix <- matrix(nrow = ncol(BuM_data), ncol = ncol(BuM_data))
  rownames(Covariance_matrix) <- colnames(BuM_data)
  colnames(Covariance_matrix) <- colnames(BuM_data)
  
  
  for (w in 1:nrow(Covariance_matrix)) {
    
    for (e in 1:ncol(Covariance_matrix)) {
      
      forecast_horizonAA <- c(
        as.numeric(rownames(Covariance_matrix)[w]), 
                                         as.numeric(colnames(Covariance_matrix)[e])
        )
      
      #print(forecast_horizonAA)

      Covariance_matrix[w,e] <- covariance(parameters, forecast_horizonAA)

    }
  }
    
  return(Covariance_matrix)
  
}

#test
Covariance_matrix <- covariance_matrix_function(c(0.0188,0.8836,0.1024), BuM_data)



#define function that returns the mean vector of the forecast horizons depending on nu and sigma  
mean_vector_function <- function(parameters, BuM_data) {
  nu <- parameters[1]
  alpha <- parameters[2]
  sigma <- parameters[3]
  
  #assumption: forecasts are right on average
  mean_vector <- rep(12*nu/(1-alpha), times = ncol(BuM_data))
  
  return(mean_vector)
  
}


#define log-likelihood function for estimation 

loglikelihood_BuM <- function(parameters, BuM_data) {
  
  Covariance <- covariance_matrix_function(parameters, BuM_data)
  print(Covariance)
  mean <- mean_vector_function(parameters, BuM_data)
  
  logli <- -(nrow(BuM_data) * ncol(BuM_data))/2 * log(2*pi) - 
    nrow(BuM_data)/2 * log(det(Covariance))
  
  for (q in 1 : nrow(BuM_data)) {
    logli <- logli - 
      1/2 * (
        (BuM_data[q,] - mean) %*% solve(Covariance) %*% (BuM_data[q,] - mean)
      )
  }
  
  return(-1 * logli)
  
}

#test
loglikelihood_BuM(c(0.0188, 0.8836, 0.1024), BuM_data)


#optimization 
#c(nu, alpha, sigma)
result <- optim(par = c(0.0188,0.8836,0.1024), fn = loglikelihood_BuM, BuM_data = BuM_data)
optimized_covariance_matrix <- covariance_matrix_function(result$par, BuM_data)
optimized_mean_vector <- mean_vector_function(result$par, BuM_data)





#correlation matrix 
sd_vector <- sqrt(diag(optimized_covariance_matrix))
optimized_correlation_matrix <- optimized_covariance_matrix 

for (i in 1:nrow(optimized_correlation_matrix)) {
  
  for (j in 1:ncol(optimized_correlation_matrix)) {
    
    optimized_correlation_matrix[i,j] <- optimized_covariance_matrix[i,j] / (sd_vector[i] * sd_vector[j])
    
  }
  
}


#now we know all parameters of Xt in the multivariate normal distribution 




#matrix to store information how distribution evolves over time 
distribution_over_time <- matrix(nrow = 4, ncol= nrow(optimized_covariance_matrix) + 1)
rownames(distribution_over_time) <- c("Time of forecast", "Forecast value", "Resulting mean", "Resulting variance")
colnames(distribution_over_time) <- c("unconditional distrubution", rownames(optimized_covariance_matrix) %>% rev())

#unconditional distribution 
current_GDP_growth_distribution <- c(optimized_mean_vector[1], optimized_covariance_matrix[1,1])
distribution_over_time[3,1] <- optimized_mean_vector[1]
distribution_over_time[4,1] <- optimized_covariance_matrix[1,1]


#define year (i.e. data point) that goes into the updating model 
updating_data_year <- BuM_data[8,]

#conditioning:
for (i in (as.numeric(nrow(optimized_covariance_matrix)) : 1)) {
  
  #print(i)

  #partitioning of mean-vector and covariance matrix according two how many forecast horizons are known 
  known_optimized_mean_vector <- optimized_mean_vector[i : length(optimized_mean_vector)] 
  unknown_optimized_mean_vector <- optimized_mean_vector[1 : (i - 1)]
  

  covariance_known_known <- optimized_covariance_matrix[i : nrow(optimized_covariance_matrix), i : nrow(optimized_covariance_matrix)] 
  covariance_unknown_unknown <- optimized_covariance_matrix[(1 : i-1), (1 : i-1)]
  covariance_known_unknown_bottom_left <- optimized_covariance_matrix[i : nrow(optimized_covariance_matrix), 1 : (i-1)] 
  covariance_unknown_known_top_right <- optimized_covariance_matrix[1 : (i-1), i : nrow(optimized_covariance_matrix)] 
  

  #mean update of Yt, i.e the true GDP growth 
  current_GDP_growth_distribution[1] <- (unknown_optimized_mean_vector + 
    covariance_unknown_known_top_right  %*% solve(covariance_known_known) %*% 
    (updating_data_year[i : length(updating_data_year)] - known_optimized_mean_vector))[1]
  

  #variance update of Yt, i.e. the true GDP growth 
  current_GDP_growth_distribution[2] <- (covariance_unknown_unknown - 
    covariance_unknown_known_top_right %*% solve(covariance_known_known) %*% 
    covariance_known_unknown_bottom_left)[1,1]
  

  #store values in "distribution over time matrix"
  distribution_over_time[3, (nrow(optimized_covariance_matrix) + 2 - i)] <- 
    current_GDP_growth_distribution[1]
  
  distribution_over_time[4, (nrow(optimized_covariance_matrix) + 2 - i)] <- 
    current_GDP_growth_distribution[2]
  
  distribution_over_time[2, (nrow(optimized_covariance_matrix) + 2 - i)] <- 
    updating_data_year[i]
  
  
}


#Reformating distribution_over_time matrix 
distribution_over_time <- distribution_over_time[-1,]
rownames(distribution_over_time)[3] <- "Resulting standart deviation"
distribution_over_time[3,] <- sqrt(distribution_over_time[3,])
