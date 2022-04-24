library(plotly)
library(readxl)
library(writexl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)


#load excel file with transformed forecasts and modify it  
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Naive Prediction', INSTITUTION)) #remove Naive prediction from data set 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set 

ml_data <- select(prediction_data_transformed, LAG_in_months, PREL_ERROR) #get data 


likelihood_logistic <- function(b, data) {
  alpha <- b[1]
  beta <- b[2]
  gamma <- b[3]
  likeli <- 0
  for(i in 1:nrow(data)) {
    sigma <- alpha / (1 + exp(-beta * (data[i,1] - gamma)))
    likeli <- likeli - 1/2 * log(2 * pi * (sigma)^2) - (data[i,2]^2) / (2 * (sigma)^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}

ten <- seq(1,10)
alpha <- 1.8 + 0.1 * ten 
beta <- -0.7 + 0.05 * ten
gamma <- -3.8 -  0.8 * ten


valuematrix <- as.matrix(expand.grid(alpha, beta, gamma))
a.0 <- rep(0, times = nrow(valuematrix))
valuematrix <- cbind(valuematrix, a.0, a.0, a.0, a.0)
colnames(valuematrix) <- c("alpha_start", "beta_start", "gamma_start", "alpha_optim", "beta_optim", "gamma_optim", "likelihood_value")

for (i in 1: nrow(valuematrix)) {
  
  temp_optim_vector <- optim(par = c(valuematrix[i,1],valuematrix[i,2],valuematrix[i,3]), fn = likelihood_logistic, data = ml_data)
  valuematrix[i,4] <- temp_optim_vector$par[1]
  valuematrix[i,5] <- temp_optim_vector$par[2]
  valuematrix[i,6] <- temp_optim_vector$par[3]
  valuematrix[i,7] <- temp_optim_vector$value[1]

}


