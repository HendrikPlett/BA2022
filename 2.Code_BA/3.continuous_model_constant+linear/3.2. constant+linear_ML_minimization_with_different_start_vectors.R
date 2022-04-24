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
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set as their error would be zero --> no information 

ml_data <- select(prediction_data_transformed, LAG_in_months, PREL_ERROR) #get data 




likelihood <- function(b, data) {
  b1 <- b[1]
  b2 <- b[2]
  switch <- b[3]
  likeli <- 0
  for(i in 1:nrow(data)) {
    #define sd 
    if (data[i,1] <= switch) {
      sd <- b1 - b2 * switch
    } else {
      sd <- b1 - b2 * data[i,1]
    }
    likeli <- likeli - 1/2 * log(2 * pi * (sd)^2) - (data[i,2]^2) / (2 * (sd)^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}


#optimization process for different starting vectors
ten <- seq(1,10)
b1 <- 0 + 0.05 * ten 
b2 <- 0 + 0.05 * ten
switch <- - 4 -  1 * ten


valuematrix <- as.matrix(expand.grid(b1, b2, switch))
a.0 <- rep(0, times = nrow(valuematrix))
valuematrix <- cbind(valuematrix, a.0, a.0, a.0, a.0)
colnames(valuematrix) <- c("b1_start", "b2_start", "switch_start", "b1_optim", "b2_optim", "switch_optim", "likelihood_value")

for (i in 1: nrow(valuematrix)) {
  
  temp_optim_vector <- optim(par = c(valuematrix[i,1],valuematrix[i,2],valuematrix[i,3]), fn = likelihood, data = ml_data)
  valuematrix[i,4] <- temp_optim_vector$par[1]
  valuematrix[i,5] <- temp_optim_vector$par[2]
  valuematrix[i,6] <- temp_optim_vector$par[3]
  valuematrix[i,7] <- temp_optim_vector$value[1]

}


