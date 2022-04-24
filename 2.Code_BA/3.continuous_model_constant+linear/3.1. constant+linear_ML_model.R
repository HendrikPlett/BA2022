
library(readxl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(plotly)

#load excel file with transformed forecasts and modify it  
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Unconditional prediction', INSTITUTION)) #remove Naive prediction from data set 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set as their error would be zero --> no information 


#plot of preliminary Error against time without naive estimation 
plot_prel_error <- ggplot(prediction_data_transformed, aes(x = LAG_in_months, y = PREL_ERROR, color = INSTITUTION)) +
  geom_jitter(width = 0.15) + 
  labs(x = "Forecast horizon in months", y = "Error in percentage points") + 
  labs(title = "Forecasting error of real GDP growth in Germany") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(breaks = c("EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich", "Statistisches Bundesamt"),values = c("coral3","blue3", "chartreuse4", "grey"))




#use ml-estimator to fit constant+linear function to the error data

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


b_vector <- optim(par = c(0.134,0.132,-18), fn = likelihood, data = ml_data, method = "Nelder-Mead")



#add quantiles of found distribution to plot

quantile_function_0.95 <- function(t) {
  #calculate standart deviation for different t values 
  
  sd <- ifelse(test = (t <= b_vector$par[3]), yes = b_vector$par[1] - b_vector$par[3] * b_vector$par[2], 
               no = b_vector$par[1] - t * b_vector$par[2])
  
  return (qnorm(0.95, mean = 0, sd = abs(sd)))
}


quantile_function_0.05 <- function(t) {
  #calculate standart deviation for different t values 
  sd <- ifelse(test = (t <= b_vector$par[3]), yes = b_vector$par[1] - b_vector$par[3] * b_vector$par[2], 
               no = b_vector$par[1] - t * b_vector$par[2])
  
  return (qnorm(0.05, mean = 0, sd = abs(sd)))
}


plot_with_quantiles <- plot_prel_error + 
  geom_function(fun = quantile_function_0.95, color = "Black")+
  geom_function(fun = quantile_function_0.05, color = "Black")
plot_with_quantiles






