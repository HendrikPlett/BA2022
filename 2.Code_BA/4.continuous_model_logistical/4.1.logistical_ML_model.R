
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


prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set



#plot of preliminary Error against forecast horizon
plot_prel_error <- ggplot(prediction_data_transformed, aes(x = LAG_in_months, y = PREL_ERROR, color = INSTITUTION)) +
  geom_jitter(width = 0.15) + 
  labs(x = "Forecast horizon in months", y = "Error in percentage points") + 
  labs(title = "Forecasting error of real GDP growth in Germany") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(breaks = c("EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich", "Statistisches Bundesamt", "Naive Prediction"),values = c("coral3","blue3", "chartreuse4", "grey", "red"))




#use ml-estimator to fit lostical model to the error data
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


b_vector_logistic <- optim(par = c(2,-0.7,-8), fn = likelihood_logistic, data = ml_data)


#add quantiles of found distribution to plot, maybe for different polynomials 

quantile_function_0.95 <- function(t) {
  return (qnorm(0.95, mean = 0, sd = b_vector_logistic$par[1] / (1 + exp(-b_vector_logistic$par[2] * (t - b_vector_logistic$par[3])))))
}

quantile_function_0.05 <- function(t) {
  return (qnorm(0.05, mean = 0, sd = b_vector_logistic$par[1] / (1 + exp(-b_vector_logistic$par[2] * (t - b_vector_logistic$par[3])))))
}

plot_with_quantiles <- plot_prel_error + 
  geom_function(fun = quantile_function_0.95, color = "Black") +
  geom_function(fun = quantile_function_0.05, color = "Black")
plot_with_quantiles




