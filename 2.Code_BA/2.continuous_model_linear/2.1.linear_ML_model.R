
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
  #labs(title = "Forecasting error of real GDP growth in Germany") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(breaks = c("EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich"),values = c("coral3","blue3", "chartreuse4"))

plot_prel_error

#Linear model
#model <- lm(PREL_ERROR ~ LAG_in_months + 1, data = prediction_data_transformed)
#summary(model)


#create summary of predictions with mean, sd... for every forecast horizon 
summary_of_predictions <- matrix(
  nrow = prediction_data_transformed$LAG_in_months %>% 
    unique() %>% length(), #define rows of summary matrix as number of different forecast horizons in data set 
  ncol = 6) 
colnames(summary_of_predictions) <- c(
  "INSTITUTION", "FORECAST HORIZON", "NUMBER OF FORECASTS", 
  "MEAN", "SD", "p-value of KS-Test for ND") #define column names of matrix 
forecast_horizons <- prediction_data_transformed$LAG_in_months %>% 
  unique() %>% sort() #get vector of all distinct forecast horizons that need to be evaluated 


#fill every row of "summary_of_predictions" 
for(i in 1 : nrow(summary_of_predictions)) {
  
  data_temp <- prediction_data_transformed %>% 
    filter(LAG_in_months == forecast_horizons[i]) #data of the forecast horizon currently evaluated 
  
  #fill matrix  
  summary_of_predictions[i,1] <- data_temp$INSTITUTION[1] #institution 
  summary_of_predictions[i,2] <- as.numeric(data_temp$LAG_in_months[1])#forecast horizon
  summary_of_predictions[i,3] <- nrow(data_temp) #number of forecasts for that horizon 
  summary_of_predictions[i,4] <- data_temp$PREL_ERROR %>% mean() #mean error for that horizon 
  summary_of_predictions[i,5] <- data_temp$PREL_ERROR %>% sd() #standart deviation for that horizon 
  summary_of_predictions[i,6] <- ks.test(
    data_temp$PREL_ERROR, "pnorm", mean=mean(data_temp$PREL_ERROR), 
    sd=sd(data_temp$PREL_ERROR))$p.value #p-value of KS-test for normal distribution 
}

rm(data_temp) #delete data_temp file 



###use ML estimator to fit ND(0, b1-b2*Lag_in_months) to the data 

ml_data <- select(prediction_data_transformed, LAG_in_months, PREL_ERROR) #get data 

likelihood <- function(b, data) {
  b1 <- b[1]
  b2 <- b[2]
  likeli <- 0
  for(i in 1:nrow(data)) {
    #likeli <- likeli * (1/(sqrt(2 * pi) * (b1 - b2 * data[i,1]))) * exp(-1 * (data[i,2]) / (2*(b1-b2*data[i,1])^2)) 
    likeli <- likeli - 1/2 * log(2 * pi * (b1 - b2 * data[i,1])^2) - (data[i,2]^2) / (2 * (b1-b2*data[i,1])^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}


b_vector <- optim(par = c(0.1,0.1), fn = likelihood, data = ml_data, method = "Nelder-Mead")



#add quantiles of found distribution to plot, maybe for different polynomials 

quantile_function_0.95 <- function(t) {
  return (qnorm(0.95, mean = 0, sd = abs(b_vector$par[1] - t * b_vector$par[2])))
}

quantile_function_0.05 <- function(t) {
  return (qnorm(0.05, mean = 0, sd = abs(b_vector$par[1] - t * b_vector$par[2])))
}

plot_with_quantiles <- plot_prel_error + 
  geom_function(fun = quantile_function_0.95, color = "Black")+
  geom_function(fun = quantile_function_0.05, color = "Black")
plot_with_quantiles








