
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
plot_prel_error <- ggplot(data = prediction_data_transformed) +
  geom_jitter(aes(x = LAG_in_months, y = PREL_ERROR),width = 0.15) + 
  labs(x = "Forecast horizon in months", y = "Error in percentage points") + 
  #labs(title = "Forecasting error of real GDP growth in Germany") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) 
  #scale_color_manual(breaks = c("EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich", "Statistisches Bundesamt"),values = c("coral3","blue3", "chartreuse4", "grey"))
  
plot_prel_error


#add quantiles of found distribution to plot --> hard coding 
      #linear
      linear_quantile_function_0.95 <- function(t) {
        return (qnorm(0.95, mean = 0, sd = abs(0.1375 - t * 0.1271)))
      }
      
      linear_quantile_function_0.05 <- function(t) {
        return (qnorm(0.05, mean = 0, sd = abs(0.1375 - t * 0.1271)))
      }

      
      #constant + linear
      constant_plus_linear_quantile_function_0.95 <- function(t) {
        #calculate standart deviation for different t values 
        
        sd <- ifelse(test = (t <= -19.5353), yes = 0.1350 - (-19.5353) * 0.1307, 
                     no = 0.1350 - t * 0.1307)
        
        return (qnorm(0.95, mean = 0, sd = abs(sd)))
      }
      
      
      constant_plus_linear_quantile_function_0.05 <- function(t) {
        #calculate standart deviation for different t values 
        sd <- ifelse(test = (t <= -19.5353), yes = 0.1350 - (-19.5353) * 0.1307, 
                     no = 0.1350 - t * 0.1307)
        
        return (qnorm(0.05, mean = 0, sd = abs(sd)))
      }
      
      
      #logistical 
      logistical_quantile_function_0.95 <- function(t) {
        return (qnorm(0.95, mean = 0, sd = 2.7936 / (1 + exp(-(-0.2707) * (t - (-9.5710))))))
      }
      
      logistical_quantile_function_0.05 <- function(t) {
        return (qnorm(0.05, mean = 0, sd = 2.7936 / (1 + exp(-(-0.2707) * (t - (-9.5710))))))
      }
      
      
      
colors <- c("Linear" = "black", "Constant + linear" = "Red4", "Logistical" = "Navyblue")
plot_with_quantiles <- plot_prel_error + 
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = linear_quantile_function_0.95),
        mapping = aes(color = "linear")
  ) +
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = linear_quantile_function_0.05),
        mapping = aes(color = "linear")
  ) +
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = constant_plus_linear_quantile_function_0.95),
        mapping = aes(color = "constantLinear")
  ) +
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = constant_plus_linear_quantile_function_0.05),
        mapping = aes(color = "constantLinear")
  ) +
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = logistical_quantile_function_0.95),
        mapping = aes(color = "logistical")
  ) +
  layer(geom = "path",
        position = "identity",
        stat = "function",
        params = list(fun = logistical_quantile_function_0.05),
        mapping = aes(color = "logistical")
  ) +
  scale_color_manual(
    name = "Model",
    values = c("linear" = "blue", "constantLinear" = "red", "logistical" = "darkgreen"),
    labels = c("Linear", "Constant plus linear", "Logistical")
    ) 
  
plot_with_quantiles

