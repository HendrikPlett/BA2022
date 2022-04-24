
library(readxl)
library(ggplot2)
library(ggridges)
library(scales)
library(magrittr) #for pipes 
library(dplyr)
library(tidyverse) # plotting and manipulation
library(grid)      # combining plots
library(gridExtra) # combining plots
library(ggpubr)    # combining plots
library(patchwork) # combining plots


#load excel file with transformed forecasts (including realization and naive prediction)
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))

prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Unconditional prediction', INSTITUTION)) #remove Naive prediction from data set 


prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set




#define logistical likelihood function
likelihood_logistic <- function(b, data) {
  alpha <- b[1]
  beta <- b[2]
  gamma <- b[3]
  likeli <- 0
  for(i in 1:nrow(data)) {
    #define standart deviation
    sd <- alpha / (1 + exp(-beta * (data[i,1] - gamma)))
    #define likelihood value 
    likeli <- likeli - 1/2 * log(2 * pi * (sd)^2) - (data[i,2]^2) / (2 * (sd)^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}



#define linear likelihood function
likelihood_linear <-function(b, data) {
  b1 <- b[1]
  b2 <- b[2]
  likeli <- 0
  for(i in 1:nrow(data)) {
    likeli <- likeli - 1/2 * log(2 * pi * (b1 - b2 * data[i,1])^2) - (data[i,2]^2) / (2 * (b1-b2*data[i,1])^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}



#define constant+linear likelihood function
likelihood_constantLinear <- function(b, data) {
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




#get data needed for ML 
ml_data <- select(prediction_data_transformed, LAG_in_months, PREL_ERROR) 
ml_data <- cbind(as.numeric(ml_data[,1]), ml_data[,2])
#order rows of data matrix in ascending order regarding forecasting horizon
ml_data_ordered <- ml_data[order(ml_data[,1], decreasing = FALSE),]


#Set parameter for k-cross validation 
#must be divisor of length(ml_data_ordered) = 260
k <- nrow(ml_data_ordered) # length of resulting data sets = 260/260 = 1
length_of_data_set <- nrow(ml_data_ordered) / k 


#for calculating average Loglikelihood (performance comparison)
logistical_loglikeli_sum <- 0
linear_loglikeli_sum <- 0 
constantLinear_loglikeli_sum <- 0

#for plotting Loglikelihood for every data point
logistical_Loglikelihood_data <- rep(0, times = k)
linear_Loglikelihood_data <- rep(0, times = k)
constantLinear_Loglikelihood_data <- rep(0, times = k)

#for calculating average b-vector 
logistical_b_vector_sum <- rep(0, times = 3)
linear_b_vector_sum <- rep(0, times = 2)
constantLinear_b_vector_sum <- rep(0, times = 3)

#for showing which data points influence b-vector optimization most 
logistical_b_vector_data <- matrix(0, nrow = k, ncol = 3)
linear_b_vector_data <- matrix(0, nrow = k, ncol = 2)
constantLinear_b_vector_data <- matrix(0, nrow = k, ncol = 3)



#iterating through the entire data set 
for (j in 0 : (k - 1)) {
  
  
  #split data set into validation and training data
  lower_bound <- (length_of_data_set*j)+1
  upper_bound <- length_of_data_set*(j+1)
  validation_data_temp <- matrix(ml_data_ordered[lower_bound : upper_bound,], ncol = 2)
  training_data_temp <- matrix(ml_data_ordered[-lower_bound : -upper_bound,], ncol = 2)
  
  
  #training and validation of the three models
      #logistical 
      logistical_b_vector_temp<- optim(par = c(2.79,-0.27,-9.5), fn = likelihood_logistic, data = training_data_temp)
      logistical_b_vector_data[j+1,] <- logistical_b_vector_temp$par
      logistical_b_vector_sum <- logistical_b_vector_sum + logistical_b_vector_temp$par
  
      logistical_likelihood_temp <- -1 * likelihood_logistic(logistical_b_vector_temp$par, validation_data_temp)
      logistical_Loglikelihood_data[j+1] <- logistical_likelihood_temp
      logistical_loglikeli_sum <- logistical_loglikeli_sum + logistical_likelihood_temp
      
      #linear 
      linear_b_vector_temp<- optim(par = c(0.1375,0.1271), fn = likelihood_linear, data = training_data_temp)
      linear_b_vector_data[j+1,] <- linear_b_vector_temp$par
      linear_b_vector_sum <- linear_b_vector_sum + linear_b_vector_temp$par
      
      linear_likelihood_temp <- -1 * likelihood_linear(linear_b_vector_temp$par, validation_data_temp)
      linear_Loglikelihood_data[j+1] <- linear_likelihood_temp
      linear_loglikeli_sum <- linear_loglikeli_sum + linear_likelihood_temp
      
      #constant+linear 
      constantLinear_b_vector_temp<- optim(par = c(0.1350,0.1307,-19.5353), fn = likelihood_constantLinear, data = training_data_temp)
      constantLinear_b_vector_data[j+1,] <- constantLinear_b_vector_temp$par
      constantLinear_b_vector_sum <- constantLinear_b_vector_sum + constantLinear_b_vector_temp$par
      
      constantLinear_likelihood_temp <- -1 * likelihood_constantLinear(constantLinear_b_vector_temp$par, validation_data_temp)
      constantLinear_Loglikelihood_data[j+1] <- constantLinear_likelihood_temp
      constantLinear_loglikeli_sum <- constantLinear_loglikeli_sum + constantLinear_likelihood_temp
      
      
      
}


#calculating means 
      #logistical
      logistical_loglikeli_mean <- logistical_loglikeli_sum / k
      logistical_b_vector_mean <- logistical_b_vector_sum / k 
      
      #linear
      linear_loglikeli_mean <- linear_loglikeli_sum / k
      linear_b_vector_mean <- linear_b_vector_sum / k 
      
      #constant + linear 
      constantLinear_loglikeli_mean <- constantLinear_loglikeli_sum / k
      constantLinear_b_vector_mean <- constantLinear_b_vector_sum / k 
    

#printing means 
      #logistical
      print(logistical_b_vector_mean)
      print(logistical_loglikeli_mean)
      
      #linear
      print(linear_b_vector_mean)
      print(linear_loglikeli_mean)
      
      #linear
      print(constantLinear_b_vector_mean)
      print(constantLinear_loglikeli_mean)

#plot LogLikelihood values for all 260 data points 
      plot(logistical_Loglikelihood_data)
      plot(linear_Loglikelihood_data)
      plot(constantLinear_Loglikelihood_data)

      

#student's t-test for different expected values
t.test(linear_Loglikelihood_data, constantLinear_Loglikelihood_data, var.equal = TRUE, alternative = "two.sided")      
t.test(linear_Loglikelihood_data, logistical_Loglikelihood_data, var.equal = TRUE, alternative = "two.sided")      
t.test(constantLinear_Loglikelihood_data, logistical_Loglikelihood_data, var.equal = TRUE, alternative = "two.sided")      


#calculate means for forecast horizon larger than 19.5353      

smaller <- filter(as.data.frame(ml_data_ordered), V1 <= -19.5353) %>% nrow()

linear_logLikelihood_long <- linear_Loglikelihood_data[1:smaller]
linear_logLikelihood_short <- linear_Loglikelihood_data[(smaller + 1):length(linear_Loglikelihood_data)]

constantLinear_logLikelihood_long <- constantLinear_Loglikelihood_data[1:smaller]
constantLinear_logLikelihood_short <- constantLinear_Loglikelihood_data[(smaller + 1):length(constantLinear_Loglikelihood_data)]

logistical_logLikelihood_long <- logistical_Loglikelihood_data[1:smaller]
logistical_logLikelihood_short <- logistical_Loglikelihood_data[(smaller + 1):length(logistical_Loglikelihood_data)]


linear_logLikelihood_mean_long <- mean(linear_logLikelihood_long)
constantLinear_logLikelihood_mean_long <- mean(constantLinear_logLikelihood_long)
logistical_logLikelihood_mean_long <- mean(logistical_logLikelihood_long)
print(linear_logLikelihood_mean_long)
print(constantLinear_logLikelihood_mean_long)
print(logistical_logLikelihood_mean_long)


linear_logLikelihood_mean_short <- mean(linear_logLikelihood_short)
constantLinear_logLikelihood_mean_short <- mean(constantLinear_logLikelihood_short)
logistical_logLikelihood_mean_short <- mean(logistical_logLikelihood_short)
print(linear_logLikelihood_mean_short)
print(constantLinear_logLikelihood_mean_short)
print(logistical_logLikelihood_mean_short)

t.test(linear_logLikelihood_long, constantLinear_logLikelihood_long, var.equal = TRUE, alternative = "two.sided")      
t.test(linear_logLikelihood_long, logistical_logLikelihood_long, var.equal = TRUE, alternative = "two.sided")      
t.test(constantLinear_logLikelihood_long, logistical_logLikelihood_long, var.equal = TRUE, alternative = "two.sided")      



#manipulate data for joint plot 

logistical_data <- cbind(c(1:length(logistical_Loglikelihood_data)), 
                         logistical_Loglikelihood_data, 
                         rep("Logistical", times = length(logistical_Loglikelihood_data)),
                         ml_data_ordered[,1])
linear_data <- cbind(c(1:length(linear_Loglikelihood_data)), 
                     linear_Loglikelihood_data, 
                     rep("Linear", times = length(linear_Loglikelihood_data)),
                     ml_data_ordered[,1])
constantLinear_data <- cbind(c(1:length(constantLinear_Loglikelihood_data)), 
                             constantLinear_Loglikelihood_data, 
                             rep("Constant + linear", times = length(constantLinear_Loglikelihood_data)),
                             ml_data_ordered[,1])

plot_data <- as.data.frame(rbind(linear_data, constantLinear_data, logistical_data))
colnames(plot_data) <- c("Data_point", "LogLikelihood_value", "Model", "Forecast_Horizon")
plot_data$Data_point <- as.numeric(plot_data$Data_point)
plot_data$LogLikelihood_value <- as.numeric(plot_data$LogLikelihood_value)
plot_data$Forecast_Horizon <- as.numeric(plot_data$Forecast_Horizon)



validation_plot1 <- ggplot(plot_data, aes(x = Data_point, y = LogLikelihood_value, color = Model, shape = Model, size = Model)) +
  geom_point() +
  scale_color_manual(breaks = c("Linear", "Constant + linear", "Logistical"),values = c("blue3", "chartreuse4","coral3")) +
  scale_shape_manual(breaks = c("Linear", "Constant + linear", "Logistical"),values = c(17, 15, 16)) +
  scale_size_manual(breaks = c("Linear", "Constant + linear", "Logistical"),values = c(0.4, 0.5,0.5)) +
  #scale_size_manual(breaks = c())
  labs(x = "data points in order of descending forecast horizons", y = "") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = c(0,50,100,150,200,250)) +
  theme(legend.position = "bottom") +
  guides(shape = guide_legend(override.aes = list(size = 5))) + 
  scale_fill_discrete(breaks = c("Linear", "Constant + linear", "Logistical"))




validation_plot2 <- ggplot(plot_data, aes(x = Model, y = LogLikelihood_value, color = Model)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  #geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(width = 0, size = 1, alpha = 0.1) + 
  stat_summary(color = "black", size = 0.7, shape = 4) +
  labs(x = "Model", y = "LogLikelihood value") + 
  scale_color_manual(breaks = c("Logistical", "Linear", "Constant + linear"),values = c("coral3","blue3", "chartreuse4"),
                     guide = guide_legend(override.aes = list(color = "white"))) +
  theme(legend.position="bottom",
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white")) +
  scale_x_discrete(limits = c("Linear", "Constant + linear", "Logistical"),
                   labels = c("Linear" = "Linear", "Constant + linear" = "C + l", "Logistical" = "Logistical")
  ) +
  scale_y_continuous(breaks = c(-10,-8,-6,-4,-2,0,2))






grid.arrange(
  validation_plot2, validation_plot1,
  nrow = 1, 
  widths = c(1,3)
  )




