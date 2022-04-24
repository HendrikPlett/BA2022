library(plotly)
library(readxl)
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


#define error function 
plot_likelihood_logistic <- function(beta, gamma) {
  alpha <- 2.498
  likeli <- 0
  for(i in 1:nrow(ml_data)) {
    sigma <- alpha / (1 + exp(-beta * (ml_data[i,1] - gamma)))
    likeli <- likeli - 1/2 * log(2 * pi * (sigma)^2) - (ml_data[i,2]^2) / (2 * (sigma)^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}




simulate <- function(beta.seq, gamma.seq) {
  # initiate the matrix
  likelihood.matrix <- matrix(0, nrow=length(beta.seq), ncol=length(gamma.seq))
  

  for (i in 1:length(beta.seq)) {
    
    for (j in 1:length(gamma.seq)) {
      temp <- plot_likelihood_logistic(beta.seq[i], gamma.seq[j])
      if (temp >= 1000 | is.na(temp) == TRUE) {
        likelihood.matrix[i,j] <- 1000
      } else {
        likelihood.matrix[i,j] <- temp
      }
      
    }
    
  }
  return(likelihood.matrix)
}


likelihood <- simulate(seq(-1,0, by = (1/60)), seq(-12,-4, by = (8/60)))

# 3D plot of joint probability distribution without projection
fig.n <- plot_ly(z = ~likelihood)
fig.n <- fig.n %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)


fig.n


