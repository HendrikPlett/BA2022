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
  filter(!grepl('Statistisches Bundesamt', INSTITUTION)) #remove true GDP values from data set as their error would be zero --> no information 

ml_data <- select(prediction_data_transformed, LAG_in_months, PREL_ERROR) #get data 


#define error function 
plotlikelihood <- function(b1,b2) {
  likeli <- 0
  for(i in 1:nrow(ml_data)) {
    #likeli <- likeli * (1/(sqrt(2 * pi) * (b1 - b2 * data[i,1]))) * exp(-1 * (data[i,2]) / (2*(b1-b2*data[i,1])^2)) 
    likeli <- likeli - 1/2 * log(2 * pi * (b1 - b2 * ml_data[i,1])^2) - (ml_data[i,2]^2) / (2 * (b1-b2*ml_data[i,1])^2)
  }
  return (-1 * likeli) #optim function minimizes --> -1 * 
}



# simulate joint probability distribution (normal)



simulate <- function(b1.seq, b2.seq) {
  # initiate the matrix
  likelihood.matrix <- matrix(0, nrow=length(b1.seq), ncol=length(b2.seq))
  

  for (i in 1:length(b1.seq)) {
    
    for (j in 1:length(b2.seq)) {
      temp <- plotlikelihood(b1.seq[i], b2.seq[j])
      if (temp >= 500 | is.na(temp) == TRUE) {
        likelihood.matrix[i,j] <- 500
      } else {
        likelihood.matrix[i,j] <- temp
      }
      
    }
    
  }
  return(likelihood.matrix)
}


likelihood <- simulate(seq(-1,1, by = (2/60)), seq(-1,1, by = (2/60)))

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



