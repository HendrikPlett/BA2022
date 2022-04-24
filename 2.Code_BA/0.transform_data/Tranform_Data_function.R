#function to transform data from one column of the handwritten excel
#sheet into new format that is easier to work with in data analysis  


#Define function 
TransformData <- function (Excel_Data_as_data_frame, Column_with_predictions, 
                           Number_of_predictions, Start_Row, Publication_month, 
                           Lag_in_months, Publication_institution) {
  
  resultmatrice <- matrix(nrow = Number_of_predictions, ncol = 13) #define dimensions of new data format 
  colnames(resultmatrice) <- c("PREDICTED_YEAR", "LAG_in_months", "DATE_OF_PREDICTION", "PREDICTION",  
                               "INSTITUTION",  "PRELIMINARY_TRUE_GDP_after_two_months", "PREL_ERROR",
                               "PREL_ABSOLUTE_ERROR", "PREL_SQARED_ERROR", "FINAL_TRUE_GDP_after_four_years", 
                               "FIN_ERROR", "FIN_ABSOLUTE_ERROR", "FIN_SQARED_ERROR" ) #define names for the 13 dimensions of new data format
  
  
  #for calculating "DATE OF PREDICTION" with "LAG_IN_MONTHS" and "PREDICTED_YEAR" 
  yeardelay <- 0
  if (Lag_in_months <= -12) {
    yeardelay <- 1
  } 
  
  if (Lag_in_months <= -24) {
    yeardelay <- 2
  }
  
  if(Lag_in_months >= 1) {
    yeardelay <- -1 
  }
  
  
  #run through column nr. "Column with predictions" of handwritten excel sheet and storing data in "resultmatrice"
  for (i in 0 : (Number_of_predictions - 1)) {
    
    resultmatrice[i+1,1] <- Excel_Data_as_data_frame[Start_Row+7*i,1] #predicted year 
    resultmatrice[i+1,2] <- Lag_in_months #lag in months
    resultmatrice[i+1,3] <- paste(Publication_month, toString(as.numeric(unlist(strsplit(Excel_Data_as_data_frame[Start_Row+7*i,1], split='_', fixed=TRUE))[2]) - yeardelay)) #date of prediction
    resultmatrice[i+1,4] <- Excel_Data_as_data_frame[Start_Row+7*i, Column_with_predictions] #prediction  
    resultmatrice[i+1,5] <- Publication_institution #institution
    resultmatrice[i+1,6] <- Excel_Data_as_data_frame[Start_Row+7*i,5] #preliminary true gdp after two months 
    resultmatrice[i+1,7] <- as.numeric(resultmatrice[i+1,6]) - as.numeric(resultmatrice[i+1,4]) #preliminary error 
    resultmatrice[i+1,8] <- abs(as.numeric(resultmatrice[i+1,4]) - as.numeric(resultmatrice[i+1,6])) #preliminary absolute error
    resultmatrice[i+1,9] <- (as.numeric(resultmatrice[i+1,4]) - as.numeric(resultmatrice[i+1,6]))^2 #preliminary sqared error 
    resultmatrice[i+1,10] <- Excel_Data_as_data_frame[Start_Row+7*i,3] #final true gdp after four years 
    resultmatrice[i+1,11] <- as.numeric(resultmatrice[i+1,10]) - as.numeric(resultmatrice[i+1,4]) #final error
    resultmatrice[i+1,12] <- abs(as.numeric(resultmatrice[i+1,4]) - as.numeric(resultmatrice[i+1,10])) #final absolute error 
    resultmatrice[i+1,13] <- (as.numeric(resultmatrice[i+1,4]) - as.numeric(resultmatrice[i+1,10]))^2 #final squared error 

    # convert into data_frame and make all numbers numerical  
    resultmatrice <- as.data.frame(resultmatrice) #data frame 
    resultmatrice$LAG_in_months <- as.numeric(resultmatrice$LAG_in_months)
    resultmatrice$PREDICTION <- as.numeric(resultmatrice$PREDICTION)
    resultmatrice$PRELIMINARY_TRUE_GDP_after_two_months <- as.numeric(resultmatrice$PRELIMINARY_TRUE_GDP_after_two_months)
    resultmatrice$PREL_ERROR <- as.numeric(resultmatrice$PREL_ERROR)
    resultmatrice$PREL_ABSOLUTE_ERROR <- as.numeric(resultmatrice$PREL_ABSOLUTE_ERROR)
    resultmatrice$PREL_SQARED_ERROR <- as.numeric(resultmatrice$PREL_SQARED_ERROR)
    resultmatrice$FINAL_TRUE_GDP_after_four_years <- as.numeric(resultmatrice$FINAL_TRUE_GDP_after_four_years)
    resultmatrice$FIN_ERROR <- as.numeric(resultmatrice$FIN_ERROR)
    resultmatrice$FIN_ABSOLUTE_ERROR <- as.numeric(resultmatrice$FIN_ABSOLUTE_ERROR)
    resultmatrice$FIN_SQARED_ERROR <- as.numeric(resultmatrice$FIN_SQARED_ERROR)
    
    
  }
  
  #return transformed forecast data 
  return (resultmatrice)
  
  
}