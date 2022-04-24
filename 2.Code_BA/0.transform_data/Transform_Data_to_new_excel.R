##### Script takes data from handwritten Excel sheet and transforms it 
#into another excel sheet that is easier to work with 
####Usage of predefined "TransformData()" Function to transform one 
#column (i.e. one prediction horizon of one institution) of handwritten Excel sheet into new format 


library(readxl)
library(writexl)
library(ggplot2)
library(scales)

source("0.transform_data/Tranform_Data_function.R")

prediction_data <- as.data.frame(read_excel("Daten_10.11.2021.xlsx")) #read in handwritten excel sheet


#Ifo Munich predictions 
ifo_predictions_nowcast <- TransformData(
  prediction_data, 9, 21, 76, "End of December", 0, "Ifo Munich") #ifo nowcasts transformed into new format 
ifo_predictions_halfyear <- TransformData(
  prediction_data, 11, 21, 76, "End of June", -6, "Ifo Munich") #ifo halfyear forecasts transformed into new format 
ifo_predictions_oneyear <- TransformData(
  prediction_data, 13, 21, 76, "End of December", -12, "Ifo Munich") #ifo oneyear forecasts transformed into new format 
ifo_predictions_oneandhalfyear <- TransformData(
  prediction_data, 15, 21, 76, "End of June", -18, "Ifo Munich") #ifo oneandhalfyear forecasts transformed into new format 

ifo_predictions_total <- na.omit(rbind(
  ifo_predictions_nowcast, ifo_predictions_halfyear, ifo_predictions_oneyear, ifo_predictions_oneandhalfyear)) #all ifo forecasts bound together in one matrix by row

rm(ifo_predictions_nowcast, ifo_predictions_halfyear, ifo_predictions_oneyear, ifo_predictions_oneandhalfyear) #delete single matrices 


#EU Commission predictions 
#exact same procedure as for Ifo forecasts but more different horizons for EU forecasts
EU_predictions_1months <- TransformData(
  prediction_data, 19, 23, 62, "November", -1, "EU Commission")
EU_predictions_5months <- TransformData(
  prediction_data, 21, 23, 62, "July", -5, "EU Commission")
EU_predictions_7months <- TransformData(
  prediction_data, 23, 23, 62, "May", -7, "EU Commission")
EU_predictions_10months <- TransformData(
  prediction_data, 25, 23, 62, "February", -10, "EU Commission")
EU_predictions_13months <- TransformData(
  prediction_data, 27, 23, 62, "November", -13, "EU Commission")
EU_predictions_17months <- TransformData(
  prediction_data, 29, 23, 62, "July", -17, "EU Commission")
EU_predictions_19months <- TransformData(
  prediction_data, 31, 23, 62, "May", -19, "EU Commission")
EU_predictions_22months <- TransformData(
  prediction_data, 33, 23, 62, "February", -22, "EU Commission")
EU_predictions_25months <- TransformData(
  prediction_data, 35, 23, 62, "November", -25, "EU Commission")

EU_predictions_total <- na.omit(rbind(
  EU_predictions_1months, EU_predictions_5months, EU_predictions_7months, EU_predictions_10months, 
  EU_predictions_13months, EU_predictions_17months, EU_predictions_19months, EU_predictions_22months, EU_predictions_25months))

rm(EU_predictions_1months, EU_predictions_5months, EU_predictions_7months, EU_predictions_10months, 
   EU_predictions_13months, EU_predictions_17months, EU_predictions_19months, EU_predictions_22months, EU_predictions_25months)



#Gemeinschaftsdiagnose predictions 
Gemeinschaftsdiagnose_predictions_2.5months <- TransformData(
  prediction_data, 37, 14, 125, "October", -2.5, "Gemeinschaftsdiagnose")
Gemeinschaftsdiagnose_predictions_8.5months <- TransformData(
  prediction_data, 39, 14, 125, "April", -8.5, "Gemeinschaftsdiagnose")
Gemeinschaftsdiagnose_predictions_14.5months <- TransformData(
  prediction_data, 41, 14, 125, "October", -14.5, "Gemeinschaftsdiagnose")
Gemeinschaftsdiagnose_predictions_20.5months <- TransformData(
  prediction_data, 43, 14, 125, "April", -20.5, "Gemeinschaftsdiagnose")

Gemeinschaftsdiagnose_predictions_total <- na.omit(rbind(
  Gemeinschaftsdiagnose_predictions_2.5months, Gemeinschaftsdiagnose_predictions_8.5months,
  Gemeinschaftsdiagnose_predictions_14.5months, Gemeinschaftsdiagnose_predictions_20.5months))

rm(Gemeinschaftsdiagnose_predictions_2.5months, Gemeinschaftsdiagnose_predictions_8.5months, 
   Gemeinschaftsdiagnose_predictions_14.5months, Gemeinschaftsdiagnose_predictions_20.5months)



#Naive prediction based on average growth of the last 10 years at the time of the prediction 
Naive_predictions_28months <- TransformData(prediction_data, 45, 17, 104, "August", -28, "Unconditional prediction")
Naive_predictions_28months <- na.omit(Naive_predictions_28months)


#Preliminary true data by Statistisches Bundesamt 
Statistisches_Bundesamt_prel_true <- TransformData(prediction_data, 5, 30, 13, "February", 2, "Statistisches Bundesamt")
Statistisches_Bundesamt_prel_true <- na.omit(Statistisches_Bundesamt_prel_true)


#merge data of three forecasting institutions 
all_predictions <- rbind(ifo_predictions_total, EU_predictions_total, 
                         Gemeinschaftsdiagnose_predictions_total, Naive_predictions_28months, 
                         Statistisches_Bundesamt_prel_true)




#create excel file and save it 
write_xlsx(all_predictions, "/Users/hendrikplett/Desktop/6. Bachelorarbeit/3. Code/prediction_data_transformed.xlsx")

