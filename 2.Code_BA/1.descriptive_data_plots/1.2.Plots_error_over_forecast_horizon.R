
library(readxl)
library(ggplot2)
library(scales)
library(magrittr) #for pipes 
library(dplyr)


#read in transformed data from excel file and modify it 
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))

#define colors for plots
cols <- c("coral3", "blue3", "chartreuse4", "black", "grey")
          

#plot of preliminary Error against time with naive prediction 
plot_prel_error <- ggplot(prediction_data_transformed, aes(x = LAG_in_months, y = PREL_ERROR, color = INSTITUTION)) +
  geom_jitter(width = 0.15, size = 1) + 
  labs(x = "Forecast horizon in months", y = "Error in percentage points") + 
  #labs(title = "Forecasting error of real GDP growth in Germany") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(breaks = c("EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich", "Unconditional prediction", "Statistisches Bundesamt"), values=cols)


plot_prel_error

