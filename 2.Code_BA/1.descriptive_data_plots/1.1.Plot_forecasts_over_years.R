

library(readxl)
library(ggplot2)
library(scales)
library(magrittr) #for pipes 
library(dplyr)

#load data and eliminate naive prediction rows 
prediction_data_transformed <- as.data.frame(read_excel("prediction_data_transformed.xlsx"))
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(!grepl('Unconditional prediction', INSTITUTION))


#udpate lags to reference point 31.12.2020 
for (i in 1:nrow(prediction_data_transformed)){
  prediction_data_transformed$LAG_in_months[i] <- prediction_data_transformed$LAG_in_months[i] -
    (2020 - as.numeric(unlist(strsplit(prediction_data_transformed$PREDICTED_YEAR[i], split='_', fixed=TRUE))[2]))*12 
}

#delete all years prior to 2008, Gemeinschaftsdiagnose startet forecasting in 2008 
prediction_data_transformed <- prediction_data_transformed %>% 
  filter(grepl(("Summe_2008|Summe_2009|Summe_2010|Summe_2011|Summe_2012|Summe_2013|Summe_2014|
                Summe_2015|Summe_2016|Summe_2017|Summe_2018|Summe_2019|Summe_2020"), 
               PREDICTED_YEAR))

#define breaks and tags for x-axis 
breaksplot <- c(-6 * (seq(1:30)-1))
labelsplot <- rev(c(
   "30.Jun.2006", "31.Dec.2006",
  "30.Jun.2007", "31.Dec.2007", "30.Jun.2008", "31.Dec.2008",
  "30.Jun.2009", "31.Dec.2009", "30.Jun.2010", "31.Dec.2010",
  "30.Jun.2011", "31.Dec.2011", "30.Jun.2012", "31.Dec.2012",
  "30.Jun.2013", "31.Dec.2013", "30.Jun.2014", "31.Dec.2014",
  "30.Jun.2015", "31.Dec.2015", "30.Jun.2016", "31.Dec.2016",
  "30.Jun.2017", "31.Dec.2017", "30.Jun.2018", "31.Dec.2018",
  "30.Jun.2019", "31.Dec.2019", "30.Jun.2020", "31.Dec.2020"))

#define colors for forecasts 

col <- c("coral3","blue3", "chartreuse4")
col <- rep(col, times = 10)


#create plot 
plot <- ggplot(prediction_data_transformed, aes(x = LAG_in_months, y = PREDICTION, shape = INSTITUTION, color = PREDICTED_YEAR)) +
  geom_point(size = 1.5) +
  labs(x = "Time", y = "GDP growth forecast") + 
  #labs(title = "GDP forecasts for Germany over time") +
  scale_x_continuous(breaks = breaksplot, labels = labelsplot) +
  theme(axis.text.x = element_text(size=06, angle=45)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_shape_manual(breaks = c("Statistisches Bundesamt", "EU Commission", "Gemeinschaftsdiagnose", "Ifo Munich"),
                     values = c(8, 15, 16, 17)) +
  scale_color_manual(values = col) +
  geom_line(data = prediction_data_transformed, aes(x = LAG_in_months, y = PREDICTION, color = PREDICTED_YEAR), inherit.aes = FALSE) +
  guides(color = "none", size = "none") + #remove color and size from legend 
  theme(legend.position = "bottom") +
  geom_hline(yintercept=0, linetype = "dashed", color = "grey", size = 0.2)

plot