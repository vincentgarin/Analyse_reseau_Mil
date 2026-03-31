####################################
# Date et densité moyenne par site #
####################################

library(dplyr)

# load the sheet manually
meta_data <- read.csv(file = "data/Meta_data_trials.xlsx - Meta_data_trials.csv")
colnames(meta_data) <- meta_data[1, ]
meta_data <- meta_data[-1, ]

meta_data$PLANT_DENSITY <- as.numeric(meta_data$PLANT_DENSITY)

d_densite <- meta_data %>% group_by(LocationName) %>%
  summarise(av_density = mean(PLANT_DENSITY, na.rm = TRUE))