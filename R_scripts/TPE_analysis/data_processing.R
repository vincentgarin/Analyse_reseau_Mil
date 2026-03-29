###################
# Data processing #
###################

# library ----
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)
library(desplot)
library(tidyr)

# ad-hoc function ----
f_mean <- function(x) mean(x, na.rm = TRUE)

# open data ----

data <- read.csv(file = "data/all_trials.csv")
# loc_id <- unique(data$locationDbId)

# process data ----

# select variables
data <- data %>% select(plotNumber, replicate, germplasmName, studyDbId,
                        studyName, locationDbId, location_name, entryType,
                        observationVariableName,
                        observationValue) %>%
  rename(plot_no = plotNumber, rep = replicate, geno = germplasmName,
         location_id = locationDbId, location = location_name,
         var_name = observationVariableName, value = observationValue)

# add year variable
data$year <- substr(data$studyName, 6, 9)

# add macro environment
data$macro_env <- NA
data$macro_env[grepl(x = data$studyName, pattern = "SAHEL")] <- "SAHEL"
data$macro_env[grepl(x = data$studyName, pattern = "SUDAN")] <- "SUDAN"
macro_env_id <- data$macro_env
macro_env_id[is.na(macro_env_id)] <- ""
data$Env <- paste0(macro_env_id, "_",data$location, "_", data$year)
data$Env[macro_env_id == ""] <- substr(data$Env[macro_env_id == ""], 2,
                                       nchar(data$Env[macro_env_id == ""]))

data$plot_id <- paste0(data$studyDbId, '_', data$plot_no)

# re-order the variables
data <- data %>% select(studyDbId, Env, macro_env, location_id, location, year,
                        rep, plot_id, plot_no, geno, var_name, value)

# Assuming your data.frame is named 'df'
data_wider <- data %>%
  pivot_wider(
    id_cols = c(studyDbId, Env, macro_env, location, year, rep, location_id, location, plot_id, plot_no, geno),      # Unique identifier for rows
    names_from = var_name,  # Column whose values become new column names
    values_from = value     # Column whose values fill the new columns
  ) %>% arrange(year)

# add the location countries, lat and lon
d_locations <- read.csv(file = "data/location_liste.csv", sep = ";")
d_locations <- d_locations %>% select(NAME, LOCATION.ID, COUNTRY, LATITUDE,
                                      LONGITUDE) %>%
  rename(location_id = LOCATION.ID, country = COUNTRY, lat = LATITUDE,
         lon = LONGITUDE)

data <- merge(x = data, y = d_locations[, 2:ncol(d_locations)],
              by = 'location_id', all.x = "TRUE")
data <- data %>% relocate(country, lat, lon, .after = location)
data <- data %>% relocate(location_id, .after = macro_env)

data_wider <- merge(x = data_wider, y = d_locations[, 2:ncol(d_locations)],
              by = 'location_id', all.x = "TRUE")
data_wider <- data_wider %>% relocate(country, lat, lon, .after = location)
data_wider <- data_wider %>% relocate(location_id, .after = macro_env)

# save data ----
write.csv(data, file = "data/data_IAVAO_Mil_network_long.csv",
          row.names = FALSE)

write.csv(data_wider, file = "data/data_IAVAO_Mil_network_wide.csv",
          row.names = FALSE)

# trial data summary (including average yield) ----
data <- read.csv(file = "data/data_IAVAO_Mil_network_wide.csv")

d_trials <- data %>% group_by(macro_env, location, year) %>%
  summarise(country = unique(country),
            lat = unique(lat),
            lon = unique(lon),
            year = unique(year),
            av_yield = f_mean(GrainYield)) %>%
  mutate(env = paste0(macro_env, "_", country, '_', location, "_", year)) %>% 
  relocate(env, .before = macro_env)

save(d_trials, file = "output/d_trials.RData")