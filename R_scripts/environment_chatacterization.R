##################################
# Environmental characterization #
##################################

# library ----
library(nasapower)
library(stringr)
library(EnvRtype)
library(dplyr)
library(ggplot2)

# ad-hoc functions ----
source(file = "functions/utils_ggbiplot_mod.R")


# load data ----

d_locations <- read.csv(file = "data/location_liste.csv", sep = ";")

# get and process the environmental covariates ----
# 
# Can immediately start fro the saved data
# 
# EC_nm <- c("rain", "rel_hum", "spe_hum", "VPD", "SPV", "ETP", "PETP",
#            "T_av", "T_min", "T_max", "T_range", "cum_deg_day", "FRUE",
#            "photoperiod", "h_sun", "sol_radiation")
# 
# n_EC <- length(EC_nm)
# 
# var_sum <- c("rain", "cum_deg_day", "h_sun", "sol_radiation")
# var_ave <- c("rel_hum", "spe_hum", "VPD", "SPV", "ETP", "PETP",
#              "T_av", "T_min", "T_max", "T_range", "FRUE", "photoperiod")
# 
# n_loc <- nrow(d_locations)
# 
# years <- 2018:2024
# # 1st june - 1st sept
# time_frame <- 152:244
# 
# d_env_EC <- matrix(NA, nrow = n_loc, ncol = n_EC)
# 
# for(i in 1:n_loc){
#   
#   st_date <- paste0(years[1], '-01-01')
#   end_date <- paste0(years[length(years)], '-12-31')
#   
#   # complement with nasapower data
#   d_nasa <- get_weather(env.id = d_locations$NAME[i],
#                         lat = d_locations$LATITUDE[i],
#                         lon = d_locations$LONGITUDE[i],
#                         start.day = st_date,
#                         end.day = end_date)
#   
#   # get extra parameters:
#   d_wth <- processWTH(env.data = d_nasa)
#   
#   d_meta <- d_wth[, 1:6]
#   
#   d_EC <- d_wth %>% dplyr::select(PRECTOT, RH2M, QV2M, VPD, SPV, ETP, PETP,
#                            T2M, T2M_MIN, T2M_MAX, T2M_RANGE, GDD, FRUE,
#                            N, n, RTA)
#   
#   colnames(d_EC) <- EC_nm
#   
#   d_wth <- cbind(d_meta, d_EC)
#   d_wth$year <- substr(d_wth$YYYYMMDD, start = 1, stop = 4)
#   
#   # subset the time frame
#   d_wth <- d_wth[d_wth$DOY %in% time_frame, ]
#   
#   # average/agregate the variable per years
#   d_EC_ave <- d_wth %>% group_by(year) %>%
#     summarise(across(all_of(var_ave), mean))
#   
#   d_EC_sum <- d_wth %>% group_by(year) %>%
#     summarise(across(all_of(var_sum), sum))
#   
#   EC_av <- colMeans(cbind(d_EC_ave[, -1], d_EC_sum[, -1]))
#   
#   d_env_EC[i, ] <- EC_av
#   
# }
# 
# d_env_meta <- d_locations %>% dplyr::select(NAME, COUNTRY, LATITUDE, LONGITUDE) %>% 
#   dplyr::rename(env = NAME, country = COUNTRY, lat = LATITUDE, lon = LONGITUDE)
# 
# d_loc_EC <- data.frame(d_env_meta, d_env_EC) 
# colnames(d_loc_EC)[5:ncol(d_loc_EC)] <- names(EC_av)

# save data ----
# save(d_loc_EC, file = "data/location_environmental_covariates.RData")

# Make a PCA ----

load(file = "data/location_environmental_covariates.RData")

set.seed(597448)

data_PCA <- d_loc_EC
rownames(data_PCA) <- data_PCA$env
data_PCA <- data_PCA[, -c(1:4)]

defaut_theme <- theme(title = element_text(size = 20),
                      axis.title = element_text(size = 18),
                      axis.text = element_text(size = 16),
                      legend.title = element_text(size = 17, face = 'bold'),
                      legend.text = element_text(size = 16))

PCA <- prcomp(data_PCA, scale. = TRUE)

PC_plot <- ggbiplot_mod(PCA, x_lim = c(-2.5, 2.5), title = 'PC biplot', var.axes = TRUE,
                        dot_size = 1, arrow_size = 0.5) + defaut_theme

PC_plot

# PCA axis interpretation ----

# PC1 plot
loading <- PCA$rotation[, 1:2]
ref_var <- rownames(loading)

d <- data.frame(var = rownames(loading), load = loading[, 1, drop = FALSE])
colnames(d)[2] <- 'loading'
d$var <- factor(x = d$var, levels = rev(ref_var))

p <- ggplot(d, aes(x = loading, y = var)) + geom_bar(stat = "identity") +
  ggtitle('plot PC1 loadings')
p

# PC2 plot
d <- data.frame(var = rownames(loading), load = loading[, 2, drop = FALSE])
colnames(d)[2] <- 'loading'
d$var <- factor(x = d$var, levels = rev(ref_var))

p <- ggplot(d, aes(x = loading, y = var)) + geom_bar(stat = "identity") +
  ggtitle('plot PC2 loadings')
p

# PCA clustering ----
# 3 clusters
n_clu <- 3
comp <- data.frame(PCA$x[, 1:3])
k <- kmeans(comp, n_clu, nstart=25, iter.max=1000)

# Set the cluster order
cl_nb <- k$cluster

# order the cluster given rel humidity
d_loc_EC$E_cluster <- cl_nb

d_clust_rain <- d_loc_EC %>% dplyr::group_by(E_cluster) %>%
  dplyr::summarise(rain_av = mean(rain)) %>% dplyr::arrange(rain_av)

d_clust_rain$new_order <- 1:nrow(d_clust_rain)
d_clust_rain$col <- c('red', 'yellow', 'green')

d_clust_rain <- d_clust_rain %>%  dplyr::arrange(E_cluster)


# expand to location data.frame
d_loc_EC$E_cluster <- d_clust_rain$new_order[cl_nb]
d_loc_EC$E_color <- d_clust_rain$col[cl_nb]


# plot PCA with clusters ----
title_i <- paste0('PCA - ', n_clu, ' clusters')

PC_plot <- ggbiplot_mod(PCA, groups = d_loc_EC$E_color, x_lim = c(-2.5, 2.5),
                        title = title_i, var.axes = TRUE, dot_size = 3) +
  defaut_theme

PC_plot

# plot PCA clusters on map ----

# represent the cluster along the map
d_map <- map_data('world')

# Define West Africa region (approximate longitude and latitude bounds)
west_africa <- subset(d_map,
                      long >= -20 & long <= 20 &
                        lat >= -10 & lat <= 30)

# Plot with country borders
p <- ggplot(data = west_africa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'grey', color = 'black', size = 0.2) +
  geom_point(
    data = d_loc_EC,
    aes(x = lon, y = lat, color = E_color),  # Map color to E_cluster
    inherit.aes = FALSE,
    alpha = 1,
    size = 2.5
  ) +
  scale_color_identity() +
  coord_equal(xlim = c(-20, 20), ylim = c(0, 30)) +
  theme_void()
# Display the plot
print(p)

# add the identified clusters to the global dataset ----
zone_lk <- c("Sudanian", "Sudano-Sahelian", "Sahelian")
names(zone_lk) <- c("green", "yellow", "red")

d_loc_EC$env_zone <- zone_lk[d_loc_EC$E_color]
d_loc_EC$env_zone <- factor(d_loc_EC$env_zone, levels = c("Sudanian", "Sudano-Sahelian", "Sahelian"))


data <- read.csv(file = "data/data_IAVAO_Mil_network_wide.csv")

all(unique(data$location) %in% d_loc_EC$env)

env_zone_lk <- as.character(d_loc_EC$env_zone)
names(env_zone_lk) <- d_loc_EC$env

data$env_zone <- env_zone_lk[data$location]

data <- data %>% relocate(env_zone, .after = macro_env)
data$env_zone <- factor(data$env_zone, levels = c("Sudanian", "Sudano-Sahelian", "Sahelian"))


# yield estimate per zone
d_yield_zone_loc <- data %>%
  dplyr::group_by(env_zone, location, country) %>%
  dplyr::summarise(yield_av = mean(GrainYield, na.rm = TRUE)) %>%
  dplyr::arrange(env_zone, desc(yield_av))

d_yield_zone <- data %>% dplyr::group_by(env_zone) %>%
  dplyr::summarise(yield_av = mean(GrainYield, na.rm = TRUE))

write.csv(d_yield_zone_loc, file = "results/grain_yield_zone_location.csv",
          row.names = FALSE)

write.csv(d_yield_zone, file = "results/grain_yield_zone.csv",
          row.names = FALSE)

# distribution year zone for yield
data_yield <- data[!is.na(data$GrainYield), ]
table(data_yield$year, data_yield$env_zone)

data_yield_location <- unique(data_yield[, c("year", "env_zone", "location")])
table(data_yield_location$year, data_yield_location$env_zone)


