####################
# Network overview #
####################

# library ----
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)
library(desplot)
library(tidyr)

# ad-hoc function
f_mean <- function(x) mean(x, na.rm = TRUE)

# Load data ----
data <- read.csv(file = "data/data_IAVAO_Mil_network_wide.csv")
load(file = "output/d_trials.RData")

# yield spatial distribution ----
d_map <- map_data('world')

# Define West Africa region (approximate longitude and latitude bounds)
west_africa <- subset(d_map,
                      long >= -20 & long <= 20 &
                        lat >= -10 & lat <= 30)

set.seed(9784)
d_trials$lon_jit <- d_trials$lon + runif(n = nrow(d_trials), min = 0.3, max = 0.7)
d_trials$lat_jit <- d_trials$lat + runif(n = nrow(d_trials), min = 0.3, max = 0.7)


p <- ggplot(data = west_africa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'grey', color = 'black', size = 0.2) +
  geom_point(
    data = d_trials,  # Assurez-vous que c'est le bon data.frame
    aes(x = lon_jit, y = lat_jit, color = av_yield),  # Map color à av_yield
    inherit.aes = FALSE,
    alpha = 1,
    size = 1.2
  ) +
  scale_color_gradient(low = "brown", high = "green") +  # Échelle de couleur continue
  coord_equal(xlim = c(-20, 20), ylim = c(0, 30)) +
  theme_void()

# Afficher le graphique
print(p)

# environment quality as average yield ----
d_env_qual <- d_trials %>% filter(!is.na(av_yield)) %>% arrange(av_yield) %>%
  select(country, location, year, av_yield) %>% mutate(env = paste0(country, '_', location, "_", year))

d_env_qual_country <- d_env_qual %>% arrange(country, location)
d_env_qual_country$env <- factor(d_env_qual_country$env)

d_gassi_2023 <- data %>% filter(location == 'Gassi', year == 2023)

# Create the barplot
p <- ggplot(data = d_env_qual_country, aes(x = env, y = av_yield)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Single color for all bars
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 5),  # Rotate x-axis labels
    legend.position = "none"  # Remove the legend
  ) + ggtitle("Avergage Yield country-location-year") +
  labs(x = "Environment", y = "Average Yield")  # Optional: Add axis labels

print(p)

d_env_qual <- d_trials %>% filter(!is.na(av_yield)) %>% arrange(av_yield) %>%
  select(country, location, year, av_yield) %>%
  mutate(env = paste0(macro_env, "_",country, '_', location, "_", year))

# !!! Quelle différence entre SUDAN_Mali_Cinzana_2024 et SAHEL_Mali_Cinzana_2024 ?

d_env_qual$env <- factor(d_env_qual$env, levels = d_env_qual$env)

p <- ggplot(data = d_env_qual, aes(x = env, y = av_yield)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Single color for all bars
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 5),  # Rotate x-axis labels
    legend.position = "none"  # Remove the legend
  ) + ggtitle("Avergage Yield increasing") +
  labs(x = "Environment", y = "Average Yield")  # Optional: Add axis labels

print(p)

env_gradient <- d_env_qual$env

# table variety yield x environment ----

data <- data %>%
  mutate(env = paste0(macro_env, "_", country, '_', location, "_", year))

d_geno_env <- data %>% group_by(env, geno) %>%
  summarise(av_yield = f_mean(GrainYield))

# Calculate the mean yield for each geno x env combination
d_geno_env <- data %>%
  group_by(env, geno) %>%
  summarise(av_yield = mean(GrainYield, na.rm = TRUE), .groups = "drop")

# Pivot to a wide format (two-way table)
geno_env_2way <- d_geno_env %>%
  pivot_wider(
    names_from = geno,
    values_from = av_yield,
    values_fill = NA  # Ensures all geno x env combinations are included
  ) %>% as.data.frame()

rownames(geno_env_2way) <- geno_env_2way$env
geno_env_2way <- geno_env_2way[, -1]

geno_n_obs <- apply(geno_env_2way, MARGIN = 2, FUN = function(x) sum(!is.na(x)))

summary(geno_n_obs)

geno_n_obs_sorted <- sort(geno_n_obs, decreasing = TRUE)

summary(geno_n_obs)

# Convert to a data frame for ggplot
df <- tibble(
  geno = names(geno_n_obs),
  n_obs = as.numeric(geno_n_obs)
)

# Sort the data frame in decreasing order
df <- df[order(-df$n_obs), ]

# Create the barplot
p <- ggplot(df, aes(x = reorder(geno, n_obs), y = n_obs, fill = geno)) +
  geom_bar(stat = "identity") +
  labs(x = "Genotype", y = "Number of Observations",
       title = "Repetition per genotype") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 5),  # Rotate x-axis labels
    legend.key.size = unit(1, "cm"),  # Adjust legend key size
    legend.text = element_text(size = 10), # Adjust legend text size
    legend.position = "none" 
  )
p

# trellis plot of yield (geno x env) ----

# remove the irrelevant observations
geno_env_2way <- geno_env_2way[as.character(env_gradient), ]

env_av_yield <- d_env_qual$av_yield

env_gradient_label <- as.character(env_gradient)
act_val <- env_av_yield[1]

for(i in 2:length(env_gradient_label)){
  
  diff_i <- env_av_yield[i] - act_val
  
  if(diff_i > 50){
    act_val <- env_av_yield[i]
  } else{
    env_gradient_label[i] <- ""
  }
  
}

for(i in 1:ncol(geno_env_2way)){
  
  d_i <- data.frame(env = env_gradient_label,
                    yield = geno_env_2way[, i],
                    env_av_yield)
  
  p <- ggplot(d_i, aes(x = env_av_yield, y = yield)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    ggtitle(colnames(geno_env_2way)[i]) +
    scale_x_continuous(
      breaks = d_i$env_av_yield,
      labels = d_i$env
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
  print(p)
  
}