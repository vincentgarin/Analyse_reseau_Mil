##################################
# Network descriptive statistics #
##################################

# library ----
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)
library(desplot)
library(tidyr)

# open data ----

data <- read.csv(file = "data/data_IAVAO_Mil_network_wide.csv")
d_locations <- read.csv(file = "data/location_liste.csv", sep = ";")

data_sahel <- data %>% filter(macro_env == "SAHEL")
data_sudan <- data %>% filter(macro_env == "SUDAN")
data_18_20 <- data %>% filter(year %in% c(2018:2020))

# explore the characteristics of the network ----

# unique genotypes
geno_list <- unique(data$geno)
geno_freq <- table(data$geno)
barplot(height = sort(geno_freq, decreasing = TRUE))

# environment: location
loc_list <- unique(data$location)
loc_list

# define local environments
table(d_locations$COUNTRY)


# filter the environment given a certain heritability threshold

# geographical map ----

d_map <- map_data('world')

# Define West Africa region (approximate longitude and latitude bounds)
west_africa <- subset(d_map,
                      long >= -20 & long <= 20 &
                        lat >= -10 & lat <= 30)

# Your locations data (replace with your actual data)
d_lat_lon <- data.frame(lat = d_locations$LATITUDE,
                        lon = d_locations$LONGITUDE)

# Plot with country borders
p <- ggplot(data = west_africa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'grey', color = 'black', size = 0.2) +
  geom_point(data = d_lat_lon, aes(x = lon, y = lat), inherit.aes = FALSE,
             alpha = 1, size = 1, col = 'red') +
  coord_equal(xlim = c(-20, 20), ylim = c(0, 30)) +
  theme_void()

# Display the plot
print(p)


# visualisation of network (heatmap) ----

traits <- colnames(data)[14:ncol(data)]

# geno_freq <- table(data$geno)
# geno_id_ref <- names(sort(geno_freq, decreasing = TRUE))
geno_id_ref <- unique(data$geno)


d_plot <- data %>% arrange(year, location, geno)
d_plot$Env <- factor(d_plot$Env, levels = unique(d_plot$Env))
d_plot$geno <- factor(d_plot$geno, levels = geno_id_ref)

text_size <- 12

# Calculate x-positions for vertical lines where Year changes
d_Experiment <- unique(d_plot[, c("Env", "location", "year")])
colnames(d_Experiment) <- c("Env", "Loc", "Year")
year_changes <- cumsum(table(d_Experiment$Year))

Experiment_counts <- table(d_Experiment$Year)
year_boundaries <- c(0, cumsum(Experiment_counts)) # Add 0 to start
year_midpoints <- (year_boundaries[-1] + year_boundaries[-length(year_boundaries)]) / 2
year_labels <- names(Experiment_counts)



p <- ggplot(d_plot, aes(Env, geno, fill= GrainYield)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "darkgreen") +
  
  geom_vline(xintercept = year_changes, color = "red", linetype = "dashed",
             linewidth = 0.1) +
  
  scale_x_discrete(
    breaks = levels(d_Experiment$Env)[round(year_midpoints)],  # Add custom breaks
    labels = year_labels) +
  
  labs(x = "Environment", y = "Genotype", fill = "trait") +
  theme(axis.text.y  = element_blank(),
        axis.title.x = element_text(size=text_size+2),
        axis.title.y = element_text(size=text_size+2),
        axis.text.x = element_text(size = text_size-4, angle = 80, vjust = 0.5),
        legend.title = element_text(size=(text_size+2)),
        legend.text = element_text(size=(text_size)))

p

# loop over the different traits ----

# Loop over each trait and create a plot
for (trait in traits) {
  p <- ggplot(d_plot, aes(Env, geno, fill = .data[[trait]])) +
    geom_tile() +
    scale_fill_gradient(low = "yellow", high = "darkgreen") +
    geom_vline(xintercept = year_changes, color = "red", linetype = "dashed", linewidth = 0.1) +
    scale_x_discrete(
      breaks = levels(d_Experiment$Env)[round(year_midpoints)],
      labels = year_labels
    ) + ggtitle(trait) +
    labs(x = "Environment", y = "Genotype", fill = trait) +
    theme(
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = text_size + 2),
      axis.title.y = element_text(size = text_size + 2),
      axis.text.x = element_text(size = text_size - 4, angle = 80, vjust = 0.5),
      legend.title = element_text(size = text_size + 2),
      legend.text = element_text(size = text_size)
    )
  
  # Print or save the plot
  print(p)
  
  # Optionally, save the plot with a unique name
  # ggsave(paste0("plot_", trait, ".png"), plot = p)
}

# Open a PDF device
pdf("plot/IAVAO_MIL_network_trait_overview.pdf", width = 10, height = 8)  # Adjust width and height as needed

# Loop over each trait and create a plot
for (trait in traits) {
  p <- ggplot(d_plot, aes(Env, geno, fill = .data[[trait]])) +
    geom_tile() +
    scale_fill_gradient(low = "yellow", high = "darkgreen") +
    geom_vline(xintercept = year_changes, color = "red", linetype = "dashed", linewidth = 0.1) +
    scale_x_discrete(
      breaks = levels(d_Experiment$Env)[round(year_midpoints)],
      labels = year_labels
    ) +
    ggtitle(trait) +
    labs(x = "Environment", y = "Genotype", fill = trait) +
    theme(
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = text_size + 2),
      axis.title.y = element_text(size = text_size + 2),
      axis.text.x = element_text(size = text_size - 4, angle = 80, vjust = 0.5),
      legend.title = element_text(size = text_size + 2),
      legend.text = element_text(size = text_size)
    )
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()

