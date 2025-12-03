########################
# Statistical analysis #
########################

# library ----
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)
library(desplot)
library(tidyr)
library(statgenGxE)
library(statgenSTA)
library(metan)

# open data ----

data <- read.csv(file = "data/data_IAVAO_Mil_network_wide.csv")

# selection of meta variables traits with enough information

data <- data %>% select(Env, macro_env, location, country, lat, lon, year, rep,
                        geno, CSF50p, GrainYield, PlHt, PanLen, PanDia, PGr,
                        BiomYield)

# quality check of the data: trial heritability ----

# set as missing the trait value of the trial for which the heritability is
# lower than 0.2

h2_thre <- 0.2

traits <- c("CSF50p", "GrainYield", "PlHt", "PanLen",
            "PanDia", "PGr", "BiomYield")

traits <- c("GrainYield", "PlHt", "PanLen",
            "PanDia", "PGr", "BiomYield")

n_traits <- length(traits)

env_id <- unique(data$Env)
n_env <- length(env_id)

h2_res <- matrix(NA, nrow = n_env, ncol = n_traits)

for(i in 1:n_traits){
  for(j in 1:n_env){
    data_ij <- data[data$Env == env_id[j], ]
    form_ij <- paste0(traits[i], " ~ rep + (1|geno)")
    m_ij <- tryCatch(lmer(formula = as.formula(form_ij), data = data_ij),
                     error = function(e) NULL)
    
    # h2
    if(!is.null(m_ij)){
      remat <- summary(m_ij)
      Verr <- remat$sigma^2
      V_var <- remat$varcor
      Vg <- V_var$geno[1, 1]
      n_rep <- length(unique(data_ij$rep))
      h2 <- Vg/(Vg + (Verr/n_rep))
      h2_res[j, i] <- h2
      
      if(h2 < h2_thre){ data[data$Env == env_id[j],
                             which(colnames(data) == traits[i])] <- NA}
      
    } else {
      data[data$Env == env_id[j], which(colnames(data) == traits[i])] <- NA
    }
    
  }
}

rownames(h2_res) <- env_id
colnames(h2_res) <- traits

# select a trait ----
trait_sel <- "GrainYield"
# single environment analysis: subset (Burkina Faso) ----

# consideration of the countries as the single (mega) environment composing
# the TPE (whole network)

env_sg <- unique(data$country)
table(data$country)

# analysis of Burkina faso

data_s <- data %>% filter(country == env_sg[2])

# remove the genotype with not enough replication ----

# remove the observation on genotypes that are not observed in at least
# n environment
n_env_min <- 3

geno_freq <- data_s %>%  group_by(geno, Env) %>% 
  summarise(freq = sum(!is.na(x = .data[[trait_sel]]))) %>% pivot_wider(
    id_cols = geno,
    names_from = Env,  # Column whose values become new column names
    values_from = freq     # Column whose values fill the new columns
  ) %>% as.data.frame()

rownames(geno_freq) <- geno_freq$geno
geno_freq <- geno_freq %>% select(-geno)

geno_env_rep <- apply(X = geno_freq, MARGIN = 1,
                      FUN = function(x) sum(x[!is.na(x)] > 0))

geno_sel <- names(geno_env_rep[geno_env_rep >= n_env_min])

data_s <- data_s %>% filter(geno %in% geno_sel)

# single environment analysis: network visualisation ----

geno_id_ref <- names(sort(table(data_s$geno), decreasing = TRUE))
d_plot <- data_s %>% arrange(year, location, geno)
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


p <- ggplot(d_plot, aes(Env, geno, fill= .data[[trait_sel]])) + 
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

# GGE and AMMI biplot ----
TD <- createTD(data = data_s, genotype = "geno", trial = "Env",
               loc = "location",
               repId = "rep", 
               trLat = "lat", 
               trLong = "lon")

# specify the design
meta <- getMeta(TD = TD)
meta$trDesign <- rep("rcbd", length(TD))
TD <- setMeta(TD = TD, meta = meta)

m_ST <- fitTD(TD = TD, traits = trait_sel, engine = "lme4")
# summary(m_ST)

plot(m_ST, 
     plotType = "base", 
     what = "random")

# prediction intra-env
BLUE <- extractSTA(STA = m_ST, what = "BLUEs")
BLUP <- extractSTA(STA = m_ST, what = "BLUPs")
colnames(BLUE)[3] <- colnames(BLUP)[3] <- "trait"

# GGE plot
gge_model <- gge(BLUE, trial, genotype, trait)
plot(gge_model)

# AMMI plot (does not work if data are too unbalanced)
TDGxE <- STAtoTD(STA = m_ST,
                 what = c("BLUEs", "seBLUEs",
                          "BLUPs", "seBLUPs"))
 
# AMMI <- gxeAmmi(TD = TDGxE, trait = paste0("BLUEs_", trait_sel))
# summary(AMMI) 
# plot(AMMI, plotType = "AMMI2", scale = 0.5)

# check the amount of missing values
BLUE_wide <- BLUE %>%
  tidyr::pivot_wider(
    id_cols = genotype,
    names_from = trial,
    values_from = trait) %>% as.data.frame()
rownames(BLUE_wide) <- BLUE_wide$genotype
BLUE_wide <- BLUE_wide[, -1]

sum(is.na(c(as.matrix(BLUE_wide))))/
  (nrow(BLUE_wide) * ncol(BLUE_wide))

# Global h2 ----

form <- paste0(trait_sel, " ~ (1|Env) + (1|Env:rep) + (1|geno) + (1|Env:geno)")
m <- lmer(formula = as.formula(form), data = data_s)
summary(m)

remat <- summary(m)
Verr <- remat$sigma^2
V_var <- remat$varcor
Vg <- V_var$geno[1, 1]
Vgxe <- V_var$`Env:geno`[1, 1]
d_used <- m@frame
n_env = length(unique(d_used$Env))
geno_rep <- d_used %>% group_by(Env, geno) %>% summarise(n_rep = n())
n_rep = mean(geno_rep$n_rep, na.rm = TRUE)
h2 <- (Vg)/(Vg + (Vgxe/n_env) + (Verr/(n_rep * n_env)))
h2

# Global BLUP ----
BLUP_geno <- ranef(m, whichel = "geno")
BLUP_geno <- BLUP_geno$geno

BLUP <- data.frame(geno = rownames(BLUP_geno),
                   BLUP = BLUP_geno[, 1])

# Global BLUE ----
form <- paste0(trait_sel, " ~ (1|Env) + (1|Env:rep) + geno + (1|Env:geno)")
m <- lmer(formula = as.formula(form), data = data_s)
summary(m)

m_sum <- summary(m)
coeff <- m_sum$coefficients

Int <- coeff[rownames(coeff) == '(Intercept)', 1]
BLUE <- coeff[grepl(pattern = "geno", x = rownames(coeff)), ]
geno_id <- gsub(pattern = "geno", replacement = "", x = rownames(BLUE))

BLUE <- data.frame(geno = geno_id, BLUE = BLUE[, 1] + Int)

# add the reference value
d_used <- m@frame
geno_ref <- levels(d_used$geno)[1]
stopifnot(!geno_ref %in% BLUE$geno)
BLUE <- rbind(data.frame(geno = geno_ref, BLUE = Int), BLUE)

d_adj_means <- merge(BLUP, BLUE, by = "geno")

# comparison BLUE et BLUP
plot(x = d_adj_means$BLUP, y = d_adj_means$BLUE)
cor(x = d_adj_means$BLUP, y = d_adj_means$BLUE)

# Visualise the adjusted means ----
d_adj_means %>%
  arrange(BLUE) %>%                     # sort from lowest to highest
  ggplot(aes(x = reorder(geno, BLUE),   # reorder factor levels
             y = BLUE)) +
  geom_col() +
  labs(
    x = "Genotype",
    y = "BLUE value",
    title = "Genotype BLUE Values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1)
  )


# Finlay-Wilkinson ----
FW <- gxeFw(TD = TDGxE, trait = paste0("BLUEs_", trait_sel))
summary(FW)
# plot(FW, plotType = "line") temporary problem to get the line plot
plot(FW, plotType = "trellis")
plot(FW, plotType = "scatter")

