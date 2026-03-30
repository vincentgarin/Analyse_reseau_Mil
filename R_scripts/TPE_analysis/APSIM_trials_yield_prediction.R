############################################
# IAVAO Millet network APSIM grid analysis #
############################################

# library ----

library(dplyr)
library(stringr)
library(apsimx)
library(ggplot2)
library(googlesheets4)
library(data.table)
library(xml2)

# ad-hoc functions ----
f_fread <- function(x) fread(x, skip = 3)

mdf_met_file <- function(apsim_file, met_file){
  
  # Parse the XML
  doc <- read_xml(apsim_file)
  
  # Find the <filename> node inside <metfile>
  filename_node <- xml_find_first(doc, "//metfile[@name='met']/filename")
  
  # Replace the text content with the new .met path
  xml_text(filename_node) <- met_file
  
  # Build a unique output filename based on the .met file name
  met_name   <- tools::file_path_sans_ext(basename(met_file))  # e.g. "Bambey_2023"
  output_apsim <- paste0("APSIM_files/", met_name, "_millet.apsim")
  
  # Save the modified XML
  write_xml(doc, output_apsim)
  
  return(output_apsim)
  
}

mdf_clock <- function(apsim_file, start_date, end_date) {
  
  # Parse the XML
  doc <- read_xml(apsim_file)
  
  # Find the <start_date> and <end_date> nodes
  start_node <- xml_find_first(doc, "//start_date")
  end_node   <- xml_find_first(doc, "//end_date")
  
  # Replace the text content with the new dates
  xml_text(start_node) <- start_date
  xml_text(end_node)   <- end_date
  
  # Overwrite the same file (already named after the met file)
  write_xml(doc, apsim_file)
  
  return(apsim_file)
}

# load data ----

# get the trial data
load(file = "output/d_trials.RData")

# get trial meta data
# sheet_url <- "https://docs.google.com/spreadsheets/d/1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g/edit?usp=drive_link&ouid=112199760743851921328&rtpof=true&sd=true"
# sheet_url <- "https://docs.google.com/spreadsheets/d/1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g/edit?gid=981267538#gid=981267538"
# sheet_url <- "https://docs.google.com/spreadsheets/d/1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g/edit#gid=981267538"
sheet_url <- "https://docs.google.com/spreadsheets/d/1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g"

# Read the data from the Google Sheet
meta_data <- read_sheet(sheet_url)
meta_data <- read_sheet("1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g", sheet = 1)

# load the sheet manually
meta_data <- read.csv(file = "data/Meta_data_trials.xlsx - Meta_data_trials.csv")
colnames(meta_data) <- meta_data[1, ]
meta_data <- meta_data[-1, ]

# recreate the trial identifiers
# Macro_env, country, location, year

Macro_env <- substr(x = meta_data$TrialName, 19, nchar(meta_data$TrialName))
Macro_env <- gsub(pattern = "_", replacement = "", x = Macro_env)
Macro_env <- gsub(pattern = "^$", replacement = "NA", x = Macro_env)

Macro_env$env <- mapply(
  function(m, c, l, y) paste0(m, "_", c, "_", l, "_", y),
  Macro_env,
  meta_data$Country,
  meta_data$LocationName,
  meta_data$year
)

Macro_env$env <- gsub(pattern = "é", replacement = "e", x = Macro_env$env)

# Parameters ----

# Define your vector of .met files
root <- "D:/Mes Donnees/WD/Teaching/Analyse_reseau_Mil/data/weather/met_files/"

met_files <- paste0(root, d_trials$met_file)

# temporary subset
d_trials <- d_trials[1:2, ]

n_trials <- nrow(d_trials)

# list of parameters of interest

# soil parameters: need to make sure that it gets the initial water data

# parameters:

# Soil file: replace a complete chunk, ...

# soil texture
# soil depth
# soil water content

# Fertilisation

# Single value arguments (easy)

# Sowing date
# Sowing density
# Cultivar
# [irrigation]

# model <- c('millet', 'PearlMillet')
# soil_depth <- c('deep', 'medium', 'shallow')
# soil_text <- c('clay', 'laom', 'sand')
# soil <- expand.grid(soil_depth, soil_text)
# soil <- paste0(soil[, 2], '_', soil[, 1])
# sow_d <- c('early', 'average', 'late')
# density <- c("12", "18", "24")
# variety <- c('hhb67', 'wrajpop')
# variety_new <- c('HHB67', 'wrajpop', 'PM9444')
# irrig <- c('off', 'int', 'on')
# fert <- c('f_0', 'f_30', 'f_50')

# corresponding factorial in apsim file

# get the filenames that will be generated

# APSIM configuration ----

# APSIM CM ingredients
APSIM_exe <- "\"C:/Program Files (x86)/APSIM710-r4221/Model/Apsim.exe\""
apsim_file <- "APSIM_files/old/millet.apsim"
# apsim_file <- "APSIM_files/new/PearlMillet.apsim"

CMD_i <- paste(APSIM_exe, apsim_file)

# simulation loop ----

# res_fold <- 'D:/Mes Donnees/WD/ICRISAT/Pearl_Millet/Results/p_opt_all_dist'
res_yield <- rep(NA, n_trials)

t1 <- Sys.time()

for(i in 1:n_trials){
  
  # modification of the APSIM file given the arguments of each trial
  file_i <- mdf_met_file(apsim_file = apsim_file, met_file = met_files[i])
  
  # modification of the APSIM file clock arguments
  year_i <- d_trials$year[i]
  file_i <- mdf_clock(apsim_file = file_i,
                      start_date = paste0('01/01/', year_i),
                      end_date = paste0('31/12/', year_i))
  
  # Execute the APSIM file
  CMD_i <- paste(APSIM_exe, file_i)
  
  # execute the simulation
  sim_check <- system(command = CMD_i, show.output.on.console = FALSE)
  
  if(sim_check == 0){
    
    # get the data
    DT <- fread("APSIM_files/Millet.out")
    col_names <- DT[1, ]
    colnames(DT) <- unlist(col_names)
    DT <- DT[-c(1:2), ]
    
    # modify the column name of old simulation
    colnames(DT)[colnames(DT) == 'millet_yield'] <- "Yield"
    colnames(DT)[colnames(DT) == 'paddock.millet.stage'] <- "Stage"
    
    res_yield[i] <- as.numeric(DT$Yield[as.numeric(DT$Stage) == 11])
    
    # print result
    cat("Yield", paste0("(",d_trials$location[i], '-', d_trials$year[i], ")"),
        ":", res_yield[i])
    cat("\n")
    
  }
  
  
  # # modification of the .met (weather) file
  # met_i <- file.path(met_file_loc, d_trials$met_file[i])
  # year_i <- c(paste0('01/01/', ), paste0('31/12/', d_trials$year[i]))
  # 
  # apsimr::edit_apsim(file = "millet.apsim", wd = apsim_d_old, var = A_var_loc,
  #                    value = met_i, overwrite = TRUE)
  # 
  # apsimr::edit_apsim(file = "millet.apsim", wd = apsim_d_old, var = A_var_clk,
  #                    value = year_i, overwrite = TRUE)
  # 
  # # execute the simulation
  # sim_check <- system(command = CMD_i, show.output.on.console = FALSE)
  # 
  # if(sim_check == 0){
  #   
  #   # get the data
  #   DT <- fread("APSIM_files/old/Millet.out")
  #   col_names <- DT[1, ]
  #   colnames(DT) <- unlist(col_names)
  #   DT <- DT[-c(1:2), ]
  #   
  #   # get yield
  #   res_yield[i] <- DT$millet_yield[as.numeric(DT$paddock.millet.stage) == 11]
  #   
  #   # print result
  #   cat("Yield", paste0("(",d_trials$location[i], '-', d_trials$year[i], ")"),
  #       ":", res_yield[i])
  #   
  # }
  
}

t2 <- Sys.time()

t_diff <- t2 - t1
t_diff