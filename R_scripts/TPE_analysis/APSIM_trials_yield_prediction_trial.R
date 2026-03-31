############################################
# IAVAO Millet network APSIM grid analysis #
############################################

# library ----

library(dplyr)
library(stringr)
# library(apsimx)
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

# NEW FUNCTION: modify sowing date ----
mdf_sowing_date <- function(apsim_file, new_date) {
  
  doc <- read_xml(apsim_file)
  
  date_node <- xml_find_first(
    doc,
    "//date[@type='text' and contains(@description, 'sowing date')]"
  )
  
  if (is.na(date_node)) {
    stop("Sowing date node not found in XML.")
  }
  
  xml_text(date_node) <- new_date
  
  write_xml(doc, apsim_file)
  
  return(apsim_file)
}

# NEW FUNCTION: format sowing date ----
format_sowing_date <- function(date_char) {
  
  if (is.na(date_char) || date_char == "") return(NA)
  
  d <- as.Date(date_char, format = "%d/%m/%Y")
  
  format(d, "%d-%b") |> tolower()
}


# load data ----

# get the trial data
load(file = "output/d_trials.RData")

# get trial meta data
sheet_url <- "https://docs.google.com/spreadsheets/d/1NjLxFH_1C1xOkTMZpB9zUpKVYBPw4E3g"

# load the sheet manually
meta_data <- read.csv(file = "data/Meta_data_trials.xlsx - Meta_data_trials.csv")
colnames(meta_data) <- meta_data[1, ]
meta_data <- meta_data[-1, ]

# recreate the trial identifiers
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

root <- "C:/Adama/RESEAU_MIL/Analyse_reseau_Mil/data/weather/met_files/"
met_files <- paste0(root, d_trials$met_file)

# temporary subset
d_trials <- d_trials[1:2, ]

n_trials <- nrow(d_trials)

# APSIM configuration ----

APSIM_exe <- "\"C:/Program Files (x86)/APSIM710-r4221/Model/Apsim.exe\""
apsim_file <- "APSIM_files/old/millet.apsim"

CMD_i <- paste(APSIM_exe, apsim_file)

# simulation loop ----

res_yield <- rep(NA, n_trials)

t1 <- Sys.time()

for(i in 1:n_trials){
  
  file_i <- mdf_met_file(apsim_file = apsim_file, met_file = met_files[i])
  
  year_i <- d_trials$year[i]
  file_i <- mdf_clock(apsim_file = file_i,
                      start_date = paste0('01/01/', year_i),
                      end_date = paste0('31/12/', year_i))
  
  # NEW: apply sowing date from meta_data
  sowing_raw <- meta_data$SOWING_DATE[i]
  sowing_fmt <- format_sowing_date(sowing_raw)
  
  if(!is.na(sowing_fmt)){
    file_i <- mdf_sowing_date(file_i, sowing_fmt)
  }
  
  CMD_i <- paste(APSIM_exe, file_i)
  
  sim_check <- system(command = CMD_i, show.output.on.console = FALSE)
  
  if(sim_check == 0){
    
    DT <- fread("APSIM_files/Millet.out")
    col_names <- DT[1, ]
    colnames(DT) <- unlist(col_names)
    DT <- DT[-c(1:2), ]
    
    colnames(DT)[colnames(DT) == 'millet_yield'] <- "Yield"
    colnames(DT)[colnames(DT) == 'paddock.millet.stage'] <- "Stage"
    
    res_yield[i] <- as.numeric(DT$Yield[as.numeric(DT$Stage) == 11])
    
    cat("Yield", paste0("(",d_trials$location[i], '-', d_trials$year[i], ")"),
        ":", res_yield[i])
    cat("\n")
    
  }
}

t2 <- Sys.time()

t_diff <- t2 - t1
t_diff
