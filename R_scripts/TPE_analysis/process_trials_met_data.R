############################################
# Process .met files for each trial (year) #
############################################

# For each trial, use the location information: latitude and longitude, as well
# as the year to get the weather data from nasapower.

# script written by Claude (AI) that iterate over the locations


## ============================================================
##  Generate APSIM .met Weather Files from NASA POWER API
##  Iterates over a data.frame of latitude, longitude, and year
## ============================================================
##
##  APSIM .met file format requires:
##    - Header block  : location metadata + unit declarations
##    - Data columns  : year, day (DOY), radn, maxt, mint, rain
##                      (optionally: wind, rh, vp, evap, etc.)
##
##  Data source: NASA POWER API (freely accessible, no API key needed)
##  API docs: https://power.larc.nasa.gov/docs/services/api/
##
##  Required R packages:
##    install.packages(c("httr", "jsonlite", "dplyr", "lubridate"))
## ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# ---------------------------------------------------------------
# 1.  DEFINE YOUR TRIALS data.frame
#     Columns required: lat, lon, year
# ---------------------------------------------------------------

# d_trials <- data.frame(
#   location = c("Montpellier", "Toulouse",   "Lyon"),   # optional location name
#   lat  = c(43.61,          43.60,        45.75),
#   lon  = c(3.87,            1.44,         4.85),
#   year = c(2020,            2020,         2021),
#   stringsAsFactors = FALSE
# )

load(file = "output/d_trials.RData")

# temporary subset
# d_trials <- d_trials[1:2, ]

# ---------------------------------------------------------------
# 2.  HELPER FUNCTIONS
# ---------------------------------------------------------------

#' Fetch daily weather data from NASA POWER for one location × year
#'
#' @param lat  Latitude  (decimal degrees, WGS84)
#' @param lon  Longitude (decimal degrees, WGS84)
#' @param year Integer year (full calendar year is retrieved)
#' @return     data.frame with columns: year, day, radn, maxt, mint, rain
fetch_nasa_power <- function(lat, lon, year) {
  
  start <- paste0(year, "0101")
  end   <- paste0(year, "1231")
  
  # NASA POWER variables needed for APSIM
  #   ALLSKY_SFC_SW_DWN  → solar radiation  (MJ/m2/day)
  #   T2M_MAX            → max temperature  (°C)
  #   T2M_MIN            → min temperature  (°C)
  #   PRECTOTCORR        → precipitation    (mm/day)
  params <- "ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN,PRECTOTCORR"
  
  url <- sprintf(
    paste0("https://power.larc.nasa.gov/api/temporal/daily/point",
           "?parameters=%s",
           "&community=AG",
           "&longitude=%.4f&latitude=%.4f",
           "&start=%s&end=%s",
           "&format=JSON"),
    params, lon, lat, start, end
  )
  
  message(sprintf("  Fetching NASA POWER: lat=%.4f lon=%.4f year=%d ...",
                  lat, lon, year))
  
  resp <- httr::GET(url, httr::timeout(120))
  
  if (httr::http_error(resp)) {
    stop(sprintf("NASA POWER API error (HTTP %d) for lat=%.4f lon=%.4f year=%d",
                 httr::status_code(resp), lat, lon, year))
  }
  
  raw   <- httr::content(resp, as = "text", encoding = "UTF-8")
  data  <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
  
  props <- data$properties$parameter
  
  dates <- as.Date(names(props$T2M_MAX), format = "%Y%m%d")
  
  df <- data.frame(
    year = as.integer(format(dates, "%Y")),
    day  = as.integer(format(dates, "%j")),   # DOY 1–365/366
    radn = as.numeric(props$ALLSKY_SFC_SW_DWN),
    maxt = as.numeric(props$T2M_MAX),
    mint = as.numeric(props$T2M_MIN),
    rain = as.numeric(props$PRECTOTCORR),
    stringsAsFactors = FALSE
  )
  
  # Replace NASA fill value (-999) with NA, then warn
  fill_val <- -999
  df[df == fill_val] <- NA
  
  if (anyNA(df)) {
    warning(sprintf("Missing values detected for lat=%.4f lon=%.4f year=%d",
                    lat, lon, year))
  }
  
  return(df)
}


#' Compute the tav and amp header values required by APSIM
#'
#' @param df  data.frame returned by fetch_nasa_power()
#' @return    named numeric vector: tav, amp
compute_tav_amp <- function(df) {
  
  # tav: annual average of mean daily temperature
  df$tmean <- (df$maxt + df$mint) / 2
  
  monthly_mean <- df %>%
    mutate(month = as.integer(format(
      as.Date(paste(year, day), "%Y %j"), "%m"))) %>%
    group_by(month) %>%
    summarise(tmean_m = mean(tmean, na.rm = TRUE),
              tmax_m  = mean(maxt,  na.rm = TRUE),
              tmin_m  = mean(mint,  na.rm = TRUE),
              .groups = "drop")
  
  tav <- mean(monthly_mean$tmean_m, na.rm = TRUE)
  amp <- max(monthly_mean$tmax_m, na.rm = TRUE) -
    min(monthly_mean$tmin_m, na.rm = TRUE)
  
  return(c(tav = round(tav, 1), amp = round(amp, 1)))
}


#' Write a single APSIM .met file
#'
#' @param df        data.frame from fetch_nasa_power()
#' @param lat       Latitude
#' @param lon       Longitude
#' @param location_name Character label used in the header
#' @param out_path  Full path for the output .met file
write_met_file <- function(df, lat, lon, location_name, out_path) {
  
  ta <- compute_tav_amp(df)
  
  # ---- Build header ----
  header <- c(
    "[weather.met.weather]",
    sprintf("!  Site         = %s", location_name),
    sprintf("!  Latitude     = %.4f (decimal degrees)", lat),
    sprintf("!  Longitude    = %.4f (decimal degrees)", lon),
    sprintf("!  Source       = NASA POWER API (community AG)"),
    sprintf("!  Created      = %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "!",
    sprintf("Latitude  = %.4f  (DECIMAL DEGREES)", lat),
    sprintf("tav       = %.1f  (oC)   ! Annual average ambient temperature",
            ta["tav"]),
    sprintf("amp       = %.1f  (oC)   ! Annual amplitude in mean monthly temperature",
            ta["amp"]),
    "!",
    "year  day  radn  maxt  mint  rain",
    "()    ()   (MJ/m^2)  (oC)  (oC)  (mm)"
  )
  
  # ---- Format data rows ----
  data_lines <- sprintf("%4d  %3d  %6.2f  %6.2f  %6.2f  %6.2f",
                        df$year, df$day,
                        ifelse(is.na(df$radn), -999, df$radn),
                        ifelse(is.na(df$maxt), -999, df$maxt),
                        ifelse(is.na(df$mint), -999, df$mint),
                        ifelse(is.na(df$rain), -999, df$rain))
  
  # ---- Write to file ----
  writeLines(c(header, data_lines), con = out_path)
  message(sprintf("  Written → %s  (%d rows)", out_path, nrow(df)))
}

# ---------------------------------------------------------------
# 3.  MAIN LOOP  –  iterate over each row of `trials` (d_trials)
# ---------------------------------------------------------------

output_dir <- "data/weather/met_files"          # change to any directory you prefer
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

met_files <- rep(NA, nrow(d_trials))

for (i in seq_len(nrow(d_trials))) {
  
  row  <- d_trials[i, ]
  lat  <- row$lat
  lon  <- row$lon
  yr   <- row$year
  location <- if ("location" %in% names(row)) row$location else paste0("location_", i)
  fname <- paste0(row$env, ".met")
  
  # Sanitise location name for use in filename
  safe_location <- gsub("[^A-Za-z0-9_-]", "_", location)
  # fname     <- sprintf("%s_%d.met", safe_location, yr)
  # met_files[i] <- fname
  fpath     <- file.path(output_dir, fname)
  
  message(sprintf("\n[%d/%d] %s  (lat=%.4f, lon=%.4f, year=%d)",
                  i, nrow(d_trials), location, lat, lon, yr))
  
  tryCatch({
    
    weather_df <- fetch_nasa_power(lat, lon, yr)
    write_met_file(weather_df, lat, lon, location, fpath)
    
  }, error = function(e) {
    message(sprintf("  ERROR for %s year %d: %s", location, yr, e$message))
  })
  
  # Brief pause to be polite to the NASA API
  Sys.sleep(1)
}

message("\nAll done. .met files saved in: ", normalizePath(output_dir))

d_trials$met_file <- met_files

save(d_trials, file = "output/d_trials.RData")

# ---------------------------------------------------------------
# 4.  (OPTIONAL) PREVIEW: read back and inspect one file
# ---------------------------------------------------------------

# preview_met <- function(path, n_rows = 5) {
#   lines <- readLines(path)
#   header_end <- grep("^\\(\\)", lines)[1]        # line with units row "()"
#   col_names  <- strsplit(trimws(lines[header_end - 1]), "\\s+")[[1]]
#   data_lines <- lines[(header_end + 1):length(lines)]
#   df <- read.table(text = data_lines, col.names = col_names)
#   cat("--- Header ---\n")
#   cat(paste(lines[1:header_end], collapse = "\n"), "\n\n")
#   cat("--- First rows ---\n")
#   print(head(df, n_rows))
# }
#
# preview_met(file.path(output_dir, "Montpellier_2020.met"))