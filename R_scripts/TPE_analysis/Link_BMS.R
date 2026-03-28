############
# Link BMS #
############

# Install package ----
# install.packages("exifr")
# install.packages("getPass")
# install.packages("package/brapir-master.tar.gz", type = "source", repos = NULL)
# install.packages("package/bmsapi_0.2.0.tar.gz", type = "source", repos = NULL)
# install.packages("package/bmsapi-main.tar.gz", type = "source", repos = NULL)

# Load library ----
library(brapir)
library(bmsapi)
library(data.table)
library(tidyr)
library(httr)
library(rjson)

# Add hoc function ----
bms_get_brapiv2_token <- function(instance_url,
                                  user,
                                  pwd= getPass::getPass(msg = paste("Enter BMS password for user",user)),
                                  copytok2clipr=FALSE){
  url <- paste0(instance_url, "/bmsapi/brapi/v1/token")
  response <- httr::POST(url,
                         accept_json(),
                         content_type_json(),
                         body = list(username=user,
                                     password=pwd),
                         encode = "json")
  token <- fromJSON(rawToChar(response$content))$access_token
  if (copytok2clipr) clipr::write_clip(token)
  return(paste(#"Bearer",
    token))
}

# generate a token ----
bmsserver <- "iavao.bmspro.io"
bms_token <- bms_get_brapiv2_token(paste0("https://", bmsserver),
                                   user = "vincent.garin")

# create a connection ----
con <- brapir::brapi_connect(
  secure = T,
  db = bmsserver,
  # apipath = "bmsapi",
  multicrop = TRUE,
  commoncropname = "iavaomillet",
  token = bms_token
)

# get_trial_data ----

data <- get_trial_data(
  con,
  trialnames = "IAVAO2023MilTrials_SUDAN",
  studydbids = NULL,
  observationLevel = "PLOT",
  numonly = TRUE
)

