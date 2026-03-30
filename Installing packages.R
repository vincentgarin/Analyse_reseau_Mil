
### Install Packages 

pkg_list <- c("apsimx", "data.table", "desplot", "dplyr", "ggplot2", "googlesheets4", "httr",  "jsonlite", "lme4", "lubridate", "nasapower", "readxl", "rjson", "stringr",  "tidyr", "xml2")

new.packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(new.packages) >0) install.packages(new.packages)

### Verify Libraries

library(data.table)
library(tidyr)
library(httr)
library(rjson)

library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)
library(desplot)
library(tidyr)

library(jsonlite)
library(lubridate)
library(nasapower)

library(stringr)
library(apsimx)
library(ggplot2)
library(googlesheets4)
library(xml2)