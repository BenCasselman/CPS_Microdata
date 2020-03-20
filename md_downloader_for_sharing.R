# Code for parsing/saving microdata straight from census site
library(tidyverse)
library(lubridate)

# Find URL here and enter it: https://thedataweb.rm.census.gov/ftp/cps_ftp.html
# (Formats aren't totally consistent, so best to do it this way)
url <- "https://thedataweb.rm.census.gov/pub/cps/basic/201701-/jul18pub.zip" # Have to change this to https


latest_microdata <- md_download(url)

md_download <- function(url) {
  lengths <- read_csv("series_lengths_2017.csv", col_types = "cii")
  
  temp <- tempfile()
  download.file(url, temp)
  
  working <- read_fwf(unzip(temp, unzip(temp, list = T)[[1]]), 
                      fwf_positions(lengths$start, lengths$end, lengths$series_id))
  
  working <- working %>% 
    mutate(date = ymd(paste(HRYEAR4, HRMONTH, 1, sep = "-")),
           personid = paste(HRHHID, HRHHID2, PULINENO, sep = "-"))
  

  return(working)
  
}