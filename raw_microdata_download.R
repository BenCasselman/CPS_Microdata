# Code for parsing/saving microdata straight from census
library(tidyverse)
library(lubridate)
library(dbplyr)

# CURRENT DATE RANGE
# Earliest: May 2012
# Latest: February 2020

# Find URL here and enter it: https://thedataweb.rm.census.gov/ftp/cps_ftp.html
# (Formats aren't totally consistent, so best to do it this way)
url <- "https://thedataweb.rm.census.gov/pub/cps/basic/201701-/aug18pub.zip" # Have to change this to https

# Run in console: cloud_sql_proxy.exe -instances=nytint-stg:us-east1:stg-mysql-mhvl=tcp:3306

# raw_data_upload(url)

# Group download. Remember to change to https!
files <- list("https://www2.census.gov/programs-surveys/cps/datasets/2020/basic/jan20pub.zip",
              "https://www2.census.gov/programs-surveys/cps/datasets/2020/basic/feb20pub.zip")

walk(files, ~raw_data_upload(.x))

# Test it out:
cps_raw <- src_mysql(dbname = "cass_cps_microdata",
                     host = "127.0.0.1",
                     user = user,
                     password = password) %>% 
  tbl("raw_microdata")

# t <- read_csv("series_lengths_2017.csv", col_types = "cii")
# db_names <- t$series_id
# save(db_names, file = "db_names.RData")

raw_data_upload <- function(url) {
  lengths <- read_csv("series_lengths_2020.csv", col_types = "ciii")
  
  temp <- tempfile()
  download.file(url, temp)
  
  working <- read_fwf(unzip(temp, unzip(temp, list = T)[[1]]), 
                      fwf_positions(lengths$start, lengths$end, lengths$series_id))
  
  working <- working %>% 
    mutate(date = ymd(paste(HRYEAR4, HRMONTH, 1, sep = "-")),
           personid = paste(HRHHID, HRHHID2, PULINENO, sep = "-"))
  
  # Connect to DB
  # Run in console: cloud_sql_proxy.exe -instances=nytint-stg:us-east1:stg-mysql-mhvl=tcp:3306
  # https://console.cloud.google.com/sql/instances/stg-mysql-mhvl/overview?project=nytint-stg&authuser=1&duration=PT1H
  
  # First-time setup only:
  # cps_con <- src_mysql(dbname = "cass_cps_microdata",
  #                      host = "127.0.0.1",
  #                      user = "cass_test",
  #                      password = "cass_test")
  # 
  # working %>%
  #   mutate(date = as.character(date)) %>%
  #   copy_to(cps_con,
  #           .,
  #           "raw_microdata",
  #           temporary = FALSE,
  #           indexes = list("personid", "date"))
  
  
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "cass_cps_microdata",
                   host = "127.0.0.1",
                   user = user,
                   password = password)
  
  load("db_names.RData")
  
  db_names <- db_names[db_names %in% names(working)]
  
  working %>%
    select(db_names) %>% 
    mutate(date = as.character(date)) %>%
    dbWriteTable(con, "raw_microdata", ., append = TRUE, row.names = FALSE)
  
  dbDisconnect(dbListConnections( dbDriver( drv = "MySQL"))[[1]])
  
}