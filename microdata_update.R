library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)

# Connect to DB
# Run in console: cloud_sql_proxy.exe -instances=nytint-stg:us-east1:stg-mysql-mhvl=tcp:3306
# https://console.cloud.google.com/sql/instances/stg-mysql-mhvl/overview?project=nytint-stg&authuser=1&duration=PT1H

# Enter filename
datafile <- "cps_00064.csv.gz"
# # Enter new month/year
# newmonth <- 3
# newyear <- 2018
# # Enter previous month/year
# oldmonth <- 2
# oldyear <- 2018

# Functions are below. Assuming they're already entered, run these:
cps_add()

cps_flow(oldmonth = 3, oldyear = 2018, newmonth = 4, newyear = 2018)

walk(5:8, ~cps_flow(oldmonth = .x, oldyear = 2018, newmonth = .x+1, newyear = 2018))

# Column order:
# cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")
# t <- cps %>% filter(YEAR == 1994, MONTH == 1) %>% collect()
# col_order <- names(t)

# Function for adding new month of data
cps_add <- function() {
  cps_new <- read_csv(eval(datafile)) %>% # 
    mutate(date = paste0(YEAR, "-", MONTH, "-01"))
  if (is.null(cps_new$ASECFLAG)) {cps_new$ASECFLAG <- NA}
  cps_new <- cps_new[c(col_order)]
  # con <- dbConnect(SQLite(), dbname = "cps")
  
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "cass_cps_microdata",
                   host = "127.0.0.1",
                   user = user,
                   password = password)
  
  dbWriteTable(con, "cps_main", cps_new, append = TRUE, row.names = F)
  dbDisconnect(con)
}


# Function for constructing new flow

cps_flow <- function(oldmonth, oldyear, newmonth, newyear) {
  # Construct flow
  cps <- src_mysql(dbname = "cass_cps_microdata",
                   host = "127.0.0.1",
                   user = user,
                   password = password) %>% 
    tbl("cps_main")
  
  w <- cps %>% 
    filter((YEAR == newyear & MONTH == newmonth) | (YEAR == oldyear & MONTH == oldmonth), WTFINL > 0) %>% 
    collect() %>% 
    mutate(date = ymd(date))
  
  # Split by months and merge
  flow <- w %>% filter(MONTH == oldmonth) %>% mutate(CPSIDP = as.character(CPSIDP))
  flow <- w %>% filter(MONTH == newmonth) %>% mutate(CPSIDP = as.character(CPSIDP)) %>% 
    inner_join(flow, ., by = "CPSIDP")
  
  if (nrow(flow) == 0) {
    print(paste0("FAIL: ", date1))
    next} else
  
  # Eliminate mismatches
  flow <- flow %>% filter(SEX.x == SEX.y, RACE.x == RACE.y, AGE.y >= AGE.x, AGE.y < (AGE.x +3))
  
  # Adjust weights
  adj <- 1/(sum(flow$WTFINL.y)/w %>% filter(MONTH == newmonth) %>% summarize(sum(WTFINL)) %>% as.numeric())
  flow <- flow %>% mutate(flow_weight = WTFINL.y * adj)
  
  # Function for IDing LF status
  status_gen <- function(x) {
    sapply(x, function(x) {
      if (x %in% c(10, 12)) {status <- "E"} else
        if (x %in% c(20, 21, 22)) {status <- "U"} else
          if (x %in% (30:36)) { status <- "N"} else {status <- NA}
      status})
  }
  
  flow <- flow %>% rowwise() %>% 
    mutate(m1 = status_gen(EMPSTAT.x),
           m2 = status_gen(EMPSTAT.y),
           flow = paste0(m1, m2))

  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "cass_cps_microdata",
                   host = "127.0.0.1",
                   user = user,
                   password = password)
  dbWriteTable(con, "flow", flow, append = TRUE)
  dbDisconnect(con)
}


check <- w %>% filter(MONTH == 12) %>% select(CPSIDP) %>% rename(Dec = CPSIDP) %>% mutate(row = row_number())
check <- w %>% filter(MONTH == 1) %>% select(CPSIDP) %>% rename(Jan = CPSIDP) %>% mutate(row = row_number()) %>% 
  inner_join(check,., by = "row")

check$Jan <- w %>% filter(MONTH == 1) %>% select(CPSIDP)

which(duplicated(w$CPSIDP))


w %>% select(MONTH, CPSIDP) %>% mutate(i = row_number()) %>% spread(MONTH, CPSIDP)

bind_cols(w %>% filter(MONTH == 12) %>% select(CPSIDP), w %>% filter(MONTH == 1) %>% select(CPSIDP))


con <- dbConnect(SQLite(), dbname = "cps")
dbExecute(con, 'DELETE FROM flow WHERE "YEAR.y" = 2018 AND "MONTH.y" = 3')
