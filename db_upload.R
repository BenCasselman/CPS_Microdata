library(tidyverse)
library(dbplyr)
library(lubridate)

filename <- "cps_00047.csv" # insert name of file to be read

# Connect to DB
# Run in console: cloud_sql_proxy.exe -instances=nytint-stg:us-east1:stg-mysql-mhvl=tcp:3306
# https://console.cloud.google.com/sql/instances/stg-mysql-mhvl/overview?project=nytint-stg&authuser=1&duration=PT1H

cps_con <- src_mysql(dbname = "cass_cps_microdata",
                   host = "127.0.0.1",
                   user = "cass_test",
                   password = "cass_test")


# First time setup:
# Have to read in chuks using 'chunkwise' and save in mysql DB.
library(chunked)
R.utils::gunzip(paste0(filename, ".gz"))
cps_data <- read_csv_chunkwise(file = filename) %>%
   mutate(date = paste0(YEAR, "-", MONTH, "-01"))

#
# # Now read into sqplite DB
insert_chunkwise_into(cps_data, cps_con, "cps_main", temporary = FALSE)

# # From now on should be able to connect via dbplyr
cps <- cps_con %>%
  tbl("cps_main")

# Going to create flows via loop
# will feed into table "flows"

# Get first month manually
# Pull two consecutive months of data
w <- cps %>% 
  filter(YEAR == 1994, MONTH %in% c(1,2), WTFINL > 0) %>% 
  collect()

# Split by months and merge
flow <- w %>% filter(MONTH == 1)
flow <- w %>% filter(MONTH == 2) %>% 
  inner_join(flow, ., by = "CPSIDP")

# Eliminate mismatches
flow <- flow %>% filter(SEX.x == SEX.y, RACE.x == RACE.y, AGE.y >= AGE.x, AGE.y < (AGE.x +3))

# Adjust weights
adj <- 1/(sum(flow$WTFINL.y)/w %>% filter(MONTH == 2) %>% summarize(sum(WTFINL)) %>% as.numeric())
flow <- flow %>% mutate(flow_weight = WTFINL.y * adj)

# Function for IDing LF status
status_gen <- function(x) {
  sapply(x, function(x) {
    if (x %in% c(10, 12)) {status <- "E"} else
      if (x %in% c(20, 21, 22)) {status <- "U"} else
        if (x %in% (30:36)) { status <- "N"} else {status <- NA}
    status})
}

flow <- flow %>% 
  mutate(m1 = status_gen(EMPSTAT.x),
         m2 = status_gen(EMPSTAT.y),
         flow = paste0(m1, m2))


flow %>% group_by(flow) %>% 
  summarize(total = sum(flow_weight))

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "cass_cps_microdata",
                 host = "127.0.0.1",
                 user = "cass_test",
                 password = "cass_test")

dbWriteTable(con, "flow", flow)

# OK, now do this for rest of months via for loop
months <- seq.Date(ymd("1994-02-01"),ymd("2018-12-01"), by = "month")

for (i in 1:length(months)) {
  date1 <-  months[i]
  date2 <- months[i+1]
  dates <- c(paste0(year(date1), "-", as.numeric(month(date1)), "-01"),
             paste0(year(date2), "-", as.numeric(month(date2)), "-01"))
  
  w <- cps %>% 
    filter(date %in% dates, WTFINL > 0) %>% 
    collect() 
  
  # Split by months and merge
  flow <- w %>% filter(date == date1)
  flow <- w %>% filter(date == date2) %>% 
    inner_join(flow, ., by = "CPSIDP")
  
  if (nrow(flow) == 0) {
    print(paste0("FAIL: ", date1))
    next} else
      
      # Eliminate mismatches
      flow <- flow %>% filter(SEX.x == SEX.y, RACE.x == RACE.y, AGE.y >= AGE.x, AGE.y < (AGE.x +3))
  
  # Adjust weights
  adj <- 1/(sum(flow$WTFINL.y)/w %>% filter(MONTH == month(date2)) %>% summarize(sum(WTFINL)) %>% as.numeric())
  rm(w)
  
  flow <- flow %>% mutate(flow_weight = WTFINL.y * adj)
  
  flow <- flow %>% rowwise() %>% 
    mutate(m1 = status_gen(EMPSTAT.x),
           m2 = status_gen(EMPSTAT.y),
           flow = paste0(m1, m2))
  
  dbWriteTable(con, "flow", flow, append = TRUE)
  rm(flow)
  
  print(months[i])
  
}
