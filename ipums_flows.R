# Looks at monthly labor force status flows using CPS basic monthly data via IPUMS

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)

# First time setup:
# Have to read in chuks using 'chunkwise' and save in sqlite DB.
library(chunked)
# R.utils::gunzip("cps_basic_092117.csv.gz")
# cps_data <- read_csv_chunkwise(file = "cps_basic_092117.csv") %>%
#   mutate(date = paste0(YEAR, "-", MONTH, "-01"))
# 
# # Now read into sqplite DB
# cps_con <- src_sqlite("cps", create = TRUE)
# insert_chunkwise_into(cps_data, cps_con, "cps_main", temporary = FALSE)

# From now on should be able to connect via dbplyr
library(dbplyr)
cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

# Going to create flows via loop
# will feed into table "flows"

# Get first month manually
# Pull two consecutive months of data
w <- cps %>% filter(YEAR == 1994, MONTH %in% c(1,2), WTFINL > 0) %>% 
  collect() %>% 
  mutate(date = ymd(date))

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
  if (x %in% c(10, 12)) {status <- "E"} else
    if (x %in% c(20, 21, 22)) {status <- "U"} else
      if (x %in% (30:36)) { status <- "N"} else {status <- NA}
  status
}

flow <- flow %>% rowwise() %>% 
  mutate(m1 = status_gen(EMPSTAT.x),
         m2 = status_gen(EMPSTAT.y),
         flow = paste0(m1, m2))


flow %>% group_by(flow) %>% 
  summarize(total = sum(flow_weight))

con <- dbConnect(SQLite(), dbname = "cps")
dbWriteTable(con, "flow", flow)
dbDisconnect(con)

# OK, now do this for rest of months via for loop
months <- seq.Date(ymd("1994-02-01"),ymd("2017-07-01"), by = "month")

for (i in 101:110) {
 date1 <-  months[i]
 date2 <- months[i+1]
 dates <- c(paste0(year(date1), "-", as.numeric(month(date1)), "-01"),
            paste0(year(date2), "-", as.numeric(month(date2)), "-01"))

 w <- cps %>% 
   filter(date %in% dates, WTFINL > 0) %>% 
   collect() %>% 
   mutate(date = ymd(date))
 
 # Split by months and merge
 flow <- w %>% filter(date == date1)
 flow <- w %>% filter(date == date2) %>% 
   inner_join(flow, ., by = "CPSIDP")
 
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
 
 con <- dbConnect(SQLite(), dbname = "cps")
 dbWriteTable(con, "flow", flow, append = TRUE)
 dbDisconnect(con)
 rm(flow)
 
 print(months[i])
 
}

flow_data <- src_sqlite("cps", create = FALSE) %>% tbl("flow")

test <- flow_data %>% filter(m1 == "U") %>% 
  collect() %>% 
  group_by(date.x, flow) %>% 
  summarize(count = sum(flow_weight))

test %>% mutate(date = as.Date(date.x)) %>% 
  ggplot(., aes(date, count, colour = flow)) + geom_line()