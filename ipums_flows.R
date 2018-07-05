# Looks at monthly labor force status flows using CPS basic monthly data via IPUMS

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)
library(scales)

# First time setup:
# Have to read in chuks using 'chunkwise' and save in sqlite DB.
# library(chunked)
# R.utils::gunzip("cps_basic_022018.csv.gz")
# cps_data <- read_csv_chunkwise(file = "cps_basic_022018.csv") %>%
#   mutate(date = paste0(YEAR, "-", MONTH, "-01"))
# #
# # # Now read into sqplite DB
# cps_con <- src_sqlite("cps", create = TRUE)
# insert_chunkwise_into(cps_data, cps_con, "cps_main", temporary = FALSE)

# From now on should be able to connect via dbplyr
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

con <- dbConnect(SQLite(), dbname = "cps")
dbWriteTable(con, "flow", flow)
dbDisconnect(con)

# OK, now do this for rest of months via for loop
months <- seq.Date(ymd("1994-02-01"),ymd("2018-12-01"), by = "month")

for (i in 1:length(months)) {
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
 
 con <- dbConnect(SQLite(), dbname = "cps")
 dbWriteTable(con, "flow", flow, append = TRUE)
 dbDisconnect(con)
 rm(flow)
 
 print(months[i])
 
}

flow_data <- src_sqlite("cps", create = FALSE) %>% tbl("flow")

unemp <- flow_data %>% filter(m1 == "U") %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, SEX.x, DURUNEMP.x) %>% 
  collect() 


unemp %>% 
  group_by(date.y, flow) %>% 
  summarize(count = sum(flow_weight)) %>% 
  mutate(date = as.Date(date.y)) %>% 
  ggplot(., aes(date, count, colour = flow)) + geom_line()



p <- unemp %>% 
  mutate(duration = cut(DURUNEMP.x, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% 
  group_by(date, duration, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, duration) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% group_by(duration) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = duration)) + geom_line() + recession_shade("1995-01-01") + scale_y_continuous(label = percent)

cbPalette <- c("#66c2a5", "#fc8d62", "#8da0cb")

p <- p + labs(x = NULL, y = NULL,
       title = "Job-finding rate by duration of unemployment",
       subtitle = "12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  geom_line(size = 1.1) + scale_color_manual(values = cbPalette) + 
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.background = element_rect(fill = "grey92"),
        panel.grid.major = element_line(colour = "grey", size = 0.15),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50"),
        axis.ticks = element_line(colour = "grey", size = 0.15),
        plot.caption = element_text(colour = "grey50")) +
  theme(legend.position = "none") +
  annotate("text", x = ymd("2014-06-01"), y = .37, label = "bold(`Less than 5 weeks`)", parse = TRUE, colour = "#66c2a5", size = 3) +
  annotate("text", x = ymd("2014-06-01"), y = .242, label = "bold(`5-26 weeks`)", parse = TRUE, colour = "#fc8d62", size = 3) +
  annotate("text", x = ymd("2014-06-01"), y = .142, label = "bold(`27 weeks and up`)", parse = TRUE, colour = "#8da0cb", size = 3) 
  
ggsave("finding.png", p, device = "png", width = 7.1, height = 4)


unemp %>% 
  mutate(duration = cut(DURUNEMP.x, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% filter(year(date) >= 2015) %>% 
  group_by(date, duration, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, duration) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% group_by(duration) %>% 
  ggplot(., aes(date, share, colour = duration)) + geom_line()


# Job finding by age
unemp %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 24, 54, 64, 100), labels = c("Under 25", "25-54", "54-64", "65+"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% 
  group_by(date, agegroup, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% group_by(agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line() + recession_shade("1995-01-01") + scale_y_continuous(label = percent)


unemp %>% filter(AGE.x >= 25, AGE.x<65) %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% 
  group_by(date, agegroup, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% select(-m2, -total) %>% 
  spread(agegroup, share) %>% 
  mutate(ratio = Older/Prime) %>% ungroup() %>% 
  mutate(roll = rollmean(ratio, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll)) + geom_line() 





# Year-to-year flows

# Get first month manually
# Pull two consecutive months of data
w <- cps %>% filter(MONTH == 1, YEAR %in% c(1994, 1995), WTFINL > 0) %>% 
  collect() %>% 
  mutate(date = ymd(date))

# Split by months and merge
year_flow <- w %>% filter(YEAR == 1994) %>% mutate(CPSIDP = as.character(CPSIDP))
year_flow <- w %>% filter(YEAR == 1995) %>% mutate(CPSIDP = as.character(CPSIDP)) %>% 
  inner_join(year_flow, ., by = "CPSIDP")

# Eliminate mismatches
year_flow <- year_flow %>% filter(SEX.x == SEX.y, RACE.x == RACE.y, AGE.y >= AGE.x, AGE.y < (AGE.x +3))

# Adjust weights
adj <- 1/(sum(year_flow$WTFINL.y)/w %>% filter(YEAR == 1995) %>% summarize(sum(WTFINL)) %>% as.numeric())
year_flow <- year_flow %>% mutate(flow_weight = WTFINL.y * adj)

year_flow <- year_flow %>% 
  mutate(m1 = status_gen(EMPSTAT.x),
         m2 = status_gen(EMPSTAT.y),
         year_flow = paste0(m1, m2))

year_flow %>% group_by(year_flow) %>% 
  summarize(total = sum(flow_weight))

con <- dbConnect(SQLite(), dbname = "cps")
dbWriteTable(con, "year_flow", year_flow)
dbDisconnect(con)

# OK, now do this for rest of months via for loop
months <- seq.Date(ymd("1998-01-01"),ymd("2018-02-01"), by = "month")

for (i in 1:length(months)) {
  date1 <-  months[i]
  date2 <- months[i+12]
  month <- month(date1)
  year1 <- year(date1)
  
  if (date2 > ymd("2018-02-01")) break
  
  w <- cps %>% 
    filter(MONTH == month, (YEAR == year1 | YEAR == (year1 +1)), WTFINL > 0) %>% 
    collect() %>% 
    mutate(date = ymd(date))
  
  # Split by months and merge
  year_flow <- w %>% filter(date == date1) %>% mutate(CPSIDP = as.character(CPSIDP))
  year_flow <- w %>% filter(date == date2) %>% mutate(CPSIDP = as.character(CPSIDP)) %>% 
    inner_join(year_flow, ., by = "CPSIDP")
  
  if (nrow(year_flow) == 0) {
    print(paste0("FAIL: ", date1))
    next} else
      
      # Eliminate mismatches
      year_flow <- year_flow %>% filter(SEX.x == SEX.y, RACE.x == RACE.y, AGE.y >= AGE.x, AGE.y < (AGE.x +3))
  
  # Adjust weights
  adj <- 1/(sum(year_flow$WTFINL.y)/w %>% filter(YEAR == year(date2)) %>% summarize(sum(WTFINL)) %>% as.numeric())
  rm(w)
  
  year_flow <- year_flow %>% mutate(flow_weight = WTFINL.y * adj)
  
  year_flow <- year_flow %>% rowwise() %>% 
    mutate(m1 = status_gen(EMPSTAT.x),
           m2 = status_gen(EMPSTAT.y),
           year_flow = paste0(m1, m2))
  
  con <- dbConnect(SQLite(), dbname = "cps")
  dbWriteTable(con, "year_flow", year_flow, append = TRUE)
  dbDisconnect(con)
  rm(year_flow)
  
  print(months[i])
  
}

year_flow <- src_sqlite("cps", create = FALSE) %>% tbl("Year_flow")
