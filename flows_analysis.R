# First run 'ipums_flows.R' to construct flow

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)
library(scales)

cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

flow_data <- src_sqlite("cps", create = FALSE) %>% tbl("flow")

status_gen <- function(x) {
  sapply(x, function(x) {
    if (x %in% c(10, 12)) {status <- "E"} else
      if (x %in% c(20, 21, 22)) {status <- "U"} else
        if (x %in% (30:36)) { status <- "N"} else {status <- NA}
    status})
}


# Job-finding rate by duration of uenmployment
unemp <- flow_data %>% filter(m1 == "U") %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, SEX.x, DURUNEMP.x) %>% 
  collect() 

# Function for categorizing unemployment durations
u_dur <- function(x) {
  d <- cut(x, breaks = c(-1, 4, 14, 26, 200), labels = c("0-4", "5-14", "15-26", "27+"))
  d
}

# First collect UE totals
finding <- unemp %>% 
  filter(flow == "UE") %>% 
  mutate(date = as.Date(date.y),
         dur = u_dur(DURUNEMP.x)) %>% 
  group_by(date, dur) %>% 
  summarize(ue_total = sum(flow_weight))

finding <- unemp %>% 
  filter(flow == "UE", SEX.x == 1) %>% 
  mutate(date = as.Date(date.y),
         dur = u_dur(DURUNEMP.x)) %>% 
  group_by(date, dur) %>% 
  summarize(ue_men = sum(flow_weight)) %>% 
  left_join(finding, ., by = c("date", "dur"))

finding <- unemp %>% 
  filter(flow == "UE", SEX.x == 2) %>% 
  mutate(date = as.Date(date.y),
         dur = u_dur(DURUNEMP.x)) %>% 
  group_by(date, dur) %>% 
  summarize(ue_women = sum(flow_weight)) %>% 
  left_join(finding, ., by = c("date", "dur"))

rm(unemp)
# Now collect total unemployed

unemp <- cps %>% filter(EMPSTAT %in% c(20, 21, 22)) %>% 
  select(date, DURUNEMP, SEX, WTFINL) %>% collect()

finding <- unemp %>% 
  mutate(date = as.Date(date),
         dur = u_dur(DURUNEMP)) %>% 
  group_by(date, dur) %>% 
  summarize(u_total = sum(WTFINL)) %>% 
  left_join(finding, ., by = c("date", "dur"))

finding <- unemp %>% filter(SEX == 1) %>% 
  mutate(date = as.Date(date),
         dur = u_dur(DURUNEMP)) %>% 
  group_by(date, dur) %>% 
  summarize(u_men = sum(WTFINL)) %>% 
  left_join(finding, ., by = c("date", "dur"))

finding <- unemp %>% filter(SEX == 2) %>% 
  mutate(date = as.Date(date),
         dur = u_dur(DURUNEMP)) %>% 
  group_by(date, dur) %>% 
  summarize(u_women = sum(WTFINL)) %>% 
  left_join(finding, ., by = c("date", "dur"))

library(scales)
finding %>% 
  mutate(rate = ue_total/u_total) %>% 
  group_by(dur) %>% 
  mutate(roll = rollmean(rate, 12, na.pad = TRUE, align = "right")) %>% 
  filter(date >= ymd("2000-01-01")) %>% 
  ggplot(., aes(date, roll, colour = dur)) + geom_line() +
  scale_y_continuous(label = percent)


finding %>% 
  mutate(rate = ue_total/u_total) %>% 
  group_by(dur) %>% 
  mutate(roll = rollmean(rate, 12, na.pad = TRUE, align = "right")) %>% 
  filter(date >= ymd("2007-12-01")) %>% 
  mutate(roll = roll/roll[1] -1) %>% 
  ggplot(., aes(date, roll, colour = dur)) + geom_line() +
  scale_y_continuous(label = percent)

save(finding, file = "finding_by_dur.RData")

# Flows into labor force

# Are young black men reentering LF at higher rate?

# First the flows
young_black <- flow_data %>% filter(SEX.x == 1, AGE.x >= 18, AGE.x > 35, RACE.x == 200) %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, DURUNEMP.x) %>% 
  collect() 

young_black %>% 
  filter(m1 == "N") %>% 
  mutate(date = as.Date(date.y)) %>% filter(date >= ymd("2000-01-01")) %>% 
  group_by(date, m2) %>% 
  summarize(value = sum(flow_weight)) %>% 
  ggplot(., aes(date, value, colour = m2)) + geom_line()

black_men_flow <- young_black %>% 
  filter(m1 == "N") %>% 
  mutate(date = as.Date(date.y)) %>% filter(date >= ymd("2000-01-01")) %>% 
  group_by(date, m2) %>% 
  summarize(black_flow = sum(flow_weight))

# Now the denominator
black_men <- cps %>% 
  filter(SEX == 1, AGE >= 18, AGE > 35, RACE == 200) %>% 
  select(date, AGE, EMPSTAT, DURUNEMP, WTFINL) %>% 
  collect()

black_men_flow <- black_men %>% 
  filter(EMPSTAT %in% (30:36)) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(black_total = sum(WTFINL)) %>% 
  left_join(black_men_flow, ., by = "date")

black_men_flow %>% 
  filter(m2 != "N") %>% group_by(m2) %>% 
  mutate(share = black_flow/black_total,
         roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = m2)) + geom_line() +
  scale_y_continuous(label = percent)


black_men <- black_men %>% rowwise() %>% 
  mutate(date = as.Date(date),
         status = status_gen(EMPSTAT))

# Generic function for calculating flow rate
flow_gen <- function(m1_state, m2_state, agerange, sex, ed) {
  
  # Flow
  flow_data <- src_sqlite("cps", create = FALSE) %>% tbl("flow")
  
  numerator <- flow_data %>% filter(m1 %in% m1_state, m2 %in% m2_state, 
                                    AGE.y %in% agerange, SEX.y %in% sex,
                                    EDUC.y %in% ed) %>% 
    select(date.y, m1, m2, flow, flow_weight) %>% 
    collect() 
  
  numerator <- numerator %>% 
    mutate(date = as.Date(date.y)) %>% 
    group_by(date, m2) %>% 
    summarize(numerator = sum(flow_weight))
  
  print("Numerator done.")
  
  # Now the denominator
  
  cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")
  
  denominator <- cps %>% 
    filter(SEX %in% sex, AGE %in% agerange, EDUC %in% ed) %>% 
    select(date, EMPSTAT, WTFINL) %>% 
    collect()
  
  denominator <- denominator %>% 
    mutate(date = as.Date(date), 
           status = status_gen(EMPSTAT)) %>% 
    filter(status %in% m1_state) %>% 
    group_by(date) %>% 
    summarize(denominator = sum(WTFINL)) 
  
  print("Denominator done.")
  
  # Now join 'em
  flow <- left_join(numerator, denominator, by = "date") %>% 
    mutate(flow = numerator/denominator)
  
  flow
}

# Enter selections
m1_state <- "N" # set first-month state
m2_state <- "E" # set second-month state

# demographic filters
agerange <- 25:34 # set age range
sex <- 1:2 # set sex
ed <- 0:111 # set education

young_noncollege <- flow_gen("N", c("E", "U"), 25:34, 1:2, 0:111)

young_noncollege <- young_noncollege %>% 
  select(-m2) %>% 
  rename(total_nilf = denominator,
         total_flow = numerator,
         total_share = flow)

young_noncollege <- flow_gen("N", c("E", "U"), 25:34, 1, 0:111) %>% 
  select(-m2) %>% 
  rename(men_nilf = denominator,
         men_flow = numerator,
         men_share = flow) %>% 
  left_join(young_noncollege, ., by = "date")

young_noncollege <- flow_gen("N", c("E", "U"), 25:34, 2, 0:111) %>% 
  select(-m2) %>% 
  rename(women_nilf = denominator,
         women_flow = numerator,
         women_share = flow) %>% 
  left_join(young_noncollege, ., by = "date")


young_noncollege %>% 
  select(date, men_share, women_share) %>% 
  filter(date >= ymd("2000-01-01")) %>% 
  gather(sex, value, -date) %>% group_by(sex) %>% 
  mutate(roll = rollmean(value, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = sex)) + geom_line()
  

test <- flow_gen(m1_state, m2_state, agerange, sex, ed)


test %>% filter(date >= ymd("2000-01-01")) %>% ungroup() %>% 
  mutate(roll = rollmean(flow, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll)) + geom_line() + scale_y_continuous(labels = percent)

status_gen(10:25)

ts_test <- cps %>% filter(YEAR >= 2000, EMPSTAT %in% c(10,12), SEX == 1) %>% 
  select(date, WTFINL) %>% 
  collect()

ts_test <- ts_test %>% mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(value = sum(WTFINL))


# Function for seasonal adjustment

seas_adjust <- function(df, name_of_date_col, name_of_val_col) {
  library(seasonal)
  library(dplyr)
  
  expr1 <- enquo(name_of_date_col)
  expr2 <- enquo(name_of_val_col)
  
  w <- df %>% select(date = UQ(expr1), val = UQ(expr2))
  
  w.ts <- ts(w$val, frequency = 12, start = c(year(w$date[1]), month(w$date[1])))
  w.sa <- seas(w.ts)
  rm(w, w.ts)
  as.numeric(w.sa$data[,1])
}

seas_adjust(ts_test, date, value)

women <- cps %>% filter(YEAR >= 2000, EMPSTAT %in% c(10,12), SEX == 2) %>% 
  select(date, WTFINL) %>% 
  collect()

women <- women %>% mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(emp_NSA = sum(WTFINL))

library("tidyseasonal")
women$emp_SA <- seas_adjust(women, date, emp_NSA)

ts_test %>% 
  gather(adj, value, -date) %>% 
  ggplot(., aes(date, value, colour = adj)) + geom_line()

class(w.sa$data)



men <- cps %>% filter(YEAR >= 2000, EMPSTAT %in% c(10,12), SEX == 1) %>% 
  select(date, WTFINL) %>% 
  collect()

men <- men %>% mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(emp_NSA = sum(WTFINL))

men$emp_SA <- seas_adjust(men, date, emp_NSA)
