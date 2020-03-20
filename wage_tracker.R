# Atlanta Fed replication
library(lubridate)
library(zoo)
library(bigvis)
cps_raw <- src_mysql(dbname = "cass_cps_microdata",
                     host = "127.0.0.1",
                     user = user,
                     password = password) %>% 
  tbl("raw_microdata")

# Want only ORG
wage_tracker <- cps_raw %>% 
  filter(PRERELG == 1) %>% 
  select(date, personid, PWORWGT, PWLGWGT, PRNAGWS, PRFTLF,
         PRERNHLY, PTHR, PEERNHRO, PRERNWA, PTWK, PEHRACT1, PRWERNAL, PRHERNAL,
         PESEX, PRTAGE, PTDTRACE, PEHSPNON, PEEDUCA) %>% 
  collect()

# Now construct flows
# Month1
wage_flow <- wage_tracker %>% 
  mutate(date = ymd(date),
         flow_date = date + years(1),
         race = case_when(PEHSPNON == 1 ~ "Hispanic",
                          PTDTRACE == 1 ~ "White",
                          PTDTRACE == 2 ~ "Black",
                          TRUE ~ "Other"),
         wage = case_when(PRHERNAL != 1 & PRERNHLY < 9999 & PRERNHLY > 0 ~ as.double(PRERNHLY),
                          PRWERNAL != 1 & PTWK != 1 & PEERNHRO > 0 ~ PRERNWA/PEERNHRO,
                          PRWERNAL != 1 & PTWK != 1 & PEHRACT1 > 0 ~ PRERNWA/PEHRACT1)) %>% 
  filter(date <= ymd("2018-03-01"))

# Month2 and merge
wage_flow <- wage_tracker %>% 
  mutate(date = ymd(date),
         flow_date = date,
         race = case_when(PEHSPNON == 1 ~ "Hispanic",
                          PTDTRACE == 1 ~ "White",
                          PTDTRACE == 2 ~ "Black",
                          TRUE ~ "Other"),
         wage = case_when(PRHERNAL != 1 & PRERNHLY < 9999 & PRERNHLY > 0 ~ as.double(PRERNHLY),
                          PRWERNAL != 1 & PTWK != 1 & PEERNHRO > 0 ~ PRERNWA/PEERNHRO,
                          PRWERNAL != 1 & PTWK != 1 & PEHRACT1 > 0 ~ PRERNWA/PEHRACT1)) %>%
  inner_join(wage_flow, ., by = c("flow_date", "personid"))

# Drop bad matches
wage_flow <- wage_flow %>% 
  filter(PESEX.x == PESEX.y,
         race.x == race.y,
         PRTAGE.x <= PRTAGE.y,
         !PRTAGE.y > PRTAGE.x + 2,
         wage.x >= 213,
         wage.y >= 213)

tracker <- wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213) %>% 
  mutate(change = wage.y/wage.x -1,
         weight = PWORWGT.y/10000) %>% 
  group_by(flow_date) %>% 
  summarize(weighted_change = weighted.median(change, weight, na.rm = T),
            unweighted_change = median(change, na.rm = T))

tracker %>% 
  mutate(roll = rollmean(unweighted_change, 3, align = "right", na.pad = T)) %>% 
  filter(!is.na(roll)) %T>% View("tracker") %>% 
  ggplot(., aes(flow_date, roll)) + geom_line()

race_tracker <- wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213) %>% 
  mutate(change = wage.y/wage.x -1) %>% 
  group_by(flow_date, race.x) %>% 
  summarize(change = median(change, na.rm = T))

race_tracker %>% 
  group_by(race = race.x) %>% 
  mutate(roll = rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(flow_date, roll, colour = race)) + geom_line()


wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213) %>% 
  mutate(change = wage.y/wage.x -1) %>% 
  group_by(flow_date, PESEX.x) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race = factor(PESEX.x)) %>% 
  mutate(roll = rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(flow_date, roll, colour = race)) + geom_line()


wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213) %>% 
  mutate(change = wage.y/wage.x -1,
         white = case_when(race.x == "White" ~ "white",
                           TRUE ~ "nonwhite")) %>% 
  group_by(flow_date, white) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race = factor(white)) %>% 
  mutate(roll = rollmean(change, 12, align = "right", na.pad = T),
         roll = signif(roll*100, 2)) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(flow_date, roll, colour = race)) + geom_line()


wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213,
         PRTAGE.x >= 25) %>% 
  mutate(change = wage.y/wage.x -1,
         ed = case_when(PEEDUCA.y <= 39 ~ "HS",
                        PEEDUCA.y %in% 40:42 ~ "some college",
                        PEEDUCA.y >= 43 ~ 'BA')) %>% 
  group_by(flow_date, ed) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race = ed) %>% 
  mutate(roll = rollmean(change, 12, align = "right", na.pad = T),
         roll = signif(roll*100, 2)) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(flow_date, roll, colour = race)) + geom_line()


wage_flow %>% 
  filter(wage.x >= 213,
         wage.y >= 213) %>% 
  mutate(change = wage.y/wage.x -1,
         age = cut(PRTAGE.x, breaks = c(0, 24, 54, 100),
                   labels = c("young", "prime", "older"))) %>% 
  group_by(flow_date, age) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race = age) %>% 
  mutate(roll = rollmean(change, 12, align = "right", na.pad = T),
         roll = signif(roll*100, 2)) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(flow_date, roll, colour = race)) + geom_line()