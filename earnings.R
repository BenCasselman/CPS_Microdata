# Earnings by decile

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)
library(scales)

cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

earnings <- cps %>% 
  filter(EARNWEEK <= 9999.98) %>% 
  select(AGE, SEX, RACE, HISPAN, EDUC, date, EARNWEEK, HOURWAGE, PAIDHOUR, UHRSWORKORG, UHRSWORKT, AHRSWORKT, WTFINL)

earnings <- earnings %>% 
  collect()

# Function to calculate hourly earnings
hourwage <- function(HOURWAGE, EARNWEEK, UHRSWORKORG, UHRSWORKT, AHRSWORKT) {
  mapply(function(HOURWAGE, EARNWEEK, UHRSWORKORG, UHRSWORKT, AHRSWORKT) {
    if (HOURWAGE < 99.99) { e <- HOURWAGE} else
      if (UHRSWORKORG > 0 & UHRSWORKORG < 998) {e <- EARNWEEK/UHRSWORKORG} else
        if (UHRSWORKT > 0 & UHRSWORKT < 997) {e <- EARNWEEK/UHRSWORKT} else
          if (AHRSWORKT > 0 & AHRSWORKT < 999) { e <- EARNWEEK/AHRSWORKT} else {e <- NA}
    as.numeric(e)}, 
    HOURWAGE, EARNWEEK, UHRSWORKORG, UHRSWORKT, AHRSWORKT)
}

earnings <- earnings %>% 
  mutate(wage = hourwage(HOURWAGE, EARNWEEK, UHRSWORKORG, UHRSWORKT, AHRSWORKT))

# Split into deciles

earnings <- earnings %>% filter(!is.na(wage)) %>% 
  group_by(date) %>% 
  mutate(deciles = as.numeric(cut(wage, breaks = quantile(wage, probs = seq(0,1, by = .1)), include.lowest = F)))

earnings <- earnings %>% filter(!is.na(wage)) %>% 
  group_by(date) %>% 
  mutate(quintiles = as.numeric(cut(wage, breaks = quantile(wage, probs = seq(0,1, by = .2)), include.lowest = F)))


earnings %>% 
  filter(wage > 10, wage < 12) %>% 
  select(date, wage, deciles) %>% View()


deciles <- earnings %>% 
  mutate(weighted = wage * WTFINL) %>% 
  group_by(date, deciles) %>% 
  summarize(wage = sum(weighted)/sum(WTFINL))

deciles %>% 
  group_by(deciles) %>% 
  mutate(date = as.Date(date), change = wage/lag(wage, 12)) %>% 
  filter(!is.na(change)) %>% 
  mutate(smooth = rollmean(change, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(smooth)) %>% 
  ggplot(., aes(date, smooth, colour = factor(deciles))) + geom_line()

quintiles <- earnings %>% 
  mutate(weighted = wage * WTFINL) %>% 
  group_by(date, quintiles) %>% 
  summarize(wage = sum(weighted)/sum(WTFINL))

quintiles %>% 
  filter(!is.na(quintiles)) %>% 
  group_by(quintiles) %>% 
  mutate(date = as.Date(date), change = wage/lag(wage, 12)) %>% 
  filter(!is.na(change)) %>% 
  mutate(smooth = rollmean(change, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(smooth)) %>% 
  ggplot(., aes(date, smooth, colour = factor(quintiles))) + geom_line()


quintiles %>% ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  filter(!is.na(quintiles), date >= ymd("2012-01-01")) %>% 
  group_by(quintiles) %>% 
  mutate(change = wage/wage[1] -1,
         smooth = rollmean(change, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(smooth)) %>% 
  ggplot(., aes(date, smooth, colour = factor(quintiles))) + geom_line()