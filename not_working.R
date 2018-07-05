# Who's not working???

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)
library(scales)

cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

status_gen <- function(x) {
  sapply(x, function(x) {
    if (x %in% c(10, 12)) {status <- "E"} else
      if (x %in% c(20, 21, 22)) {status <- "U"} else
        if (x %in% (30:36)) { status <- "N"} else {status <- NA}
    status})
}

ed_gen <- function(x) {
  sapply(x, function(x) {
    if (x <= 73) {status <- 1} else
      if (x %in% 74:109) {status <- 2} else
        if (x %in% 110:125) { status <- 3} else {status <- NA}
    factor(status, levels = 1:3, labels = c("HS_or_less", "some_college", "BA_plus"))})
}

age_gen <- function(x) {
  sapply(x, function(x) {
    cut(x, breaks = c(0, 24, 54, 64), labels = c("18_to_24", "prime", "55_plus"))})
}

# For race, need two variables.
race_gen <- function(h, r) {
  mapply(function(h, r) {
    if (h > 0) {status <- 1} else
      if (r == 100 ) {status <- 2} else
        if (r == 200) { status <- 3} else 
          if (r %in% 650:652) {status <- 4} else {status <- NA}
    factor(status, levels = 1:4, labels = c("Hispanic", "White", "Black", "Asian"))}, h, r)
}




# Slice of data for 2018
nowork <- cps %>% 
  filter(YEAR == 2018, WTFINL > 0) %>% 
  collect() %>% 
  mutate(date = as.Date(date))

nowork <- nowork %>% 
  mutate(status = status_gen(EMPSTAT), 
         agegroup = age_gen(AGE),
         ed = ed_gen(EDUC),
         race = race_gen(HISPAN, RACE))


# Sice for 2010 (bottom of recession)
nowork_10 <- cps %>% 
  filter(YEAR == 2010, MONTH %in% c(1,2,3), WTFINL > 0) %>% 
  collect() %>% 
  mutate(date = as.Date(date), 
         status = status_gen(EMPSTAT), 
         agegroup = age_gen(AGE),
         ed = ed_gen(EDUC),
         race = race_gen(HISPAN, RACE))

# Sice for 2007 (before recession)
nowork_07 <- cps %>% 
  filter(YEAR == 2007, MONTH %in% c(1,2,3), WTFINL > 0) %>% 
  collect() %>% 
  mutate(date = as.Date(date), 
         status = status_gen(EMPSTAT), 
         agegroup = age_gen(AGE),
         ed = ed_gen(EDUC),
         race = race_gen(HISPAN, RACE))


# quarterly unemployment rate
nowork %>% 
  filter(status != "N") %>% 
  group_by(status) %>% 
  summarize(rate = sum(WTFINL)) %>% 
  mutate(rate = rate/sum(rate))

# We need 10,000 for 2018
sample <- nowork %>% 
  filter(status %in% c("U", "N")) %>% 
  sample_n(10000, weight = WTFINL)

# We want the dots to represent the same number of people in all years, so need to scale
# First: how many people does each dot represent for 2018?
n <- nowork %>% 
  filter(status %in% c("U", "N")) %>% 
  summarize(total = sum(WTFINL/30000)) %>% # Divided by 3 because data represents three months, and by 10,000 b/c 10,000 dots
  as.numeric(.)

n_10 <- nowork_10 %>% 
  filter(status %in% c("U", "N")) %>% 
  summarize(total = sum(WTFINL/30000)) %>% # Divided by 3 because data represents three months, and by 10,000 b/c 10,000 dots
  as.numeric(.)/n

sample_10 <- nowork_10 %>% 
  filter(status %in% c("U", "N")) %>% 
  sample_n(10000*n_10, weight = WTFINL)

n_07 <- nowork_07 %>% 
  filter(status %in% c("U", "N")) %>% 
  summarize(total = sum(WTFINL/30000)) %>% # Divided by 3 because data represents three months, and by 10,000 b/c 10,000 dots
  as.numeric(.)/n

sample_07 <- nowork_07 %>% 
  filter(status %in% c("U", "N")) %>% 
  sample_n(10000*n_07, weight = WTFINL) 

  
rm(nowork_07, nowork_10)

save(sample, sample_07, sample_10, file = "samples_for_charts.RData")


# Simple, just showing unemployed vs NILF
# Functions for generating x & y
xgen <- function(line, rows) {line %% rows -1}
ygen <- function(line, rows) {line %/% rows}


chart <- sample %>% mutate(group = ifelse(status == "U", "Unemployed",
                                          ifelse(AGE <65,"NILF under 65", "NILF, 65+"))) %>% 
  arrange(group) %>% 
  mutate(x = xgen(row_number(), 300),
         y = ygen(row_number(), 300))




save(chart, file = "not_working_chart.RData")


# First let's look at everyone who isn't working
chart %>% 
  ggplot(., aes(x, y)) + geom_point(colour = "white", size = 1) + 
  geom_point(aes(colour = group), size = 0.5) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

nowork %>% filter(status != "E") %>% 
  mutate(group = ifelse(status == "U", "Unemployed",
                        ifelse(AGE <65,"NILF under 65", "NILF, 65+"))) %>% 
  group_by(group) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))

sample_10 %>% filter(status != "E") %>% 
  mutate(group = ifelse(status == "U", "Unemployed",
                        ifelse(AGE <65,"NILF under 65", "NILF, 65+"))) %>% 
  group_by(group) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))

# Zoom in just on unemployed
# Cut by gender
chart %>% 
  filter(status == "U") %>% 
  mutate(SEX = factor(SEX,levels = c(1,2), labels = c("Men", "Women"))) %>% 
  arrange(SEX) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = SEX)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

# Cut by race
chart %>% 
  filter(status == "U") %>% 
  arrange(race) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = race)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

# Cut by ed
chart %>% 
  filter(status == "U") %>% 
  arrange(ed) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = ed)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

# Cut by duration
chart %>% 
  filter(status == "U") %>% 
  mutate(duration = cut(DURUNEMP, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE)) %>% 
  arrange(duration) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = duration)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

sample_10 %>% 
  filter(status == "U") %>% 
  mutate(duration = cut(DURUNEMP, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE)) %>% 
  arrange(duration) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = duration)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 

sample %>% 
  filter(status == "U") %>% 
  mutate(duration = cut(DURUNEMP, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE)) %>%
  group_by(duration) %>% 
  summarize(total = sum(WTFINL)) %>% 
  mutate(share = total/sum(total))

sample_10 %>% 
  filter(status == "U") %>% 
  mutate(duration = cut(DURUNEMP, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE)) %>%
  group_by(duration) %>% 
  summarize(total = sum(WTFINL)) %>% 
  mutate(share = total/sum(total))

sample_07 %>% 
  filter(status == "U") %>% 
  mutate(duration = cut(DURUNEMP, breaks = c(0, 4, 26,125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE)) %>%
  group_by(duration) %>% 
  summarize(total = sum(WTFINL)) %>% 
  mutate(share = total/sum(total))

# Cut by reason
sample %>% 
  filter(status == "U") %>% 
  mutate(reason = cut(WHYUNEMP, breaks = c(0, 3, 4, 6), labels = c("Lost job", "Quit job", "Entering/re-entering workforce"), include.lowest = TRUE)) %>% 
  arrange(reason) %>% 
  mutate(x = xgen(row_number(), 100),
         y = ygen(row_number(), 100)) %>% 
  ggplot(., aes(x, y)) + 
  geom_point(aes(colour = reason)) +
  xlab(NULL) + ylab(NULL) +
  theme_void() 


sample %>% 
  filter(status == "U") %>% 
  mutate(reason = cut(WHYUNEMP, breaks = c(0, 3, 4, 6), labels = c("Lost job", "Quit job", "Entering/re-entering workforce"), include.lowest = TRUE)) %>% 
  group_by(reason) %>% 
  summarize(total = sum(WTFINL)) %>% 
  mutate(share = total/sum(total))

sample_10 %>% 
  filter(status == "U") %>% 
  mutate(reason = cut(WHYUNEMP, breaks = c(0, 3, 4, 6), labels = c("Lost job", "Quit job", "Entering/re-entering workforce"), include.lowest = TRUE)) %>% 
  group_by(reason) %>% 
  summarize(total = sum(WTFINL)) %>% 
  mutate(share = total/sum(total))


nowork %>% 
  filter(status == "N", AGE < 65) %>% 
  group_by(WNLOOK) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))

sample %>% 
  filter(status == "N", MONTH == 3) %>% 
  mutate(want = ifelse(WNLOOK < 999, "want", "no want")) %>% 
  arrange(want)


nowork %>% 
  filter(status == "N", AGE < 65, WNLOOK < 999) %>% 
  mutate(want = cut(WNLOOK, breaks = c(0, 2, 3, 5, 7, 8, 9, 10, 11), 
                    labels = c("No work available", "Lacks necessary training", "Age or other discrimination", "Childcare or other family", "In school/training", "Ill health/disability", "Transportation problems", "Other"))) %>% 
  group_by(want) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))


nowork %>% 
  filter(status == "N", AGE <65, WNLOOK == 999) %>% 
  mutate(group = ifelse(SCHLCOLL %in% c(1,3), "FT_student",
                        ifelse(MARST == 1 & YNGCH < 13, "young_kids", 
                               ifelse(EMPSTAT == 32, "Disabled", "Other")))) %>% 
  group_by(group) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))

nowork_07 %>% 
  filter(AGE <65, WNLOOK == 999) %>% 
  mutate(status = status_gen(EMPSTAT),
         group = ifelse(SCHLCOLL %in% c(1,3), "FT_student",
                        ifelse(MARST == 1 & YNGCH < 13, "young_kids", 
                               ifelse(EMPSTAT == 32, "Disabled", "Other")))) %>% 
  filter(status == "N") %>% 
  group_by(group) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))



nowork %>% 
  filter(status == "N", AGE <65, WNLOOK == 999) %>% 
  mutate(group = ifelse(SCHLCOLL %in% c(1,3), "FT_student",
                        ifelse(MARST == 1 & YNGCH < 13, "young_kids", 
                               ifelse(EMPSTAT == 32, "Disabled", "Other")))) %>% 
  filter(group == "Other") %>% 
  group_by(SEX) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))

nowork_07 %>% 
  filter(AGE <65, WNLOOK == 999) %>% 
  mutate(status = status_gen(EMPSTAT),
         group = ifelse(SCHLCOLL %in% c(1,3), "FT_student",
                        ifelse(MARST == 1 & YNGCH < 13, "young_kids", 
                               ifelse(EMPSTAT == 32, "Disabled", "Other")))) %>% 
  filter(group == "Other", status == "N") %>% 
  group_by(SEX) %>% 
  summarize(total = sum(WTFINL)/3) %>% 
  mutate(share = total/sum(total))



# Extract for Keith
sample %>% 
  mutate(want = cut(WNLOOK, breaks = c(0, 2, 3, 5, 7, 8, 9, 10, 11, 999), 
                    labels = c("No work available", "Lacks necessary training", "Age or other discrimination", "Childcare or other family", "In school/training", "Ill health/disability", "Transportation problems", "Other", NA)),
         duration = cut(DURUNEMP, breaks = c(0, 4, 26, 125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE),
         marital = cut(MARST, breaks = c(0, 2, 5, 6), labels = c("Married", "Divorced/widowed/separated", "Never")),
         student = ifelse(SCHLCOLL %in% c(1,3), "full-time student", 
                          ifelse(SCHLCOLL %in% c(2,4), "part-time student", "non-student")),
         kids = ifelse(YNGCH < 13, "under_13",
                       ifelse(YNGCH < 18, "13-17", "no kids")),
         disabled = ifelse(EMPSTAT == 32, "unable_to_work", NA)) %>% 
  select(AGE, SEX, race, agegroup, ed, status, want, DURUNEMP, duration, marital, student, kids, disabled) %>% 
  write.csv(., file = "sample_for_keith_2018.csv")

sample_07 %>% 
  mutate(want = cut(WNLOOK, breaks = c(0, 2, 3, 5, 7, 8, 9, 10, 11, 999), 
                    labels = c("No work available", "Lacks necessary training", "Age or other discrimination", "Childcare or other family", "In school/training", "Ill health/disability", "Transportation problems", "Other", NA)),
         duration = cut(DURUNEMP, breaks = c(0, 4, 26, 125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE),
         marital = cut(MARST, breaks = c(0, 2, 5, 6), labels = c("Married", "Divorced/widowed/separated", "Never")),
         student = ifelse(SCHLCOLL %in% c(1,3), "full-time student", 
                          ifelse(SCHLCOLL %in% c(2,4), "part-time student", "non-student")),
         kids = ifelse(YNGCH < 13, "under_13",
                       ifelse(YNGCH < 18, "13-17", "no kids")),
         disabled = ifelse(EMPSTAT == 32, "unable_to_work", NA)) %>% 
  select(AGE, SEX, race, agegroup, ed, status, want, DURUNEMP, duration, marital, student, kids, disabled) %>% 
  write.csv(., file = "sample_for_keith_2007.csv")

sample_10 %>% 
  mutate(want = cut(WNLOOK, breaks = c(0, 2, 3, 5, 7, 8, 9, 10, 11, 999), 
                    labels = c("No work available", "Lacks necessary training", "Age or other discrimination", "Childcare or other family", "In school/training", "Ill health/disability", "Transportation problems", "Other", NA)),
         duration = cut(DURUNEMP, breaks = c(0, 4, 26, 125), labels = c("0-4 weeks", "5-26 weeks", "27+ weeks"), include.lowest = TRUE),
         marital = cut(MARST, breaks = c(0, 2, 5, 6), labels = c("Married", "Divorced/widowed/separated", "Never")),
         student = ifelse(SCHLCOLL %in% c(1,3), "full-time student", 
                          ifelse(SCHLCOLL %in% c(2,4), "part-time student", "non-student")),
         kids = ifelse(YNGCH < 13, "under_13",
                       ifelse(YNGCH < 18, "13-17", "no kids")),
         disabled = ifelse(EMPSTAT == 32, "unable_to_work", NA)) %>% 
  select(AGE, SEX, race, agegroup, ed, status, want, DURUNEMP, duration, marital, student, kids, disabled) %>% 
  write.csv(., file = "sample_for_keith_2010.csv")




disab <- cps %>% filter(YEAR >= 2005, MONTH == 12, AGE >= 18) %>% select(YEAR, AGE, SEX, LABFORCE, WTFINL) %>% 
  collect()

age_distr <- disab %>% 
  group_by(AGE, SEX, YEAR) %>% 
  summarize(pop = sum(WTFINL))

age_distr <- disab %>% filter(LABFORCE == 2) %>% 
  group_by(AGE, SEX, YEAR) %>% 
  summarize(labforce = sum(WTFINL)) %>% 
  left_join(age_distr, by = c("AGE", "SEX", "YEAR"))

write.csv(age_distr, file = "~/Taxes/manufacturing/age_distr.csv")
