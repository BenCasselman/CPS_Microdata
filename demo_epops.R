# Employment rate among non-college prime-age men

library(tidyverse)
library(lubridate)
library(RSQLite)
library(zoo)
library(dbplyr)
library(scales)
library(tidyseasonal)

cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

# First get all prime-age

primeage <- cps %>% 
  filter(AGE %in% 25:54) %>% 
  select(date, WTFINL, SEX, EDUC, EMPSTAT) %>% 
  collect()

primeage <- primeage %>% 
  mutate(college = ifelse(EDUC >= 111, "college", "noncollege"),
         emp = ifelse(EMPSTAT %in% c(10,12), WTFINL, 0))

# epops by sex and education
epops <- primeage %>% 
  group_by(date, SEX, college) %>% 
  summarize(epop = sum(emp)/sum(WTFINL)) %>% 
  ungroup() %>% mutate(date = as.Date(date))

# Prelim chart
epops %>% ungroup() %>% 
  mutate(demo = paste0(college, "-", SEX)) %>% 
  ggplot(., aes(date, epop, colour = demo)) + geom_line()

# Need to seasonally adjust (which is a bit of a pain)
epops_sa <- epops %>% ungroup() %>% 
  mutate(demo = paste0(college, "_", SEX)) %>% 
  select(date, demo, epop) %>% 
  spread(demo, epop)

epops_sa$college_1 <- seas_adjust(epops_sa, date, college_1)
epops_sa$college_2 <- seas_adjust(epops_sa, date, college_2)
epops_sa$noncollege_1 <- seas_adjust(epops_sa, date, noncollege_1)
epops_sa$noncollege_2 <- seas_adjust(epops_sa, date, noncollege_2)

epops_sa %>% 
  gather(demo, value, - date) %>% 
  ggplot(., aes(date, value, colour = demo)) + geom_line()

# Change since recession
epops_sa %>% 
  gather(demo, value, - date) %>% 
  filter(date >= ymd("2007-12-01")) %>% 
  group_by(demo) %>% 
  mutate(change = value/value[1] - 1) %>% 
  ggplot(., aes(date, change, colour = demo)) + geom_line()
  


epops2 <- primeage %>% 
  group_by(date, college) %>% 
  summarize(epop = sum(emp)/sum(WTFINL)) %>% 
  ungroup() %>% mutate(date = as.Date(date))

epops2 <- epops2 %>% 
  spread(college, epop)

epops2$college_SA <- seas_adjust(epops2, date, college)
epops2$noncollege_SA <- seas_adjust(epops2, date, noncollege)

epops2 %>% 
  filter(date >= ymd("2007-12-01")) %>% 
  select(date, college_SA, noncollege_SA) %>% 
  gather(group, value, -date) %>% 
  group_by(group) %>% 
  mutate(change = value/value[1] - 1) %>% 
  ggplot(., aes(date, change, colour = group)) + geom_line()


epops2 %>% 
  filter(date >= ymd("2007-12-01")) %>% 
  select(date, college_SA, noncollege_SA) %>% 
  write.csv(., file = "epop_data.csv")




# Young black men
cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

w <- cps %>% 
  filter(SEX == 1, AGE >= 18, AGE < 35, RACE == 200, !is.na(WTFINL)) %>% 
  collect() %>% 
  mutate(date = as.Date(date))

w <- w %>% mutate(emp = status_gen(EMPSTAT))

# epop
blackmen <- w %>% 
  mutate(epop = ifelse(emp == "E", WTFINL, 0)) %>% 
  filter(!is.na(emp)) %>% 
  group_by(date) %>% 
  summarize(epop = sum(epop)/sum(WTFINL)) 

blackmen %>% 
  mutate(roll = rollmean(epop, 12, na.pad = TRUE, align = "right")) %>%
  ggplot(., aes(date, roll)) + geom_line()

  
  
  
# Now we'll do this for a wider array of demographic groups.
# We'll split by race, sex, age and education.
cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")
memory.limit(size = 25000)

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

epop_demo <- cps %>% 
  filter(YEAR >= 2000, AGE %in% 18:64) %>% 
  select(YEAR, MONTH, date, EMPSTAT, RACE, HISPAN, AGE, SEX, EDUC, WTFINL, NATIVITY, SCHLCOLL, UHRSWORKT) %>% 
  collect()

epop_demo <- epop_demo %>% 
  mutate(date = as.Date(date),
         emp = status_gen(EMPSTAT),
         agegroup = age_gen(AGE),
         ed = ed_gen(EDUC),
         race = race_gen(HISPAN, RACE))

epop_deom <- epop_demo %>% 
  mutate(sex_age = paste0(SEX, "_", agegroup),
         sex_race = paste0(SEX, "_", race),
         sex_ed = paste0(SEX, "_", ed),
         sex_age_ed = paste0(SEX, "_", agegroup, "_", ed),
         race_ed = paste0(race, "_", ed),
         race_age_ed = paste0(race, "_", agegroup),
         age_ed = paste0(agegroup, "_", ed),
         all = paste0(SEX, "_", race, "_", agegroup, "_", ed))


# OK, that uses way too much memory. So trying this another way.
# Start with DF with each demo
# 3 sexes * 5 races * 4 ages * 4 eds = 240 demo groups
epop_demo <- tibble(date = NA, epop = NA, sex = NA, race = NA, age = NA, ed = NA)

sexes <- list(All = 1:2, Men = 1, Women = 2)
races <- list("All", "White", "Black", "Hispanic", "Asian")
# ages <- list(All = 18:64, age18_24 = 18:24, prime = 25:54, age55_plus = 55:64)
ages <- list(prime = 25:54, age25_34 = 25:34, age35_44 = 35:44, age45_54 = 45:54)
eds <- list(All = 0:125, HS_or_less = 0:73, some_college = 74:109, BA_plus = 110:125)

i <- 1

for (sex in 1:3) {
  for (race in 1:5) {
    for (age in 1:4) {
      for (ed in 1:4) {
        if (race == 1) {
          w <- cps %>% 
            filter(YEAR >= 2000, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]]) %>% 
            select(date, EMPSTAT, WTFINL) %>% 
            collect() 
        } else if (race == 4) {
          w <- cps %>% 
            filter(YEAR >= 2000, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]],
                   HISPAN %in% 100:412) %>% 
            select(date, EMPSTAT, WTFINL) %>% 
            collect()
        } else if (race == 2) {
          w <- cps %>% 
            filter(YEAR >= 2000, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]],
                   HISPAN == 0,
                   RACE == 100) %>% 
            select(date, EMPSTAT, WTFINL) %>% 
            collect()
        } else if (race == 3) {
          w <- cps %>% 
            filter(YEAR >= 2000, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]],
                   HISPAN == 0,
                   RACE == 200) %>% 
            select(date, EMPSTAT, WTFINL) %>% 
            collect()
        } else if (race == 5) {
          w <- cps %>% 
            filter(YEAR >= 2000, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]],
                   HISPAN == 0,
                   RACE %in% 650:652) %>% 
            select(date, EMPSTAT, WTFINL) %>% 
            collect()
        }
        w <- w %>% mutate(emp = ifelse(EMPSTAT %in% c(10, 12), WTFINL, 0)) %>% 
          group_by(date) %>% 
          summarize(epop = sum(emp)/sum(WTFINL)) %>% 
          mutate(date = as.Date(date))
        
        w$sex <- names(sexes)[sex]
        w$race <- races[[race]]
        w$age <- names(ages)[age]
        w$ed <- names(eds)[ed]
        
        epop_demo <- bind_rows(epop_demo, w)
        rm(w)
        
        i <- i+1
        print(i)
        
      }
    }
  }
}
epop_demo <- epop_demo[-1,]

save(epop_demo, file = "epop_by_demo.RData")

# Age and race
epop_demo %>%
  filter(ed == "All", sex == "All", race != "Asian", race != "All", age != "All") %>% 
  mutate(demo = paste0(race, "_", age)) %>% 
  group_by(demo) %>% 
  arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  ggplot(., aes(date, roll, colour =demo)) + geom_line()

# Benchmark to start of recession
epop_demo %>% 
  filter(date >= ymd("2007-12-01"),
         ed == "All", sex == "All", race != "Asian", race != "All", age != "All") %>% 
  mutate(demo = paste0(race, "_", age)) %>% 
  group_by(demo) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = demo)) + geom_line() + scale_y_continuous(label = percent)

# Benchmark to start of recession
epop_demo %>% 
  filter(date >= ymd("2007-12-01"),
         ed == "All", sex == "All", race != "All", age != "All") %>% 
  group_by(race, age) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = race)) + geom_line(size = 1) + scale_y_continuous(label = percent) + facet_wrap(~age)

epop_demo %>% 
  filter(date >= ymd("2007-12-01"),
         ed != "All", sex != "All", race == "All", age == "prime") %>% 
  mutate(ed = factor(ed, levels = c("HS_or_less", "some_college", "BA_plus"), labels = c("High school or less", "Some college", "BA-plus"))) %>% 
  group_by(ed, sex) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = sex)) + geom_line(size = 1) + scale_y_continuous(label = percent) + facet_wrap(~ed) +
  labs(title = "Prime-age employment rates by sex and education",
       subtitle = "Change from Dec. 2007, 12-month rolling average",
       x = NULL, y = NULL)

epop_demo %>% 
  filter(date >= ymd("2001-01-01"),
         ed != "All", sex != "All", race == "All", age == "prime") %>% 
  mutate(ed = factor(ed, levels = c("HS_or_less", "some_college", "BA_plus"), labels = c("High school or less", "Some college", "BA-plus"))) %>% 
  group_by(ed, sex) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = sex)) + geom_line(size = 1) + scale_y_continuous(label = percent) + facet_wrap(~ed) +
  labs(title = "Prime-age employment rates by sex and education",
       subtitle = "Change from Jan. 2001, 12-month rolling average",
       x = NULL, y = NULL)


epop_demo %>% 
  filter(date >= ymd("2007-12-01"),
         ed == "All", sex != "All", race == "All", age != "All") %>% 
  group_by(age, sex) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = age)) + geom_line(size = 1) + scale_y_continuous(label = percent) + facet_wrap(~sex) 


epop_demo %>% 
  filter(date >= ymd("2007-12-01"),
         ed != "All", sex == "All", race != "All", age == "prime") %>% 
  mutate(ed = factor(ed, levels = c("HS_or_less", "some_college", "BA_plus"), labels = c("High school or less", "Some college", "BA-plus"))) %>% 
  group_by(race, ed) %>% arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = race)) + geom_line(size = 1) + scale_y_continuous(label = percent) + facet_wrap(~ed) 




# Compare black with college degree to white without
epop_demo %>% 
  filter(age != "prime") %>% 
  mutate(group = ifelse(race == "White" & ed == "some_college", "Some college white",
                        ifelse(race == "Black" & ed == "BA_plus", "College black", NA))) %>% 
  filter(!is.na(group)) %>% 
  group_by(group, age, sex) %>% 
  arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = group)) + geom_line() + facet_wrap(~age + sex) +
  ggtitle("Employment rates of more-educated African Americans vs less-educated whites")



# Compare black with college degree to white without
epop_demo %>% 
  filter(age != "prime") %>% 
  mutate(group = ifelse(race == "White" & ed == "HS_or_less", "HS white",
                        ifelse(race == "Black" & ed == "BA_plus", "College black", NA))) %>% 
  filter(!is.na(group)) %>% 
  group_by(group, age, sex) %>% 
  arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = group)) + geom_line() + facet_wrap(~age + sex) +
  ggtitle("Employment rates of more-educated African Americans vs less-educated whites")


recession_shade <- function(startdate) {
  load("c:/users/208546/Documents/FRED/recession_dates.RData")
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = 0.2)
}

epop_demo %>% 
  filter(race %in% c("White", "Black", "Hispanic"), age == "prime", ed != "All") %>% 
  group_by(race, ed, sex) %>% 
  arrange(date) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_wrap(~sex + ed) +
  ggtitle("Employment rates by race") + recession_shade("2000-01-01")


# Full-time vs part-time by race, ed
# Want employed FT, employed PT, employed total, unemployed, pop



lf_demo <- tibble(date = NA, sex = NA, race = NA, age = NA, ed = NA, marital = NA, FT = NA, PT = NA, unemp = NA, pop = NA)

sexes <- list(Men = 1, Women = 2)
races <- list("White", "Black", "Hispanic", "Asian", "Other")
ages <- list(age16_17 = 16:17, age18_24 = 18:24, age25_34 = 25:34, age35_44 = 35:44, age45_54 = 45:54, age_55_64 = 55:64, age65_plus = 65:99)
eds <- list(less_than_HS = 0:71, HS_GED = 72:73, some_college = 74:109, BA_plus = 110:125)
marital <- list(married = 1:2, div_sep_wid = 3:5, never = 6)

i <- 1

for (sex in 1:2) {
  for (race in 1:5) {
    for (age in 1:7) {
      for (ed in 1:4) {
        for(mar in 1:3) {
          w <- cps %>% 
            filter(YEAR >= 1995, 
                   SEX %in% sexes[[sex]],
                   AGE %in% ages[[age]],
                   EDUC %in% eds[[ed]],
                   MARST %in% marital[[mar]]) %>% 
            select(date, RACE, HISPAN, EMPSTAT, UHRSWORK1, WTFINL) %>% 
            collect() 
          
          if (race == 5) {
            w <- w %>% 
              filter(HISPAN == 0,
                     (RACE == 300 | RACE >= 700)) 
            
          } else if (race == 3) {
            w <- w %>% 
              filter(HISPAN %in% 100:412) 
            
          } else if (race == 1) {
            w <- w %>% 
              filter(HISPAN == 0,
                     RACE == 100) 
            
          } else if (race == 2) {
            w <- w %>% 
              filter(HISPAN == 0,
                     RACE == 200)
            
          } else if (race == 4) {
            w <- w %>% 
              filter(HISPAN == 0,
                     RACE %in% 650:652) 
          }
          w <- w %>% mutate(FT = ifelse(EMPSTAT %in% c(10,12) & UHRSWORK1 >= 35, WTFINL, 0),
                            PT = ifelse(EMPSTAT %in% c(10,12) & UHRSWORK1 < 35, WTFINL, 0),
                            unemp = ifelse(EMPSTAT %in% 21:22, WTFINL, 0)) %>% 
            group_by(date) %>% 
            summarize(FT = sum(FT),
                      PT = sum(PT),
                      unemp = sum(unemp),
                      pop = sum(WTFINL)) %>% 
            mutate(date = as.Date(date))
          
          if (nrow(w) > 0) {
          w$sex <- names(sexes)[sex]
          w$race <- races[[race]]
          w$age <- names(ages)[age]
          w$ed <- names(eds)[ed]
          w$marital <- names(marital)[mar]
          
          lf_demo <- bind_rows(lf_demo, w) }
          rm(w)
          
          print(i)
          i <- i+1
          
        }
      }
    }
  }
}

lf_demo <- lf_demo[-1,]
lf_demo <- lf_demo %>% 
  mutate(emp = FT + PT,
         NILF = pop - (emp + unemp))

save(lf_demo, file = "lf_by_demo.RData")

# prime-age black male epop
lf_demo %>% 
  filter(sex == "Men", age %in% c("age25_34", "age35_44", "age45_54"), race == "Black") %>%
  group_by(date) %>% 
  summarize(emp = sum(emp), pop = sum(pop)) %>% 
  mutate(epop = emp/pop)

# Full-time employment rate by race and ed
lf_demo %>% 
  filter(age %in% c("age25_34", "age35_44", "age45_54"), race %in% c("Black", "White", "Hispanic")) %>% 
  group_by(date, race, ed, sex) %>% 
  summarize(FT = sum(FT), PT = sum(PT), pop = sum(pop)) %>% 
  mutate(FT = FT/pop,
         PT = PT/pop,
         emp = (FT + PT)/pop) %>% 
  arrange(date) %>% 
  group_by(race, ed, sex) %>% 
  mutate(roll = rollmean(FT, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_wrap(~ed + sex) +
  recession_shade("1995-12-01") + ggtitle("Full-time employment rate by race")



lf_demo %>% 
  filter(age %in% c("age25_34", "age35_44", "age45_54"), race %in% c("Black", "White", "Hispanic")) %>% 
  group_by(date, race, ed, sex) %>% 
  summarize(FT = sum(FT), PT = sum(PT), pop = sum(pop)) %>% 
  mutate(PT = PT/pop,
         emp = (FT + PT)/pop) %>% 
  arrange(date) %>% 
  group_by(race, ed, sex) %>% 
  mutate(roll = rollmean(FT, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_wrap(~ed + sex) +
  recession_shade("1995-12-01")



lf_demo %>% 
  filter(age %in% c("age25_34", "age35_44", "age45_54"), race %in% c("Black", "White", "Hispanic")) %>% 
  mutate(ed = factor(ed, levels = c("HS_or_less", "HS_GED", "some_college", "BA_plus"), labels = c("Less than HS", "HS diploma or GED", "Some college or associate", "BA or higher"))) %>% 
  group_by(date, race, ed, sex, age) %>% 
  summarize(FT = sum(FT), PT = sum(PT), pop = sum(pop)) %>% 
  mutate(FT = FT/pop,
         PT = PT/pop,
         emp = (FT + PT)/pop) %>% 
  arrange(date) %>% 
  group_by(race, ed, sex, age) %>% 
  mutate(roll = rollmean(FT, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_grid(ed ~ age) +
  recession_shade("1995-12-01") + ggtitle("Full-time employment rate by race")



lf_demo %>% 
  filter(age %in% c("age25_34", "age35_44", "age45_54"), race %in% c("Black", "White", "Hispanic"), ed == "BA_plus") %>% 
  group_by(date, race, sex, marital) %>% 
  summarize(FT = sum(FT), PT = sum(PT), pop = sum(pop)) %>% 
  mutate(FT = FT/pop,
         PT = PT/pop,
         emp = (FT + PT)/pop) %>% 
  arrange(date) %>% 
  group_by(race, marital, sex) %>% 
  mutate(roll = rollmean(FT, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_grid(marital ~ sex) +
  recession_shade("1995-12-01") + ggtitle("Full-time employment rate among BA holders, by race")


lf_demo %>% 
  filter(age %in% c("age25_34", "age35_44", "age45_54"), race %in% c("Black", "White", "Hispanic")) %>% 
  group_by(date, race, ed, marital) %>% 
  summarize(FT = sum(FT), PT = sum(PT), pop = sum(pop)) %>% 
  mutate(FT = FT/pop,
         PT = PT/pop,
         emp = (FT + PT)/pop) %>% 
  arrange(date) %>% 
  group_by(race, marital, ed) %>% 
  mutate(roll = rollmean(FT, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + facet_grid(marital ~ ed) +
  ggtitle("Full-time employment rate by education and race")


# Build generalized version in SHINY
shiny_data <- lf_demo %>% 
  mutate(age = factor(age, 
                      levels = c("age16_17", "age18_24", "age25_34", "age35_44", "age45_54", "age_55_64", "age65_plus"),
                      labels = c("Ages 16-17", "Ages 18-24", "Ages 25-34", "Ages 35-44", "Ages 45-54", "Ages 55-64", "Ages 65-plus")),
         ed = factor(ed, 
                     levels = c("less_than_HS", "HS_GED", "some_college", "BA_plus"), 
                     labels = c("Less than high school", "HS diploma or GED", "Some college or associate", "BA or higher")),
         marital = factor(marital,
                          levels = c("never", "married", "div_sep_wid"),
                          labels = c("Never married", "Married", "Previously married")),
         lf = emp + unemp,
         race = factor(race,
                       levels = c("White", "Black", "Hispanic", "Asian", "Other")))


save(shiny_data, file = "shiny_data.RData")




lf_demo %>% 
  group_by(date, race) %>% 
  summarize(epop = sum(emp)/sum(pop)) %>% 
  arrange(date) %>% 
  group_by(race) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = race)) + geom_line() + 
  recession_shade("1995-12-01")


library(ggvis)
lf_demo %>% 
  group_by(date, race) %>% 
  summarize(epop = sum(emp)/sum(pop)) %>% 
  arrange(date) %>% 
  group_by(race) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  ggvis(~date, ~ roll, stroke = ~race) %>% 
  layer_paths()




library(rsconnect)
deployApp()



shiny_data %>% 
  group_by(date, ed) %>% 
  filter(age %in% c("Ages 25-34", "Ages 35-44", "Ages 45-54"),
         date >= ymd("2011-01-01")) %>%
  summarize(epop = sum(emp)/sum(pop)) %>% 
  arrange(date) %>% 
  spread(ed, epop) %>% 
  write.csv(., file = "employment_by_ed.csv")
  
  
  group_by(ed) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = ed)) + geom_line() +
  ggtitle("In recent years, the strongest employment gains have been among less-educated workers")
  
  + 
  recession_shade("1995-12-01")


