# Black wage growth for Jeanna
library(tidyverse)
library(dtplyr)
library(lubridate)
library(bigvis)
library(scales)

atlanta <- read_csv("cps_00060.csv.gz") %>% 
  lazy_dt()

atlanta <- atlanta %>% 
  mutate(date = ymd(paste0(YEAR, "-", MONTH, "-01")))

# Function to create longitudinal links
linker <- function(startdate, df = .) {
  startdate <- ymd(startdate)
  enddate <- startdate + years(1)
  
  # Split by months and merge
  flow <- df %>% filter(date == enddate) %>% mutate(CPSIDP = as.character(CPSIDP))
  flow <- df %>% filter(date == startdate) %>% 
    mutate(CPSIDP = as.character(CPSIDP)) %>% 
    inner_join(flow, by = "CPSIDP")
  
  flow <- flow %>%
    filter(SEX.x == SEX.y,
           RACE.x == RACE.y,
           AGE.y >= AGE.x,
           AGE.y < (AGE.x + 3))

  flow

}

linker("2015-01-01", atlanta) %>% 
  as_tibble

atlanta_flow <- seq.Date(ymd("1990-01-01"),
         ymd("2018-12-01"),
         by = "month") %>% 
  map_dfr(~linker(.x, atlanta) %>% as_tibble) %>% 
  lazy_dt()

atlanta_flow %>% 
  as_tibble() %>% 
  save(atlanta_flow = ., file = "atlanta_flow_data.RData")

atlanta %>% 
  as_tibble() %>% 
  save(., file = "atlanta_data.RData")

# atlanta_flow_add <- seq.Date(ymd("2018-10-01"),
#                          ymd("2018-12-01"),
#                          by = "month") %>%
#   map_dfr(~linker(.x, atlanta) %>% as_tibble) %>%
#   lazy_dt()
# # 
# load("atlanta_flow_data.RData")
# atlanta_flow <- .
# atlanta_flow <- atlanta_flow_add %>%
#   as_tibble() %>%
#   mutate_if(is.logical, as.character) %>% 
#   bind_rows(atlanta_flow) %>% 
#   lazy_dt()
# 
# rm(atlanta_flow_add, .)


# rm(atlanta)

# Initial attempt
atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1) %>%
  as_tibble() %>%
  group_by(date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  ggplot(., aes(date.y, avg)) + geom_line()


race_replicate <- atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         race = case_when(RACE.x == 100 ~ "white",
                          TRUE ~ "non-white")) %>%
  as_tibble() %>%
  group_by(race, date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  ggplot(., aes(date.y, avg, colour = race)) + geom_line(size = 0.8)

race_detail <- atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         race = case_when(HISPAN.x > 0 ~ "Hispanic",
                          RACE.x == 100 ~ "White",
                          RACE.x == 200 ~ "Black",
                          TRUE ~ "All other")) %>%
  filter(race != "All other") %>% 
  as_tibble() %>%
  group_by(race, date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  ggplot(., aes(date.y, avg, colour = race)) + geom_line(size = 0.8)

race_replicate +
  labs(x = NULL, y = NULL,
       title = "Wage Growth Tracker by Race (Replicating Atlanta Fed)",
       subtitle = "12-month moving average of median wage growth",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(labels = percent, limits = c(.01, .06)) +
  recession_shade("1998-01-01")
  
race_detail +
  labs(x = NULL, y = NULL,
       title = "Wage Growth Tracker by Detailed Race (Extending Atlanta Fed)",
       subtitle = "12-month moving average of median wage growth",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(labels = percent) +
  recession_shade("1998-01-01")



atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99,
         AGE.y %in% 25:54) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         race = case_when(HISPAN.x > 0 ~ "Hispanic",
                          RACE.x == 100 ~ "White",
                          RACE.x == 200 ~ "Black",
                          TRUE ~ "All other")) %>%
  filter(race != "All other") %>% 
  as_tibble() %>%
  group_by(race, date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(race) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  ggplot(., aes(date.y, avg, colour = race)) + geom_line(size = 0.8) +
  labs(x = NULL, y = NULL,
       title = "*Prime Age* Wage Growth Tracker by Detailed Race (Extending Atlanta Fed)",
       subtitle = "12-month moving average of median wage growth",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(labels = percent) +
  recession_shade("1998-01-01")



# Black workers by sex
atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99,
         RACE.x == 200,
         HISPAN.x == 0) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         SEX = factor(SEX.x, levels = c(1,2), labels = c("Men", "Women"))) %>%
  as_tibble() %>%
  group_by(SEX, date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(SEX) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  ggplot(., aes(date.y, avg, colour = SEX)) + geom_line(size = 0.8) +
  labs(x = NULL, y = NULL,
       title = "Wage growth by black workers, by sex",
       subtitle = "12-month trailing average of median wage growth",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(labels = percent) +
  recession_shade("1998-01-01")


# Black workers by education
atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99,
         RACE.x == 200,
         HISPAN.x == 0) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         ed = case_when(EDUC.x <= 73 ~ "HS or less",
                        EDUC.x %in% 74:109 ~ "Some college/AA",
                        EDUC.x < 999 ~ "BA+",
                        TRUE ~ "other")) %>%
  filter(ed != "other") %>% 
  as_tibble() %>%
  group_by(ed, date.y) %>% 
  summarize(change = median(change, na.rm = T)) %>% 
  group_by(ed) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  ggplot(., aes(date.y, avg, colour = ed)) + geom_line(size = 0.8) +
  labs(x = NULL, y = NULL,
       title = "Wage growth by black workers, by education",
       subtitle = "12-month trailing average of median wage growth",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(labels = percent) +
  recession_shade("1998-01-01")



atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99,
         RACE.x == 200,
         HISPAN.x == 0) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         ed = case_when(EDUC.x <= 73 ~ "HS or less",
                        EDUC.x %in% 74:109 ~ "Some college/AA",
                        EDUC.x < 999 ~ "BA+",
                        TRUE ~ "other")) %>%
  filter(ed != "other") %>% 
  as_tibble() %>%
  group_by(ed, date.y) %>% 
  summarize(n = n()) %>% 
  spread(ed, n) %>% 
  arrange(desc(date.y))



distrib <- atlanta %>% 
  filter(HOURWAGE != 999.99,
         YEAR %in% 2013:2018,
         HOURWAGE >= 7.25) %>% 
  group_by(YEAR) %>% 
  mutate(quintile = as.numeric(cut(HOURWAGE, 
                                 breaks = quantile(HOURWAGE, probs = seq(0, 1, .2)),
                                 include.lowest = T)),
         race = case_when(HISPAN > 0 ~ "Hispanic",
                          RACE == 100 ~ "White",
                          RACE == 200 ~ "Black",
                          TRUE ~ "All other")) %>% 
  count(race, YEAR, quintile) %>% 
  as_tibble


distrib %>% 
  filter(race == "Black") %>%
  as_tibble() %>% 
  ggplot(., aes(quintile, n)) +
  geom_col() + facet_wrap(~YEAR) +
  labs(x = NULL, y = NULL,
       title = "Distribution of black worker wages by overall wage quintile, by year",
       caption = "Source: Current Population Survey via IPUMS")


distrib %>% 
  ggplot(., aes(quintile, n)) +
  geom_col() + facet_wrap(~YEAR) 


black_distrib <- atlanta %>% 
  filter(RACE == 200, HISPAN == 0,
         HOURWAGE != 999.99,
         YEAR %in% 2013:2018,
         HOURWAGE >= 7.25) %>% 
  as_tibble() %>% 
  ggplot(., aes(log(HOURWAGE))) +
  geom_density() + facet_wrap(~YEAR)

black_distrib +
  labs(x = NULL, y = NULL,
       title = "Distribution of wages by black workers, by year",
       subtitle = "Kernel density of log wages among black workers earning at least $7.25/hr",
       caption = "Source: Current Population Survey via IPUMS")


all_race_distrib <- atlanta %>% 
  filter(HOURWAGE != 999.99,
         YEAR %in% 2010:2018,
         HOURWAGE >= 7.25) %>% 
  mutate(race = case_when(HISPAN > 0 ~ "Hispanic",
                          RACE == 100 ~ "White",
                          RACE == 200 ~ "Black",
                          TRUE ~ "All other")) %>% 
  filter(race != "All other") %>% 
  as_tibble() %>% 
  ggplot(., aes(HOURWAGE, colour = race)) +
  geom_density(size = 0.8) + facet_wrap(~YEAR) +
  scale_x_log10()

all_race_distrib +
  labs(x = NULL, y = NULL,
       title = "Distribution of wages byworkers, by race/ethnicity and year",
       subtitle = "Kernel density of wages among workers earning at least $7.25/hr. X axis shows log scale.",
       caption = "Source: Current Population Survey via IPUMS")

library(gganimate)
# devtools::install_github("thomasp85/transformr")
library(transformr)
distrib_anim <- atlanta %>% 
  filter(HOURWAGE != 999.99,
         YEAR %in% 2010:2019,
         HOURWAGE >= 7.25) %>% 
  mutate(race = case_when(HISPAN > 0 ~ "Hispanic",
                          RACE == 100 ~ "White",
                          RACE == 200 ~ "Black",
                          TRUE ~ "All other")) %>% 
  filter(race != "All other") %>% 
  as_tibble() %>% 
  ggplot(., aes(HOURWAGE, colour = race)) +
  geom_density(size = 0.8)

distrib_anim +
  transition_states(YEAR,
                    transition_length = 1,
                    state_length = 1) +
  scale_x_log10() +
  labs(x = NULL, y = NULL,
       title = "Year: {closest_state}",
       subtitle = "Kernel density of wages among workers earning at least $7.25/hr. X axis shows log scale.",
       caption = "Source: Current Population Survey via IPUMS")





# Young black men vs others
cps <- src_mysql(dbname = "cass_cps_microdata",
                 host = "127.0.0.1",
                 user = user,
                 password = password) %>% 
  tbl("cps_main")

epops <- cps %>% 
  filter(AGE %in% 18:64,
         YEAR >= 2005) %>% 
  select(YEAR, MONTH, WTFINL, EMPSTAT, AGE, SEX, MARST, RACE, HISPAN, EDUC) %>% 
  collect()

epops <- epops %>% 
  as_tibble() %>% 
  mutate(agegroup = case_when(AGE < 25 ~ "18-24",
                              AGE < 35 ~ "25-34",
                              AGE < 45 ~ "35-44",
                              AGE < 55 ~ "45-54",
                              AGE < 65 ~ "55-64"),
         race = case_when(HISPAN > 0 ~ "Hispanic",
                          RACE == 100 ~ "White",
                          RACE == 200 ~ "Black",
                          TRUE ~ "All other"),
         sex = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female"),
         working = case_when(EMPSTAT %in% 10:12 ~ WTFINL,
                             TRUE ~ 0),
         date = ymd(paste(YEAR, MONTH, 1, sep = "-"))
         )

demo_epop <- epops %>% 
  group_by(date, race, agegroup, sex) %>% 
  summarize(epop = sum(working)/sum(WTFINL)) %>% 
  ungroup() 

demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  ggplot(., aes(date, roll, colour = race)) +
  geom_line() + facet_wrap(agegroup ~ sex, scales = "free_y")


demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = roll - first(roll)) %>% 
  ggplot(., aes(date, change, colour = race)) +
  geom_line() + facet_wrap(agegroup ~ sex) +
  geom_hline(yintercept = 0, linetype = "dashed")


demo_epop %>% 
  filter(race %in% c("White", "Black"),
         agegroup %in% c("25-34", "35-44", "45-54")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = roll - first(roll)) %>% 
  ggplot(., aes(date, change, colour = race)) +
  geom_line(size = 0.8) + facet_wrap(agegroup ~ sex) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Change in employment rates by age, sex and race")


p1 <- demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic"),
         agegroup %in% c("25-34", "35-44", "45-54")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = 100*(roll - first(roll)),
         demo_group = paste(sex, race, agegroup, sep = "-"),
         ybm = case_when(sex == "Male" & race == "Black" & agegroup == "25-34" ~ "young_black_men",
         TRUE ~ "other"),
         demo_group = factor(demo_group)) %>% 
  ggplot(., aes(date, change, group = fct_reorder(demo_group, ybm, .desc = T), colour = ybm)) +
  geom_line(size = 0.8) + 
  scale_colour_manual(values = c(young_black_men = "blue", other = "grey75")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, 
       title = "Catching Up",
       subtitle = "Young black men saw their employment rate fall more than any other demographic group in the recession.\nBut in recent years, they have made strong gains.",
       caption = "Source: Current Population Survey via IPUMS") +
  recession_shade("2007-01-01") +
  annotate("text", x = ymd("2009-06-01"), y = 7, 
           label = as.character("Change in employment rate since January 2007"), 
           colour = "black", size = 6) +
  theme(legend.position = "none",
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        strip.text = element_text(size = 16))

ggsave("epop.png", p1, device = "png", width = 14.2, height = 8)


p2 <- demo_epop %>% 
  filter(race %in% c("White", "Black"),
         agegroup == "25-34", 
         sex == "Male") %>% 
  group_by(race) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = 100*(roll - first(roll)))

p2 <- epops %>% 
  filter(race %in% c("White", "Black")) %>% 
  group_by(date, race) %>% 
  summarize(epop = sum(working)/sum(WTFINL)) %>% 
  group_by(race) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = 100*(roll - first(roll)),
         agegroup = "total",
         sex = "total") %>% 
  bind_rows(p2)

p2c <- p2 %>% 
  mutate(demo_group = case_when(race == "Black" & sex == "Male" ~ "Young black men",
                                race == "White" & sex == "Male" ~ "Young white men",
                                race == "Black" ~ "Black total",
                                race == "White" ~ "White total")) %>% 
  ggplot(., aes(date, change, colour = demo_group)) +
  geom_line(size = 0.8) + 
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, 
       title = "Catching Up",
       subtitle = "Young black men saw their employment rate fall more than any other demographic group in the recession.\nBut in recent years, they have made strong gains.",
       caption = "Source: Current Population Survey via IPUMS") +
  recession_shade("2007-01-01") +
  annotate("text", x = ymd("2009-06-01"), y = 7, 
           label = as.character("Change in employment rate since January 2007"), 
           colour = "black", size = 6) +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        strip.text = element_text(size = 16))

ggsave("epop2.png", p2c, device = "png", width = 14.2, height = 8)




demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic"),
         agegroup %in% c("25-34", "35-44", "45-54")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = roll - first(roll),
         demo_group = paste(sex, race, agegroup, sep = "-")) %>% 
  filter(date == ymd("2019-12-01")) %>% 
  arrange(change)

atlanta_flow %>% 
  filter(HOURWAGE.x != 999.99,
         HOURWAGE.y != 999.99) %>% 
  mutate(change = HOURWAGE.y/HOURWAGE.x - 1,
         race = case_when(HISPAN.x > 0 ~ "Hispanic",
                          RACE.x == 100 ~ "White",
                          RACE.x == 200 ~ "Black",
                          TRUE ~ "All other")) %>%
  filter(race != "All other") %>% 
  as_tibble() %>%
  group_by(race, date.y) %>% 
  summarize(change = 100*median(change, na.rm = T)) %>% 
  group_by(race) %>% 
  mutate(avg = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
  filter(year(date.y) >= 1998) %>% 
  select(-change) %>% 
  spread(race, avg) %>% 
  write.csv("wage_growth_by_race.csv")



demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic"),
         agegroup %in% c("25-34", "35-44", "45-54")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = 100*(roll - first(roll)),
         demo_group = paste(sex, race, agegroup, sep = "-"),
         demo_group = factor(demo_group)) %>% 
  select(date, sex, race, agegroup, epop, roll_avg = roll, change) %>% 
  write.csv("epops_for_guilbert.csv")



demo_epop %>% 
  filter(race %in% c("White", "Black", "Hispanic"),
         agegroup %in% c("25-34", "35-44", "45-54")) %>% 
  group_by(race, agegroup, sex) %>% 
  mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>% 
  filter(year(date) >= 2007) %>% 
  mutate(change = 100*(roll - first(roll)),
         demo_group = paste(sex, race, agegroup, sep = "-"),
         ybm = case_when(sex == "Male" & race == "Black" & agegroup == "25-34" ~ "young_black_men",
                         TRUE ~ "other"),
         demo_group = factor(demo_group)) %>% 
  ungroup() %>% 
  select(date, demo_group, change) %>% 
  spread(demo_group, change) %>% 
  write.csv("epops_for_mcm.csv")

save(epops, file = "race_epops_alldata.RData")
# rm(epops)

unemp <- read_csv("cps_00061.csv.gz")
save(unemp, file = "race_unemp.RData")

unemp <- unemp %>% 
  lazy_dt() %>% 
  filter(HISPAN == 0) %>% 
  select(YEAR, MONTH, WTFINL, RACE, EMPSTAT)

bw_uer <- unemp %>% 
  mutate(unemp = case_when(EMPSTAT %in% c(20, 21, 22) ~ WTFINL,
                           TRUE ~ 0),
         race = case_when(RACE == 100 ~ "White",
                          RACE == 200 ~ "Black"),
         date = ymd(paste(YEAR, MONTH, 1, sep = "-"))) %>% 
  group_by(date, race) %>% 
  summarize(UER = sum(unemp)/sum(WTFINL))

rm(unemp)

bw_uer <- bw_uer %>% 
  as_tibble()

bw_uer %>% 
  ggplot(., aes(date, UER, colour = race)) + geom_line()

library(tidyseasonal)

bw_uer <- bw_uer %>% 
  group_by(race) %>% 
  nest() %>% 
  group_by(race) %>% 
  mutate(sa = map(data, ~seas_adjust(.x, date, UER))) %>% 
  unnest

bw_uer %>% 
  ggplot(., aes(date, sa, colour = race)) + geom_line()

bw_uer %>% 
  gather(series, value, -race, -date) %>% 
  ggplot(., aes(date, value, colour = series)) + geom_line() +
  facet_wrap(~race)

bw_uer %>% 
  filter(year(date) >= 1990) %>% 
  mutate(sa = sa * 100) %>% 
  select(date, race, sa) %>% 
  spread(race, sa) %>% 
  write.csv("new_unemp_data.csv")
