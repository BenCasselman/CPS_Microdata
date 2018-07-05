# Age discrimination
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)

flow_data <- src_sqlite("cps", create = FALSE) %>% tbl("flow")

unemp <- flow_data %>% filter(m1 == "U") %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, SEX.x, DURUNEMP.x, OCC.x, OCC.y) %>% 
  collect() 



# Job finding by age
age_find <- unemp %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 24, 54, 64, 100), labels = c("Under 25", "25-54", "54-64", "65+"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% 
  group_by(date, agegroup, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% group_by(agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line() + recession_shade("1995-01-01") + scale_y_continuous(label = percent)

cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")

age_find <- age_find + labs(x = NULL, y = NULL,
         title = "Job-finding rate by age group",
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
  theme(legend.position = "top", legend.background = element_blank(), legend.key = element_blank(), legend.title = element_blank())

ggsave("age_find.png", age_find, device = "png", width = 7.1, height = 4)


unemp %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 24, 54, 64, 100), labels = c("Under 25", "25-54", "54-64", "65+"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% filter(date >= ymd("2007-01-01")) %>% 
  group_by(date, agegroup, m2) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% group_by(agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  mutate(change = roll/roll[1] -1) %>% 
  ggplot(., aes(date, change, colour = agegroup)) + geom_line() + scale_y_continuous(label = percent)



+
  annotate("text", x = ymd("2014-06-01"), y = .37, label = "bold(`Under 25`)", parse = TRUE, colour = "#a6cee3", size = 3) +
  annotate("text", x = ymd("2014-06-01"), y = .242, label = "bold(`Age 25-54`)", parse = TRUE, colour = "#1f78b4", size = 3) +
  annotate("text", x = ymd("2014-06-01"), y = .142, label = "bold(`Age 55-64`)", parse = TRUE, colour = "#b2df8a", size = 3) +
  annotate("text", x = ymd("2014-06-01"), y = .142, label = "bold(`Age 65+`)", parse = TRUE, colour = "#33a02c", size = 3) 

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

# Older finding rates by occupation
ipums_occs <- read_csv("~/occupations/ipums_occs.csv")

# First find top occupations
top_25 <- cps %>% 
  filter(AGE  %in% 55:70, YEAR >= 2015) %>% collect() %>% 
  group_by(OCC) %>% 
  summarize(total = sum(WTFINL)) %>% 
  arrange(desc(total)) %>% 
  slice(1:20) %>% 
  select(OCC)


unemp %>% 
  filter(AGE.x  %in% 25:70, OCC.x %in% top_25$OCC) %>% 
  mutate(date = as.Date(date.y), year = year(date), 
         agegroup = cut(AGE.x, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE)) %>% 
  filter(year >= 2005) %>% 
  group_by(year, OCC.x, m2, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(year, OCC.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% 
  left_join(ipums_occs, by = c("OCC.x" = "OCC")) %>%  
  ggplot(., aes(year, share, colour = agegroup)) + geom_line() + facet_wrap(~Occ_name) +
  ggtitle("Job finding rates by age for top 20 occupations for seniors")


worst <- 
  unemp %>% 
  filter(AGE.x  %in% 25:70, year(as.Date(date.y)) >= 2012) %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE)) %>% 
  group_by(OCC.x, m2, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(OCC.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% select(OCC.x, agegroup, share) %>% 
  spread(agegroup, share) %>% 
  mutate(ratio = Older/Prime) %>% ungroup() %>% 
  arrange(ratio) %>% 
  slice(1:20) %>% 
  select(OCC.x)

unemp %>% 
  filter(AGE.x  %in% 25:70, OCC.x %in% worst$OCC.x) %>% 
  mutate(date = as.Date(date.y), year = year(date), 
         agegroup = cut(AGE.x, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE)) %>% 
  filter(year >= 2005) %>% 
  group_by(year, OCC.x, m2, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(year, OCC.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% 
  left_join(ipums_occs, by = c("OCC.x" = "OCC")) %>%  ungroup() %>%
  ggplot(., aes(year, share, colour = agegroup)) + geom_line() + facet_wrap(~Occ_name) 


# Durations of unemployment by occupation
cps <- src_sqlite("cps", create = FALSE) %>% tbl("cps_main")

duration <- cps %>% 
  filter(EMPSTAT %in% c(20, 21, 22), YEAR >= 2003) %>% collect() %>% 
  mutate(date = as.Date(date), 
         agegroup = cut(AGE, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE)) %>% 
  group_by(YEAR, agegroup, OCC) %>% 
  summarize(duration = median(rep(DURUNEMP, times = WTFINL/100)))

duration %>% filter(OCC %in% top_25$OCC) %>% 
  spread(agegroup, duration) %>% 
  mutate(ratio = Older/Prime) %>% 
  left_join(ipums_occs, by = "OCC") %>% 
  ggplot(., aes(YEAR, ratio, colour = Occ_name)) + geom_line()


duration %>% filter(OCC %in% top_25$OCC, YEAR == 2017) %>% 
  spread(agegroup, duration) %>% 
  mutate(ratio = Older/Prime) %>% 
  left_join(ipums_occs, by = "OCC") %>% 
  arrange(desc(ratio))



ipums_inds <- read_csv("ipums_inds.csv")

top_inds <- cps %>% 
  filter(AGE  %in% 55:70, YEAR >= 2015) %>% collect() %>% 
  group_by(IND) %>% 
  summarize(total = sum(WTFINL)) %>% 
  arrange(desc(total)) %>% 
  slice(1:100) %>% 
  select(IND)

duration_ind <- cps %>% 
  filter(EMPSTAT %in% c(20, 21, 22), YEAR >= 2003) %>% collect() %>% 
  mutate(date = as.Date(date), 
         agegroup = cut(AGE, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE)) %>% 
  group_by(YEAR, agegroup, IND) %>% 
  summarize(duration = median(rep(DURUNEMP, times = WTFINL/100)))

duration_ind %>% filter(YEAR == 2017, IND %in% top_inds$IND) %>% 
  spread(agegroup, duration) %>% 
  mutate(ratio = Older/Prime) %>% 
  left_join(ipums_inds, by = c("IND" = "ind_code")) %>% 
  arrange(desc(ratio)) %>% View("industries")

# Human resources
duration_ind %>% filter(IND == 9480) %>% 
  spread(agegroup, duration) %>% 
  mutate(ratio = Older/Prime) %>% 
  ggplot(., aes(YEAR, ratio)) + geom_line()

duration %>% filter(OCC %in% c(136, 630)) %>% 
  spread(agegroup, duration) %>% 
  mutate(ratio = Older/Prime) %>% 
  ggplot(., aes(YEAR, ratio, colour = factor(OCC))) + geom_line()





unemp %>% filter(AGE.x >= 25, AGE.x<65, OCC.x %in% c(136, 630)) %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE),
         date = as.Date(date.y)) %>% filter(year(date) >= 2011) %>% 
  group_by(date, agegroup, m2) %>%
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>% 
  ggplot(., aes(date, share, colour = agegroup)) + geom_line() 


# Unemployment rate
HR <- cps %>% 
  filter(YEAR >= 2011, EMPSTAT %in% c(10, 12, 20, 21, 22), AGE >= 25) %>% 
  select(YEAR, date, AGE, OCC, IND, EMPSTAT, WTFINL, DURUNEMP) %>% 
  collect() %>% 
  mutate(date = as.Date(date),
         HR = ifelse(IND == 9480, "HR", "Other"),
         emp = ifelse(EMPSTAT %in% c(10,12), "employed", "unemployed"),
         agegroup = cut(AGE, breaks = c(0, 54, 100), labels = c("Prime", "Older"), include.lowest = TRUE))

HR %>% 
  group_by(YEAR, HR, emp, agegroup) %>% 
  summarize(total = sum(WTFINL)) %>% 
  group_by(YEAR, HR, agegroup) %>% 
  mutate(UER = total/sum(total)) %>% 
  filter(emp == "unemployed") %>% 
  ggplot(., aes(YEAR, UER, colour = agegroup)) + geom_line() + facet_wrap(~HR)

HR %>% filter(emp == "unemployed") %>% 
    group_by(YEAR, HR, agegroup) %>% 
    summarize(total = sum(WTFINL*DURUNEMP)/sum(WTFINL)) %>% 
    ggplot(., aes(YEAR, total, colour = agegroup)) + geom_line() + facet_wrap(~HR)


HR %>% filter(emp == "unemployed", IND %in% top_inds$IND, YEAR == 2017) %>% 
  group_by(IND, agegroup) %>% 
  summarize(total = sum(WTFINL*DURUNEMP)/sum(WTFINL)) %>% 
  spread(agegroup, total) %>% 
  mutate(ratio = Older/Prime) %>% 
  arrange(desc(ratio)) %>% 
  left_join(ipums_inds, by = c("IND" = "ind_code")) %>% 
  View()

  
  
  ggplot(., aes(YEAR, total, colour = agegroup)) + geom_line() + facet_wrap(~HR)
  
  
  
ed_breakdown <- cps %>% 
  filter(YEAR %in% c(2007, 2017), AGE >= 18, BPL < 15000) %>% 
  collect() %>% 
  mutate(ed = cut(EDUC, breaks = c(0, 71, 73, 92, 111, 125), labels = c("Less than HS", "HS/GED", "Some college", "BA", "Grad degree")),
         agegroup= cut(AGE, breaks = c(0, 24, 34, 44, 54, 64, 999), labels = c("Less than 25", "25-34", "35-44", "45-54", "55-64", "65+")))

ed_breakdown %>% filter(AGE >= 25, AGE < 45) %>% 
  group_by(ed, YEAR) %>% 
  summarize(share = sum(WTFINL)) %>% group_by(YEAR) %>% 
  mutate(share = share/sum(share)) %>% 
  spread(YEAR, share)





nilf_flow <- flow_data %>% 
  filter(m1 == "N", AGE.x < 75, AGE.x >= 16, YEAR.x >= 2000) %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, SEX.x, MARST.x) %>% 
  collect() 

nilf_flow <- nilf_flow %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 24, 34, 44, 54, 64, 100), labels = c("Under 25", "25-34", "35-44", "45-54", "54-64", "65-74"), include.lowest = TRUE),
         date = as.Date(date.y))

agesex <- nilf_flow %>% filter(AGE.x >= 25) %>% mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women"))) %>% 
  group_by(date, m2, SEX.x, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>%  group_by(SEX.x, agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(~SEX.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows from not-in-labor-force to employment",
       subtitle = "As share of total NILF, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")


agesexmar <- nilf_flow %>% filter(AGE.x >= 25) %>% 
  mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women")), 
         MARST.x = `levels<-`(factor(MARST.x), list(Married = c(1,2), `Prev. married` = c(3,4,5), `Never married` = 6))) %>% 
  group_by(date, m2, SEX.x, agegroup, MARST.x) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup, MARST.x) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(m2 == "E") %>%  group_by(SEX.x, agegroup, MARST.x) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(SEX.x ~ MARST.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows from not-in-labor-force to employment",
       subtitle = "As share of total NILF, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")

ggsave("age_sex.png", agesex, device = "png")
ggsave("age_sex_marital.png", agesexmar, device = "png")



agesex2 <- nilf_flow %>% filter(AGE.x >= 25) %>% 
  mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women")),
         lf2 = ifelse(m2 %in% c("E", "U"), "LF", "NILF")) %>% 
  group_by(date, lf2, SEX.x, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(lf2 == "LF") %>%  group_by(SEX.x, agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(~SEX.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows into labor force",
       subtitle = "As share of total NILF, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")


agesexmar2 <- nilf_flow %>% filter(AGE.x >= 25) %>% 
  mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women")), 
         MARST.x = `levels<-`(factor(MARST.x), list(Married = c(1,2), `Prev. married` = c(3,4,5), `Never married` = 6)),
         lf2 = ifelse(m2 %in% c("E", "U"), "LF", "NILF")) %>% 
  group_by(date, lf2, SEX.x, agegroup, MARST.x) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup, MARST.x) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(lf2 == "LF") %>%  group_by(SEX.x, agegroup, MARST.x) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(SEX.x ~ MARST.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows into labor force",
       subtitle = "As share of total NILF, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")

ggsave("age_sex2.png", agesex2, device = "png")
ggsave("age_sex_marital2.png", agesexmar2, device = "png")



lf_flow <- flow_data %>% 
  filter((m1 == "E" | m1 == "U"), AGE.x < 75, AGE.x >= 16, YEAR.x >= 2000) %>% 
  select(date.y, m1, m2, flow, flow_weight, AGE.x, SEX.x, MARST.x) %>% 
  collect() 

lf_flow <- lf_flow %>% 
  mutate(agegroup = cut(AGE.x, breaks = c(0, 24, 34, 44, 54, 64, 100), labels = c("Under 25", "25-34", "35-44", "45-54", "54-64", "65-74"), include.lowest = TRUE),
         date = as.Date(date.y))


agesex3 <- lf_flow %>% filter(AGE.x >= 25) %>% 
  mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women")),
         lf2 = ifelse(m2 %in% c("E", "U"), "LF", "NILF")) %>% 
  group_by(date, lf2, SEX.x, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(lf2 == "NILF") %>%  group_by(SEX.x, agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(~SEX.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows out of labor force",
       subtitle = "As share of total labor force, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")


ggsave("age_sex3.png", agesex3, device = "png")


p <- lf_flow %>% filter(AGE.x >= 25, AGE.x < 65) %>%
  mutate(SEX.x = factor(SEX.x, labels = c("Men", "Women")),
         lf2 = ifelse(m2 %in% c("E", "U"), "LF", "NILF")) %>% 
  group_by(date, lf2, SEX.x, agegroup) %>% 
  summarize(total = sum(flow_weight)) %>% 
  group_by(date, SEX.x, agegroup) %>% 
  mutate(share = total/sum(total)) %>% 
  filter(lf2 == "NILF") %>%  group_by(SEX.x, agegroup) %>% 
  mutate(roll = rollmean(share, 12, na.pad = TRUE, align = "right")) %>% filter(!is.na(roll)) %>% 
  ggplot(., aes(date, roll, colour = agegroup)) + geom_line(size = 1) + facet_wrap(~SEX.x) + 
  labs(x = NULL, y = NULL,
       title = "Monthly flows out of labor force, ages 25-64",
       subtitle = "As share of total labor force, 12-month rolling average",
       caption = "Source: Current Population Survey via IPUMS") +
  scale_y_continuous(label = percent) + recession_shade("2003-01-01")

ggsave("test.png", p, device = "png")