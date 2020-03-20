# 2017 contingent worker supplement
# Code modified from standard microdata parser
library(tidyverse)
library(lubridate)


# Set up function to download:

# This should work, but doesn't, apparently because of errors in source documentation:
# cw_download <- function(url) {
#   lengths <- read_csv("cws_lengths.csv", col_types = "cii")
#   
#   temp <- tempfile()
#   download.file(url, temp)
#   
#   working <- read_fwf(unzip(temp, unzip(temp, list = T)[[1]]), 
#                       fwf_positions(lengths$start, lengths$end, lengths$series_id))
#   
#   working <- working %>% 
#     mutate(date = ymd(paste(HRYEAR4, HRMONTH, 1, sep = "-")),
#            personid = paste(HRHHID, HRHHID2, PULINENO, sep = "-"))
#   
#   working$PWSUPWGT <- as.numeric(working$PWSUPWGT)
#   working$PWCMPWGT <- as.numeric(working$PWCMPWGT)
#   
#   return(working)
#   
# }

# This does seem to work:
cw_download <- function(url) {
  lengths <- read_csv("cws_lengths_fixed.csv", col_types = "ccii")
  
  temp <- tempfile()
  download.file(url, temp)
  
  working <- read_fwf(unzip(temp, unzip(temp, list = T)[[1]]), 
                      fwf_positions(lengths$start, lengths$end, lengths$series_id))
  
  working <- working %>% 
    mutate(date = ymd(paste(HRYEAR4, HRMONTH, 1, sep = "-")),
           personid = paste(HRHHID, HRHHID2, PULINENO, sep = "-"))
  
  working$PWSUPWGT <- as.numeric(working$PWSUPWGT)
  working$PWCMPWGT <- as.numeric(working$PWCMPWGT)
  
  return(working)
  
}


# Download/parse data:
cws_url <- "https://thedataweb.rm.census.gov/pub/cps/supps/may17pub.zip"
cws <- cw_download(cws_url)

# Check to make sure unweighted tallies match Attachment 13 here: https://www2.census.gov/programs-surveys/cps/techdocs/cpsmay17.pdf
cws %>% 
  group_by(PRCONDF3) %>% 
  summarize(n = n())

# Check this to make sure matches published data: https://www.bls.gov/news.release/conemp.t01.htm
cws2 %>% filter(PEMLR %in% c(1,2)) %>% 
  group_by(PRCONDF1) %>% 
  summarize(total = sum(PWSUPWGT/10000))

cws %>% filter(PEMLR %in% c(1,2), PRTAGE %in% 25:34) %>% 
  group_by(PRCONDF1) %>% 
  summarize(total = sum(PWSUPWGT/10000))

# And here: https://www.bls.gov/news.release/conemp.t05.htm
cws %>% filter(PEMLR %in% c(1,2)) %>% 
  group_by(PRIC) %>% 
  summarize(total = sum(PWSUPWGT/10000))

cws %>% filter(PEMLR %in% c(1,2), PRTAGE %in% 25:34, PESEX == 1) %>% 
  group_by(PRIC) %>% 
  summarize(total = sum(PWSUPWGT/10000))

cws %>% filter(PRIC != -1) %>% 
  mutate(ed = cut(PEEDUCA, breaks = c(0, 39, 42, 43, 46), labels = c("HS_or_less", "some_college", "BA", "grad_degree")),
         agegroup = cut(PRTAGE, breaks = c(0, 18, 21, 24, 34, 44, 54, 64, 100))) %>% 
  group_by(ed, agegroup, PRIC) %>% 
  summarize(share = sum(PWSUPWGT/10000)) %>% 
  group_by(ed, agegroup) %>% 
  mutate(share = 100*share/sum(share)) %>% 
  filter(PRIC == 1) %>% 
  spread(ed, share)

save(cws, file = "cws_data.RData")
  

# ANALYSIS

cws %>% filter(PEMLR %in% c(1,2), PRERELG == 1, PRIC != -1, !is.na(PRSCWKLY), PRSCWKLY>= 0) %>% 
  mutate(ptft = ifelse(PEHRUSL1 >= 35, "full_time", 
                       ifelse(PEHRUSL1 == -4 & PEHRFTPT == 1, "full_time", "part_time"))) %>% 
  group_by(PRIC, ptft) %>% 
  summarize(median = median(rep(PRSCWKLY/100, times = PWSORWGT/10000)))

cws %>% filter(PEMLR %in% c(1,2), PRIC != -1) %>% 
  mutate(ptft = ifelse(PEHRUSL1 >= 35, "full_time", 
                       ifelse(PEHRUSL1 == -4 & PEHRFTPT == 1, "full_time", "part_time"))) %>% 
  group_by(PRIC, ptft) %>% 
  summarize(total = sum(PWSUPWGT/10000))




cws2 %>% filter(!is.na(PWSUPWGT)) %>% 
  group_by(PESOEM) %>% 
  summarize(n = sum(PWSUPWGT))


cws2 %>%
  group_by(PESOEM) %>% 
  summarize(total = sum(PWSUPWGT/10000))
