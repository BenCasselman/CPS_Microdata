# Add recession shading to plots.
library(tidyverse)
library(lubridate)

# recessions <- read_csv("recession_dates.csv")
# save(recessions, file = "recession_dates.RData")

# Function. "startdate" must be in "yyyy-mm-dd" format.
recession_shade <- function(startdate) {
  load("recession_dates.RData")
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = 0.2)
}