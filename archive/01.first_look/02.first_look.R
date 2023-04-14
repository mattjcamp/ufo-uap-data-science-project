library(tidyverse)

load("nuforc_reports.rdata")

glimpse(nuforc_reports)

# CLEAN DATA

nuforc_reports %>% summarize(min(posted, na.rm = TRUE))

library(lubridate)

nuforc_reports <- 
  nuforc_reports %>% 
  mutate(date_time = ymd_hms(date_time),
         posted = ymd(posted))


