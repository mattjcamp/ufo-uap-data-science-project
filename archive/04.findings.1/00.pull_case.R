
# NOTE key = 75379 contains very interesting case from Newtown Square that may related to 
# 
# the Kecksburg incident. Very 
# key = 35585 is a possible hudson valley style ufo near dover air force base

library(tidyverse)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")

load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# Code to pull report text by key
clipr::clear_clip()
nuforc_reports %>%
  filter(key == 14063) %>% 
  select(text) %>% clipr::write_clip()
