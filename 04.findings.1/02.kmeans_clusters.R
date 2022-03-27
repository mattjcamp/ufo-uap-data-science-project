
# NOTE key = 75379 contains very interesting case from Newtown Square that may related to 
# 
# the Kecksburg incident. Very 
# key = 35585 is a possible hudson valley style ufo near dover air force base

library(tidyverse)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")

load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# Around 1994 the number of reports went up and the median duration when from around 5 to 3 mins

# Code to pull report text by key
clipr::clear_clip()
nuforc_reports %>%
  filter(key == 14063) %>% 
  select(text) %>% clipr::write_clip()

# EXPLORE KMEANS

df1 <- 
  nuforc_reports %>% 
  filter(!is.na(latitude),
         !is.na(longitude),
         !is.na(duration_time_in_minutes)) 
# %>% 
#   select(key, latitude, longitude, duration_time_in_minutes)
  # select(duration_time_in_minutes)

df <- 
  df1 %>% 
  select(duration_time_in_minutes) %>% 
  scale()

head(df, n = 3)

set.seed(123)
km.res <- kmeans(df, 6, nstart = 25)
print(km.res)

dd <- cbind(df1, cluster = km.res$cluster)

# MAP IT

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

