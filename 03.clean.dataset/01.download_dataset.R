
library(tidyverse)

datadir <- sprintf("%s/%s", here::here(), "03.clean.dataset/data")

# library(data.world)
# saved_cfg <- save_config(Sys.getenv("data.world_apikey")) # API key stored in .Renviron file
# set_config(saved_cfg)
# 
# nuforc_reports_download <- query(
#   qry_sql("
#     SELECT * FROM nuforc_reports"),
#   dataset = "https://data.world/timothyrenner/ufo-sightings"
# ) %>%
#   mutate(key = row_number()) %>%
#   select(key, occurred = date_time, state, city, duration, shape,
#          latitude = city_latitude, longitude = city_longitude,
#          text, date_posted = posted)
# 
# save(nuforc_reports_download,
#      file = sprintf("%s/%s", datadir, "nuforc_reports_download.rdata"))

load(file = sprintf("%s/%s", datadir, "nuforc_reports_download.rdata"))

# 136,937 records

# Use state dataset to only include valid US States

nuforc_reports <- 
  nuforc_reports_download %>% 
  filter(state %in% state.abb)

# 121,832 records

# Fix dates

library(lubridate)

nuforc_reports <- 
  nuforc_reports %>% 
  mutate(occurred = ymd_hms(occurred),
         date_posted = ymd(date_posted))

# Fix duration field

# d1 = str_replace_all(duration,"\\d", "#")

duration_dataset <- 
  nuforc_reports %>% 
  select(key, duration) %>% 
  mutate(
    pattern = str_replace_all(duration,"\\d", "#"),
    numbers = str_extract_all(duration, "\\d+"),
    words = str_replace_all(duration, "\\d", "")) %>% 
  filter(!is.na(duration),
         !duration %in% c(
           "?",
           "unknown",
           "-------------------------",
           "----",
           "--",
           "-",
           "you butt holes"))

d <-   
  duration_dataset %>% # head() %>%
  distinct(pattern) %>% 
  arrange(pattern)

d <-   
  duration_dataset %>% # head() %>%
  count(pattern) %>% 
  arrange(desc(n))


d <- d %>% filter(key == 56622)
r <- map(.x = d$t1, .f = function(x) (
  # x[length(x)]
  length(x)
))

l <- unlist(r, recursive=FALSE)

