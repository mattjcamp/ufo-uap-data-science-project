

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

duration_dataset <-
  nuforc_reports %>%
  select(key, duration) %>%
  mutate(
    pattern = str_replace_all(duration, "\\d", "#"),
    numbers = str_extract_all(duration, "\\d+"),
    words = str_replace_all(duration, "\\d", "")
  ) %>%
  filter(
    !is.na(duration),!duration %in% c(
      "?",
      "unknown",
      "-------------------------",
      "----",
      "--",
      "-",
      "you butt holes"
    )
  ) %>%
  mutate(words = str_to_lower(words),
         words = str_extract_all(words, "[a-z]+"))

# Pull out minutes, hours, seconds from lists

d <- duration_dataset 
# %>% filter(key <= 12)
d <- map(.x = d$words, .f = function(x)({
  
  s <- NA

  for (val in x)
      if (val %in% c("minutes", "mins", "min","minute",
                     "mintues","minuts","minuets","minutess",
                     "minites","minuites","minets","mimutes","minues",
                     "miniutes","miutes","minits","mintes","minuted",
                     "minutea","minutos","mnutes","ninutes","mintutes",
                     "mim","mm","mns","mint","minuet","miinutes",
                     "minnutes","minures","minutew","miuntes","mniutes"
                     ))
        s = "minutes"
    
  for (val in x)
      if (val %in% c("seconds", "sec", "secs","second","secounds",
                     "secconds","milliseconds","moment","secods",
                     "secomds","secons","seckonds","momentary",
                     "fast","brief","short","secondss",
                     "minsec","sconds","secinds","secnds",
                     "secondes","seonds","moments","econds",
                     "millisecond"
                     ))
        s = "seconds"

  for (val in x)
      if (val %in% c("hours","hour","hrs","hr"))
        s = "hours"
  
  # s
  if(!is.na(s))
    s
  else
    paste(x,collapse=" ")
  
  }))


l <- unlist(d, recursive = FALSE) %>% 
  as_tibble()%>%
  count(value) %>%
  arrange(n)



d <-
  duration_dataset %>% # head() %>%
  distinct(words) %>%
  arrange(words)

d <-
  duration_dataset %>% # head() %>%
  count(pattern) %>%
  arrange(desc(n))


d <-
  duration_dataset %>% # head() %>%
  filter(words == "")


d <- d %>% filter(key == 56622)
r <- map(
  .x = d$t1,
  .f = function(x)
    (# x[length(x)]
      length(x))
)

l <- unlist(r, recursive = FALSE)
