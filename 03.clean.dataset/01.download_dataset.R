
library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)

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

# TRIED TO FIX TIMEZONE IN OCCURRED FIELD BUT NOTHING WOULD WORK

# %>% 
#   mutate(time_zone = tz_lookup_coords(lat = latitude, lon = longitude, method = "accurate"))
# 
# # ISSUE: APPEARS TO ONLY ASSIGN THE FIRST TZ IN A LIST TO THE NEW DATE
# # (EVEN THOUGH IT ASSIGNS THE CORRECT TEXT TO DATAFRAME)
# 
# for (i in 1:nrow(nuforc_reports)){
#   state <- nuforc_reports$state[i]
#   this_timezone <- as.character(nuforc_reports$time_zone[i])
#   this_date <- nuforc_reports$occurred[i]
#   # new_date <- ymd_hms(this_date, tz = this_timezone)
#   if(!is.na(this_timezone)){
#   new_date <- ymd("2021-03-03", tz = this_timezone)
#   } else {
#     new_date <- ymd("2021-03-03")
#   }
#   # force_tz(new_date, tzone = this_timezone)
#   if (i == 1){
#     new_dates <- tibble(state, this_date, this_timezone, 
#                         new_date, new_tz = tz(new_date))
#   } else {
#     new_dates <- bind_rows(new_dates, tibble(state, this_date, 
#                                              this_timezone, new_date, 
#                                              new_tz = tz(new_date)))
#   }
#   state <- NA
#   this_timezone <- NA
#   this_date <- NA
#   new_date <- NA
# 
# }

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
         words = str_extract_all(words, "[a-z]+"),
         word_category = map_chr(.x = words, .f = function(x)({
  
  s <- NA

  for (val in x)
      if (val %in% c("minutes", "mins", "min","minute",
                     "mintues","minuts","minuets","minutess",
                     "minites","minuites","minets","mimutes","minues",
                     "miniutes","miutes","minits","mintes","minuted",
                     "minutea","minutos","mnutes","ninutes","mintutes",
                     "mim","mm","mns","mint","minuet","miinutes",
                     "minnutes","minures","minutew","miuntes","mniutes",
                     "m","munites","mi","mims","minitues","minnute","minuates",
                     "minuetes","minuits","minuntes","minurwa","minustes","minuters",
                     "minutis","minuutes","monutes","ms","tominutes","minuit","minutue",
                     "minu","miniuts","minns","minea","tomins","mints","minutestriangle",
                     "minuite","imin","iminute","menutes","miin","miites","mina","minents",
                     "minet","minetes","miniute","minonds","mnute","muntes","munutes","muinte",
                     "miuets","miunets","miunute","miute","miuts","mlnutes","minurtes","minut",
                     "minuteds","minutee","minutees","minutese","minuteswhile","minutets"
                     ,"minutez","minuties","minutres","minuttes","minutues","mintue","mintute",
                     "minsorlonger","inutes"
                     ))
        s = "minutes"
    
  for (val in x)
      if (val %in% c("seconds", "sec", "secs","second","secounds",
                     "secconds","milliseconds","moment","secods",
                     "secomds","secons","seckonds","momentary",
                     "fast","brief","short","secondss",
                     "minsec","sconds","secinds","secnds",
                     "secondes","seonds","moments","econds",
                     "millisecond","aboutseconds","secodns","seconda",
                     "secondds","seconnds","secpnds","secunds","toseconds",
                     "quick","quickly","approxsec","aproxsecs","seco",
                     "tosecs","tosec","ceconds","deconds","desonds",
                     "secon","milisec","milisecod","sseconds","ssecs",
                     "sesconds","seounds","segs","segundos","senconds","seocds","seoonds",
                     "secands","secants","seceonds","secionds","seconcs","secondsss",
                     "seconfs","secopnd","secounts","secsonds","secthen","secx",
                     "sceonds","scounds","secaond","mseconds","seeconds","blink",
                     "secends","seconts","instantaneous","instant","flash"
                     
                     ))
        s = "seconds"

  for (val in x)
      if (val %in% c("hours","hour","hrs","hr","hous","tohour",
                     "hiours","hm","horas","houres","hourish","hoursmin",
                     "onehour","houl","housr","nours","houra","hrmin"
                     
                     ))
        s = "hours"

  for (val in x)
    if (val %in% c("days","day","year","years","month","months","lifes","life","week",
                   "summer","nite","winter","daily","allways","ongoing","weeks",
                   "daytime","months","weeks","every","months","nights","yrs",
                   "wks","night"))
      s = "day_or_more"
  s
  
  })),
  numbers_length = map_int(.x = numbers, .f = function(x)({
    length(x)
  }))) %>% 
  filter(numbers_length <= 2) %>%  # REMOVE LONG SERIES OF NUMBERS SINCE THEY WERE TOO MESSED UP
  mutate(numbers = map_dbl(.x = numbers, .f = function(x)({
    n <- NA
    l <- length(x)
    if(l == 2)
      n <- (as.numeric(x[1]) + as.numeric(x[2])) / 2
    if(l == 1)
      n <- as.numeric(x[1])
    n

  }))) %>% 
  select(key, duration_time = numbers, duration_unit = word_category)

nuforc_reports <- 
  nuforc_reports %>% 
  left_join(duration_dataset, by = "key")

nuforc_reports %>% 
  count(duration_unit) %>% 
  arrange(duration_unit)

save(nuforc_reports,
     file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))



