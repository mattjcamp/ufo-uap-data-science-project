# TODO
# - ADD DETAILED FIELDS UP FRONT AND ONLY IN ONE PLACE

library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")
load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# 1.Build dataset specifically for analyzing duration

ufo_duration <-
  nuforc_reports %>%
  head(100) %>%
  mutate(
    duration_time_in_seconds =
      case_when(
        duration_unit == "seconds" ~ duration_time,
        duration_unit == "minutes" ~ duration_time * 60,
        duration_unit == "hours" ~ duration_time * 60 * 60,
        TRUE ~ -1
      ),
    duration_time_in_seconds = round(ifelse(duration_time_in_seconds == -1, NA, duration_time_in_seconds)),
    duration_time_in_minutes = round(duration_time_in_seconds / 60, 2),
    duration_time_in_hours = round(duration_time_in_minutes / 60, 2),
    occurred_day_of_week = wday(occurred),
    occurred_day_of_month = day(occurred),
    occurred_hour = hour(occurred),
    occurred_minute = minute(occurred),
    occurred_second = second(occurred),
    occurred_day_of_year = yday(occurred),
    occurred_month = month(occurred),
    occurred_year = year(occurred),
    occurred_time_zone = tz_lookup_coords(lat = latitude, lon = longitude, method = "accurate"),
    date_formated = sprintf("%s-%s-%s %s:%s:%s", occurred_year, occurred_month, 
                            occurred_day_of_month, occurred_hour, occurred_minute, 
                            occurred_second)
    
  )

sunrise_set_time_for_date_tz <- function(date_string, 
                                         timezone_string,
                                         longitude,
                                         latitude,
                                         sunrise_or_sunset = "sunrise"){
  
  m_ufo <- matrix(c(longitude, latitude), nrow = 1)
  for_date <- as.POSIXct(date_string, tz = timezone_string)
  s <- sunriset(m_ufo, for_date, direction=sunrise_or_sunset, POSIXct.out = TRUE)
  s$time

}

i <- 1
ufo_duration$sunrise <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                        ufo_duration$occurred_time_zone[i],
                                                        ufo_duration$longitude[i],
                                                        ufo_duration$latitude[i],
                                                        "sunrise")
ufo_duration$sunset <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                       ufo_duration$occurred_time_zone[i],
                                                       ufo_duration$longitude[i],
                                                       ufo_duration$latitude[i],
                                                       "sunset")

for(i in 1:nrow(ufo_duration)){
  if(!is.na(ufo_duration$occurred_time_zone[i]) & !is.na(ufo_duration$occurred[i])){
    ufo_duration$sunrise[i] <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                            ufo_duration$occurred_time_zone[i],
                                                            ufo_duration$longitude[i],
                                                            ufo_duration$latitude[i],
                                                            "sunrise")
    ufo_duration$sunset[i] <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                           ufo_duration$occurred_time_zone[i],
                                                           ufo_duration$longitude[i],
                                                           ufo_duration$latitude[i],
                                                           "sunset")
  }
}

rm(nuforc_reports)

# NOTE: since occurred is not in the correct time zone the date comparisons are not correct
# NOTE: sunset and sunrise times appear to be wrong (off by hours)

d <- ufo_duration %>% 
  select(key, state, occurred_time_zone, occurred, sunrise, sunset) %>% 
  mutate(predawn = ifelse(occurred < sunrise, TRUE, FALSE),
         postsunset = ifelse(occurred > sunset, TRUE, FALSE))

