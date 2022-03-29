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
  head(5) %>%
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
  
  m_ufo <- matrix(c(longitude, latitude), nrow=1)
  for_date <- as.POSIXct(date_string, tz = timezone_string)
  s <- sunriset(m_ufo, for_date, direction=sunrise_or_sunset, POSIXct.out = TRUE)
  s$time

}
i <- 1
d <- 
  ufo_duration %>% 
  select(key, date_formated, occurred_time_zone) %>% 
  mutate(sunrise = sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                ufo_duration$occurred_time_zone[i],
                                                ufo_duration$longitude[i], 
                                                ufo_duration$latitude[i],
                                                "sunrise"),
         sunset = sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                                ufo_duration$occurred_time_zone[i],
                                                ufo_duration$longitude[i], 
                                                ufo_duration$latitude[i],
                                                "sunset"))

for(i in 1:5){
  d$sunrise[i] <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                               ufo_duration$occurred_time_zone[i],
                                               ufo_duration$longitude[i],
                                               ufo_duration$latitude[i],
                                               "sunrise")
  d$sunset[i] <- sunrise_set_time_for_date_tz(ufo_duration$date_formated[i], 
                                               ufo_duration$occurred_time_zone[i],
                                               ufo_duration$longitude[i],
                                               ufo_duration$latitude[i],
                                               "sunset")
}

rm(nuforc_reports)
# Summary stats duration in seconds
#  NOTE day or more is removed from calculation

# Histogram to show duration

nuforc_reports %>%
  filter(duration_time_in_minutes >= .025,
         duration_time_in_minutes <= 210) %>%
  ggplot(aes(duration_time_in_minutes)) +
  geom_histogram(bins = 30) +
  labs(x="Minutes Observed",
       y="Number of UFO Reports",
       title="Duration of UFO Sightings") +
  theme_minimal()

# 3. DURATION BY SHAPE

duration_for_shape <- function(s){
  if(!is.na(s)){
    d <- 
      nuforc_reports %>% 
      filter(shape == s)
  } else {
    d <- 
      nuforc_reports
  }
  d %>% 
    summarise(
      minutes_min = round(min(duration_time_in_minutes),4),
      minutes_p01 = round(quantile(duration_time_in_minutes, 0.01, na.rm = TRUE),4),
      minutes_p05 = round(quantile(duration_time_in_minutes, 0.05, na.rm = TRUE),4),
      minutes_p10 = round(quantile(duration_time_in_minutes, 0.10, na.rm = TRUE),4),
      minutes_p50 = round(quantile(duration_time_in_minutes, 0.50, na.rm = TRUE),4),
      minutes_p90 = round(quantile(duration_time_in_minutes, 0.90, na.rm = TRUE),4),
      minutes_p95 = round(quantile(duration_time_in_minutes, 0.95, na.rm = TRUE),4),
      minutes_p99 = round(quantile(duration_time_in_minutes, 0.99, na.rm = TRUE),4),
      minutes_max = round(max(duration_time_in_minutes)),
      count = n()) %>% 
    mutate(shape = ifelse(!is.na(s), s, "All Reports")) %>% 
    select(shape,
           count,
           minutes_min,
           minutes_p01,
           minutes_p05,
           minutes_p10,
           minutes_p50,
           minutes_p90,
           minutes_p95,
           minutes_p99,
           minutes_max)
}

durations_by_shape <- duration_for_shape(NA)

shapes <- 
  nuforc_reports %>% 
  distinct(shape) %>% 
  select(shape) %>% 
  filter(!is.na(shape))

for(s in shapes$shape){
  durations_by_shape <- 
    durations_by_shape %>% 
    bind_rows(duration_for_shape(s))
}

rm(shapes)

# 4. DURATION OVER YEARS
# Have UFO sighting times stayed constant over the past 30 years?

duration_over_years <- 
  nuforc_reports %>% 
  mutate(month = month(occurred), year = year(occurred)) %>% 
  select(key, year, duration_time_in_minutes) %>% 
  group_by(year) %>% 
  summarise(p50 = median(duration_time_in_minutes, na.rm = TRUE),
            num_reports = n()) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  filter(!is.na(year))

duration_over_years %>%
  ggplot(aes(x = year, y = p50)) +
  geom_line() +
  labs(x="Year",
       y="Duration in Minutes",
       title="Duration Over Years of UFO Sightings") +
  theme_minimal() +
  ylim(0, 10)

# note that around 1994 reported duration went down noticeable and stayed that way
# see if there is a correlation between number of reports vs duration 

# 5. TIME OF DAY DATASET (MOVE TO TOP BTW)

# DEPRICATED
# time_of_day <- 
#   nuforc_reports %>% 
#   mutate(day_of_week = wday(occurred),
#          hour = hour(occurred),
#          day = yday(occurred),
#          month = month(occurred),
#          year = year(occurred)) %>% 
#   select(key, occurred, day_of_week, hour, day, month, year)

# 6. FREQ OF WEEKDAY SIGHTINGS
# NOTE SHOULD PLOT THIS

time_of_day %>% 
  filter(!is.na(day_of_week)) %>% 
  group_by(day_of_week) %>% 
  count() %>% 
  arrange(day_of_week) %>% 
  ungroup()

# 7. DAY OF YEAR SIGHTINGS

day_of_year_freq <- 
  time_of_day %>% 
  filter(!is.na(day)) %>% 
  group_by(day, year) %>%
  count() %>% 
  ungroup() %>% 
  group_by(day) %>% 
  summarize(total = sum(n),
            min = min(n),
            max = max(n),
            mean = mean(n),
            median = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75)
            ) %>% 
  arrange(day) %>% 
  ungroup()

day_of_year_freq %>%
  # filter(day != 185) %>% # take out fourth of July to see pattern more clearly
  ggplot(aes(x = day, y = mean)) +
  geom_line() +
  labs(x="Day in Year",
       y="Mean Number of Reports",
       title="Summary of Daily Reports") +
  theme_minimal() 

# SMOOTHED PLOT OF DAILY REPORTS

time_of_day %>%
  group_by(day, year) %>%
  count() %>% 
  ungroup() %>% 
  group_by(day) %>% 
  ggplot(aes(x = day, y = n)) +
  geom_smooth() +
  labs(x="Day in Year",
       y="Mean Number of Reports",
       title="Summary of Daily Reports") +
  theme_minimal()

# LINE PLOT OF MONTHLY REPORTS

time_of_day %>%
  group_by(month, year) %>%
  count() %>% 
  ungroup() %>% 
  group_by(month) %>% 
  summarise(mean = mean(n)) %>% 
  filter(!is.na(month)) %>% 
  ggplot(aes(x = month, y = mean)) +
  geom_line() +
  geom_point()+
  labs(x="Month",
       y="Mean Number of Reports",
       title="Summary of Monthly Reports") +
  theme_minimal() +
  ylim(0, 300) + 
  scale_x_continuous(breaks = 1:12)


# 355th day is Winter Solstice
# 
# day_of_year_freq %>% 
#   summarise(mean(n),median(n))

# FREQ OF TIME OF DAY SIGHTINGS

time_of_day %>% 
  filter(!is.na(hour)) %>% 
  group_by(hour) %>% 
  count() %>% 
  arrange(hour)


# STILL TRYING THINGS OUT HERE

# FIGURE OUT IF OUR IS DAYLIGHT OR NIGHT
# https://www.r-bloggers.com/2014/09/seeing-the-daylight-with-r/
# NOTES have to format date with just YYYY-MM-DD
# take care with lat/long


# 
# # these functions need the lat/lon in an unusual format
# portsmouth <- matrix(c(-70.762553, 43.071755), nrow=1)
# for_date <- as.POSIXct("2014-12-25", tz="America/New_York")
# sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)
# sunriset(portsmouth, for_date, direction="sunset", POSIXct.out=TRUE)
# 
# ##         day_frac                time
# ## newlon 0.3007444 2014-12-25 07:13:04

# these functions need the lat/lon in an unusual format
one_obs <- nuforc_reports %>% filter(key == 84) %>% head(1)
one_obs_date <- sprintf("%s-%s-%s", year(one_obs$occurred), month(one_obs$occurred),day(one_obs$occurred))
m_ufo <- matrix(c(one_obs$longitude, one_obs$latitude), nrow=1)
for_date <- as.POSIXct(one_obs_date, tz="America/New_York")
# for_date <- as.POSIXct("2015-1-2", tz="America/New_York") # NOTE WE NEED REAL TIME ZONE, date spelled out
sunriset(m_ufo, for_date, direction="sunrise", POSIXct.out=TRUE)
sunriset(m_ufo, for_date, direction="sunset", POSIXct.out=TRUE)

# install.packages("lutz")


tz_lookup_coords(lat = 36.35665, lon = -119.34794, method = "accurate")
tz_lookup_coords(lat = 36.35665, lon = -119.34794, method = "fast")



# LEARNINGS 1: DURATION
# 
# Most UFO sightings last around 3 minutes. Sightings lasting up to three days are
# reported but are very rare compared to the shorter sightings. Most types of UFOs have
# similar duration characteristics with the exception of "changing" and "flash" UFO
# types.
# 
# Flash type UFOs have a median of only half a second. This makes sense when you think of
# what a flash really is. "Changing" UFOs are interesting with a median sighting time of
# 10 minutes.
# 
# Around 1994 the number of reports went up and the median duration when from around 5 to 3 mins
