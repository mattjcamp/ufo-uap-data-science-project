
# NOTE key = 75379 contains very interesting case from Newtown Square that may related to 
# 
# the Kecksburg incident. Very 
# key = 35585 is a possible hudson valley style ufo near dover air force base

library(tidyverse)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")

load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# Average Time of Encounter

nuforc_reports <- 
  nuforc_reports %>% 
  mutate(duration_time_in_seconds = 
          case_when(
            duration_unit == "seconds" ~ duration_time,
            duration_unit == "minutes" ~ duration_time * 60,
            duration_unit == "hours" ~ duration_time * 60 * 60,
            TRUE ~ -1
           ),
         duration_time_in_seconds = ifelse(duration_time_in_seconds == -1, NA, duration_time_in_seconds),
         duration_time_in_minutes = duration_time_in_seconds / 60,
         duration_time_in_hours = duration_time_in_minutes / 60)

# Remove time values that I found are clearly wrong

nuforc_reports <- 
  nuforc_reports %>% 
  filter(!key %in% c('31427','75379','44692'),
         duration_time_in_seconds != -1)

# Summary stats duration in seconds
#  NOTE day or more is removed from calculation

summarize_duration <- function(s){
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

# Look at shapes vs duration

d <- 
  nuforc_reports %>% 
  count(shape) %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(shape))

durations_by_shape <- summarize_duration(NA)

for(s in d$shape){
  durations_by_shape <- 
    durations_by_shape %>% 
    bind_rows(summarize_duration(s))
}

# Duration Over Time
# Have UFO sighting times stayed constant over the past 30 years?

library(lubridate)

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
  
# Histogram to show duration over time

duration_over_years %>%
  ggplot(aes(x = year, y = p50)) +
  geom_line() +
  labs(x="Year",
       y="Duration in Minutes",
       title="Duration Over Years of UFO Sightings") +
  theme_minimal() +
  ylim(0, 10)

# TIME OF DAY AND DAY

time_of_day <- 
  nuforc_reports %>% 
  mutate(day_of_week = wday(occurred),
         hour = hour(occurred),
         day = yday(occurred),
         month = month(occurred),
         year = year(occurred)) %>% 
  select(key, occurred, day_of_week, hour, day, month, year)

# FREQ OF WEEKDAY SIGHTINGS

time_of_day %>% 
  filter(!is.na(day_of_week)) %>% 
  group_by(day_of_week) %>% 
  count() %>% 
  arrange(day_of_week) %>% 
  ungroup()

# FREQ OF DAY OF YEAR SIGHTINGS

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
  filter(day != 185) %>% # take out fourth of July to see pattern more clearly
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
       y="Number of Reports",
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

day_of_year_freq %>% 
  filter(day == 145)

day_of_year_freq %>% 
  summarise(mean(n),median(n))

# FREQ OF TIME OF DAY SIGHTINGS

time_of_day %>% 
  filter(!is.na(hour)) %>% 
  group_by(hour) %>% 
  count() %>% 
  arrange(hour)

# FIGURE OUT IF OUR IS DAYLIGHT OR NIGHT
# https://www.r-bloggers.com/2014/09/seeing-the-daylight-with-r/
# NOTES have to format date with just YYYY-MM-DD
# take care with lat/long
library(maptools)

# these functions need the lat/lon in an unusual format
d <- nuforc_reports %>% filter(key == 84) %>% head(1)
m_ufo <- matrix(c(d$longitude, d$latitude), nrow=1)
# for_date <- as.POSIXct(d$occurred, tz="America/New_York")
for_date <- as.POSIXct("2015-01-02", tz="UTC")
sunriset(m_ufo, for_date, direction="sunrise", POSIXct.out=TRUE)
sunriset(m_ufo, for_date, direction="sunset", POSIXct.out=TRUE)

install.packages("lutz")
library(lutz)

if (require("sf")) {
  
  state_pts <- lapply(seq_along(state.center$x), function(i) {
    st_point(c(state.center$x[i], state.center$y[i]))
  })
  
  state_centers_sf <- st_sf(st_sfc(state_pts))
  
  state_centers_sf$tz <- tz_lookup(state_centers_sf)
  
  plot(state_centers_sf[, "tz"])
}

library(maptools)

# these functions need the lat/lon in an unusual format
portsmouth <- matrix(c(-70.762553, 43.071755), nrow=1)
for_date <- as.POSIXct("2014-12-25", tz="America/New_York")
sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)
sunriset(portsmouth, for_date, direction="sunset", POSIXct.out=TRUE)

##         day_frac                time
## newlon 0.3007444 2014-12-25 07:13:04

# LEARNINGS 1: DURATION
# 
# Most UFO sightings last around 3 minutes. Sightings lasting up to three days are
# reported but are very rare compared to the shorter sightings. Most types of UFOs have
# similar duration charactoristics with the exception of "changing" and "flash" UFO
# types.
# 
# Flash type UFOs have a median of only half a second. This makes sense when you think of
# what a flash really is. "Changing" UFOs are interesting with a median sighting time of
# 10 minutes.
# 
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


library(corrr)

nuforc_cor <- 
  nuforc_reports %>% 
  select(latitude,longitude) %>% 
  correlate(use = "na.or.complete")

tbl = matrix(data=c(55, 45, 20, 30), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

