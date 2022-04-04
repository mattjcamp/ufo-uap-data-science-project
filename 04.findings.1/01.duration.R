
library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")
load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# 1.BUILD DURATION DATASET

ufo_duration <-
  nuforc_reports %>%
  mutate(
    time_in_seconds =
      case_when(
        duration_unit == "seconds" ~ duration_time,
        duration_unit == "minutes" ~ duration_time * 60,
        duration_unit == "hours" ~ duration_time * 60 * 60,
        TRUE ~ -1
      ),
    time_in_seconds = round(ifelse(time_in_seconds == -1, NA, time_in_seconds)),
    time_in_minutes = round(time_in_seconds / 60, 2),
    time_in_hours = round(time_in_minutes / 60, 2),
    day_of_year = yday(occurred),
    day_of_month = day(occurred),
    day_of_week = wday(occurred),
    month = month(occurred),
    year = year(occurred),
    hour = hour(occurred),
    minute = minute(occurred),
    time_zone = tz_lookup_coords(lat = latitude, lon = longitude, method = "accurate")
  ) %>% 
  select(-time_in_seconds,-time_in_hours)

# 2. HOW LONG (DURATION) DO MOST UFO SIGHTINGS LAST?

ufo_duration %>%
  filter(time_in_minutes >= .025,
         time_in_minutes <= 210) %>%
  ggplot(aes(time_in_minutes)) +
  geom_histogram(bins = 30) +
  labs(x="Minutes Observed",
       y="Number of UFO Reports",
       title="Duration of UFO Sightings") +
  theme_minimal()

# 3. DURATION BY SHAPE

duration_for_shape <- function(s){
  if(!is.na(s)){
    d <- 
      ufo_duration %>% 
      filter(shape == s)
  } else {
    d <- 
      ufo_duration
  }
  d %>% 
    summarise(
      minutes_min = round(min(time_in_minutes),4),
      minutes_p01 = round(quantile(time_in_minutes, 0.01, na.rm = TRUE),4),
      minutes_p05 = round(quantile(time_in_minutes, 0.05, na.rm = TRUE),4),
      minutes_p10 = round(quantile(time_in_minutes, 0.10, na.rm = TRUE),4),
      minutes_p50 = round(quantile(time_in_minutes, 0.50, na.rm = TRUE),4),
      minutes_p90 = round(quantile(time_in_minutes, 0.90, na.rm = TRUE),4),
      minutes_p95 = round(quantile(time_in_minutes, 0.95, na.rm = TRUE),4),
      minutes_p99 = round(quantile(time_in_minutes, 0.99, na.rm = TRUE),4),
      minutes_max = round(max(time_in_minutes)),
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

durations_by_shape %>% 
  filter(count > 10) %>% # REMOVE OUTLIERS
  ggplot(aes(x = shape, y = minutes_p50)) +
  geom_col() +
  labs(x="UFO Shape",
       y="Medium Minutes",
       title="Sighting Duration by Shape") +
  theme_minimal() +
  coord_flip()

# 4. DURATION OVER YEARS
# Have UFO sighting times stayed constant over the past 30 years?

duration_over_years <- 
  ufo_duration %>% 
  mutate(month = month, year = year) %>% 
  select(key, year, time_in_minutes) %>% 
  group_by(year) %>% 
  summarise(p50 = median(time_in_minutes, na.rm = TRUE),
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

rm(duration_over_years)

# note that around 1994 reported duration went down noticeable and stayed that way
# see if there is a correlation between number of reports vs duration 

# 5. TIME OF DAY DATASET

# 24-hour clock	12-hour clock
# 00:00	12.00a.m. - midnight
# 01:00	1:00 a.m.
# 02:00	2:00 a.m.
# 03:00	3:00 a.m.
# 04:00	4:00 a.m.
# 05:00	5:00 a.m.
# 06:00	6:00 a.m.
# 07:00	7:00 a.m.
# 08:00	8:00 a.m.
# 09:00	9:00 a.m.
# 10:00	10:00 a.m.
# 11:00	11:00 a.m.
# 12:00	12:00 p.[1]m. - noon
# 13:00	1:00 p.m.
# 14:00	2:00 p.m.
# 15:00	3:00 p.m.
# 16:00	4:00 p.m.
# 17:00	5:00 p.m.
# 18:00	6:00 p.m.
# 19:00	7:00 p.m.
# 20:00	8:00 p.m.
# 21:00	9:00 p.m.
# 22:00	10:00 p.m.
# 23:00	11:00 p.m.
# 00:00	12:00 a.m. - midnigh

time_of_day <- 
  ufo_duration %>% 
  filter(!is.na(hour)) %>% 
  group_by(hour, year) %>% 
  count() %>% 
  arrange(hour) %>% 
  ungroup() %>% 
  group_by(hour) %>% 
  summarize(mean = mean(n, na.rm = TRUE),
            sum = sum(n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(hour_12 = ifelse(hour > 12, hour - 12, hour),
         hour_12 = ifelse(hour == 0, 12, hour_12),
         am_pm = ifelse(hour > 12, "PM", "AM"),
         hour_label = paste0(hour_12," ", am_pm))

time_of_day %>% 
  ggplot(aes(x = hour, y = mean)) +
  geom_col() +
  labs(x="Hour of Day",
       y="Average Sightings",
       title="Sighting by Time of Day") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23, labels = time_of_day$hour_label) +
  coord_flip()

# 6. FREQ OF WEEKDAY SIGHTINGS

sightings_by_day_of_week <- 
  ufo_duration %>% 
  filter(!is.na(day_of_week)) %>% 
  group_by(day_of_week) %>% 
  count() %>% 
  arrange(day_of_week) %>% 
  ungroup()

ufo_duration %>% 
  filter(!is.na(day_of_week)) %>% 
  group_by(day_of_week,day_of_month, month) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(day_of_week) %>% 
  summarize(mean = mean(n)) %>% 
  arrange(day_of_week) %>% 
  ungroup() %>% 
  ggplot(aes(x = day_of_week, y = mean)) +
  geom_col() +
  labs(x="Day of Week",
       y="Average Sightings",
       title="Average Number of UFO Sightings") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7)

rm(sightings_by_day_of_week)

# 7. DAY OF YEAR SIGHTINGS

day_of_year_freq <- 
  ufo_duration %>% 
  filter(!is.na(day_of_year)) %>% 
  group_by(day_of_year, year) %>%
  count() %>% 
  ungroup() %>% 
  group_by(day_of_year) %>% 
  summarize(total = sum(n),
            min = min(n),
            max = max(n),
            mean = mean(n),
            median = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75)
            ) %>% 
  arrange(day_of_year) %>% 
  ungroup()

day_of_year_freq %>%
  ggplot(aes(x = day_of_year, y = mean)) +
  geom_line() +
  labs(x="Day in Year",
       y="Mean Number of Reports",
       title="Summary of Daily Reports") +
  theme_minimal() 

# SMOOTHED PLOT OF DAILY REPORTS

ufo_duration %>% 
  filter(!is.na(day_of_year)) %>% 
  group_by(day_of_year, year) %>%
  count() %>% 
  ungroup() %>% 
  group_by(day_of_year) %>% 
  ggplot(aes(x = day_of_year, y = n)) +
  geom_smooth() +
  labs(x="Day in Year",
       y="Mean Number of Reports",
       title="Summary of Daily Reports") +
  theme_minimal()

# LINE PLOT OF MONTHLY REPORTS

ufo_duration %>%
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
# 1994 is also when NUFORC started to use their website as the means to report sightings:
# 
# The principal means used by the Center to receive sighting reports is this website, which 
# has operated continuously since 1994.   Prior to that period, the telephone hotline and 
# the U.S. mail were the primary means of taking reports.   
# 
# https://nuforc.org/about-us/