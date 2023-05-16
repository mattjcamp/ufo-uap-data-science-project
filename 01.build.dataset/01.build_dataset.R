library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)
library(sf)

# Downloaded NUFORC reports from:
#   https://data.world/timothyrenner/ufo-sightings
#   (ref: Tim Renner)

nuforc_reports_download <-
  read_csv("./01.build.dataset/nuforc_reports_download.csv") %>%
  glimpse()

nuforc_reports <-
  nuforc_reports_download %>%
  filter(state %in% state.abb) %>% # removes all but valid US states
  mutate(
    date_occurred = str_remove(date_time, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"),
    date_occurred = ymd(date_occurred),
    time_occurred = str_extract(date_time, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"),
    time_occurred = hms(time_occurred),
    time_occurred_hour = hour(date_time) %>% as.integer(),
    time_occurred_minute = minute(date_time) %>% as.integer(),
    time_occurred_second = second(date_time) %>% as.integer(),
    date_posted = ymd(posted),
    key = row_number()
  ) %>%
  select(-posted, -date_time) %>%
  glimpse()

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
    !is.na(duration), !duration %in% c(
      "?",
      "unknown",
      "-------------------------",
      "----",
      "--",
      "-",
      "you butt holes"
    )
  ) %>%
  mutate(
    words = str_to_lower(words),
    words = str_extract_all(words, "[a-z]+"),
    word_category = map_chr(.x = words, .f = function(x) {
      ({
        s <- NA

        for (val in x) {
          if (val %in% c(
            "minutes", "mins", "min", "minute",
            "mintues", "minuts", "minuets", "minutess",
            "minites", "minuites", "minets", "mimutes", "minues",
            "miniutes", "miutes", "minits", "mintes", "minuted",
            "minutea", "minutos", "mnutes", "ninutes", "mintutes",
            "mim", "mm", "mns", "mint", "minuet", "miinutes",
            "minnutes", "minures", "minutew", "miuntes", "mniutes",
            "m", "munites", "mi", "mims", "minitues", "minnute", "minuates",
            "minuetes", "minuits", "minuntes", "minurwa", "minustes", "minuters",
            "minutis", "minuutes", "monutes", "ms", "tominutes", "minuit", "minutue",
            "minu", "miniuts", "minns", "minea", "tomins", "mints", "minutestriangle",
            "minuite", "imin", "iminute", "menutes", "miin", "miites", "mina", "minents",
            "minet", "minetes", "miniute", "minonds", "mnute", "muntes", "munutes", "muinte",
            "miuets", "miunets", "miunute", "miute", "miuts", "mlnutes", "minurtes", "minut",
            "minuteds", "minutee", "minutees", "minutese", "minuteswhile", "minutets",
            "minutez", "minuties", "minutres", "minuttes", "minutues", "mintue", "mintute",
            "minsorlonger", "inutes"
          )) {
            s <- "minutes"
          }
        }

        for (val in x) {
          if (val %in% c(
            "seconds", "sec", "secs", "second", "secounds",
            "secconds", "milliseconds", "moment", "secods",
            "secomds", "secons", "seckonds", "momentary",
            "fast", "brief", "short", "secondss",
            "minsec", "sconds", "secinds", "secnds",
            "secondes", "seonds", "moments", "econds",
            "millisecond", "aboutseconds", "secodns", "seconda",
            "secondds", "seconnds", "secpnds", "secunds", "toseconds",
            "quick", "quickly", "approxsec", "aproxsecs", "seco",
            "tosecs", "tosec", "ceconds", "deconds", "desonds",
            "secon", "milisec", "milisecod", "sseconds", "ssecs",
            "sesconds", "seounds", "segs", "segundos", "senconds", "seocds", "seoonds",
            "secands", "secants", "seceonds", "secionds", "seconcs", "secondsss",
            "seconfs", "secopnd", "secounts", "secsonds", "secthen", "secx",
            "sceonds", "scounds", "secaond", "mseconds", "seeconds", "blink",
            "secends", "seconts", "instantaneous", "instant", "flash"
          )) {
            s <- "seconds"
          }
        }

        for (val in x) {
          if (val %in% c(
            "hours", "hour", "hrs", "hr", "hous", "tohour",
            "hiours", "hm", "horas", "houres", "hourish", "hoursmin",
            "onehour", "houl", "housr", "nours", "houra", "hrmin"
          )) {
            s <- "hours"
          }
        }

        for (val in x) {
          if (val %in% c(
            "days", "day", "year", "years", "month", "months", "lifes", "life", "week",
            "summer", "nite", "winter", "daily", "allways", "ongoing", "weeks",
            "daytime", "months", "weeks", "every", "months", "nights", "yrs",
            "wks", "night"
          )) {
            s <- "day_or_more"
          }
        }
        s
      })
    }),
    numbers_length = map_int(.x = numbers, .f = function(x) {
      ({
        length(x)
      })
    })
  ) %>%
  filter(numbers_length <= 2) %>% # REMOVE LONG SERIES OF NUMBERS SINCE THEY WERE TOO MESSED UP
  mutate(numbers = map_dbl(.x = numbers, .f = function(x) {
    ({
      n <- NA
      l <- length(x)
      if (l == 2) {
        n <- (as.numeric(x[1]) + as.numeric(x[2])) / 2
      }
      if (l == 1) {
        n <- as.numeric(x[1])
      }
      n
    })
  })) %>%
  select(key, duration_time = numbers, duration_unit = word_category)

nuforc_reports <-
  nuforc_reports %>%
  left_join(duration_dataset, by = "key")

# ADD MORE DETAILED TIME DATA
nuforc_reports <-
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
    day_of_year = yday(date_occurred),
    day_of_month = day(date_occurred),
    day_of_week = wday(date_occurred),
    month = month(date_occurred),
    year = year(date_occurred),
    hour = hour(date_occurred),
    minute = minute(date_occurred),
    time_zone = tz_lookup_coords(
      lat = city_latitude,
      lon = city_longitude,
      method = "accurate"
    )
  ) %>%
  mutate(day_of_week = case_when(
    day_of_week == 1 ~ "Sunday",
    day_of_week == 2 ~ "Monday",
    day_of_week == 3 ~ "Tuesday",
    day_of_week == 4 ~ "Wednesday",
    day_of_week == 5 ~ "Thursday",
    day_of_week == 6 ~ "Friday",
    day_of_week == 7 ~ "Saturday",
    TRUE ~ NA_character_
  )) %>%
  mutate( # Shape Super Bin
    shape_bin = case_when(
      shape %in% c("light", "star", "fireball", "flash") ~ "lights",
      shape %in% c("disk", "circle", "egg", "oval") ~ "disks",
      shape %in% c("triangle", "delta", "chevron", "diamond", "rectangle") ~ "triangles",
      shape %in% c("cigar", "cylinder") ~ "cigars",
      shape %in% c("teardrop", "cone") ~ "teardrops",
      shape %in% c("unknown", "other") ~ "unknowns",
      TRUE ~ shape
    ),
    description_length = str_length(text)
  ) %>% 
  select(
    key,
    # datetime information
    date_occurred,
    day_of_week,
    day_of_month,
    day_of_year,
    time_occurred,
    time_occurred_hour,
    time_occurred_minute,
    time_zone,
    duration,
    duration_time,
    duration_unit,
    duration_in_hours = time_in_hours,
    duration_in_minutes = time_in_minutes,
    duration_in_seconds = time_in_seconds,
    date_posted,
    # location information
    country,
    city,
    state,
    latitude = city_latitude,
    longitude = city_longitude,
    # Report details
    shape,
    shape_bin,
    -summary,
    description = text,
    description_length,
    report_link,
    -stats
    #-city_location
  ) %>%
  glimpse()

nuforc_reports %>%
  write_csv("./01.build.dataset/nuforc_reports_usa.csv")
