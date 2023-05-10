library(tidyverse)

nuforc <-
  read_csv("./01.build.dataset/nuforc_reports_usa.csv") %>%
  glimpse()

# nuforc <- 
  # read_csv("./01.build.dataset/nuforc_reports_past_10_years_bucks.csv") %>%
  # glimpse()

d <- 
  nuforc  %>% 
  filter(
    duration_in_minutes <= 5,
    shape_bin == "lights",
    time_occurred_hour == 11,
    day_of_week == "Sunday",
    is_positive == 1,
    dominate_emotion == "anticipation"
  )

d  %>% 
  tail(1) %>% 
  select(description) %>% 
  as.character()  %>% 
  clipr::write_clip()

# DURATION
# Most encounters are around 3 minutes
# A large portion are a minute or less, very skewed to the lower end

nuforc %>% 
  filter(!is.na(duration_in_minutes)) %>%
  summarise(
    min = min(duration_in_minutes, na.rm = TRUE),
    max = max(duration_in_minutes, na.rm = TRUE),
    mean = mean(duration_in_minutes, na.rm = TRUE),
    median = median(duration_in_minutes, na.rm = TRUE),
    sd = sd(duration_in_minutes, na.rm = TRUE),
    q2 = quantile(duration_in_minutes, .25),
    q3 = quantile(duration_in_minutes, .75))
# min    max     mean      median    sd     q2     q3
# 0      144000  19.8      3         576.   0.5    10

d <- 
  nuforc %>% 
  filter(!is.na(duration_in_minutes)) %>% 
  mutate(minutes = round(duration_in_minutes))  %>% 
  group_by(minutes)  %>% 
  count()  %>% 
  arrange(desc(minutes))

ggplot(
  d  %>% filter(minutes <= 120), 
  # d,
  aes(x = minutes, y = n)) + # remove extreme outliers so I can see
  geom_point() +
  labs(x = "minutes", y = "n") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_bw()

nuforc %>% 
  filter(
    !is.na(shape),
    shape != "unknown",
    shape != "other") %>%
  group_by(shape) %>% 
  count() %>% 
  ungroup()  %>% 
  mutate(
    perc = round(n/sum(n) * 100, 1)
    ) %>% 
  arrange(desc(n)) %>% 
  View()

# SHAPE
# Top 10 shape types (usa sample)
# shape         n  perc
# 1 light     25417  24.7
# 2 circle    13327  13  
# 3 triangle  11345  11  
# 4 fireball   8761   8.5
# 5 sphere     8124   7.9
# 6 disk       7185   7  
# 7 oval       5391   5.2
# 8 formation  4171   4.1
# 9 changing   3256   3.2
# 10 cigar      3113   3 

nuforc %>% 
  group_by(shape_bin) %>% 
  count() %>% 
  ungroup()  %>% 
  mutate(
    perc = round(n/sum(n) * 100, 1)
  ) %>% 
  arrange(desc(n))

# shape_bin       n  perc
# 1 lights    36368  29  
# 2 disks     26946  21.5
# 3 unknowns  17032  13.6
# 4 triangles 16807  13.4
# 5 sphere     8124   6.5

# TIME OF DAY, YEAR, WEEK ETC

# TIME OF DAY
nuforc %>% 
  filter(
    !is.na(time_occurred_hour)) %>%
  group_by(time_occurred_hour) %>% 
  count() %>% 
  ungroup()  %>% 
  mutate(
    perc = round(n/sum(n) * 100, 1)
  ) %>% 
  arrange(desc(time_occurred_hour)) %>% 
  mutate(time = format(as.POSIXct(time_occurred_hour  %>% as.character(), format = "%H"), format = "%I:%M %p")) %>% 
  select(
    time, n, perc
  )
# time         n  perc
# 1 11:00 PM 11098   9  
# 2 10:00 PM 15939  12.9
# 3 09:00 PM 17719  14.4
# 4 08:00 PM 13374  10.9
# 5 07:00 PM  9347   7.6
# 6 06:00 PM  6437   5.2
# 7 05:00 PM  3926   3.2
# 8 04:00 PM  2518   2  
# 9 03:00 PM  2111   1.7
# 10 02:00 PM  1924   1.6


# DAY OF WEEK

nuforc %>% 
  filter(
    !is.na(day_of_week)) %>%
  group_by(day_of_week) %>% 
  count() %>% 
  ungroup()  %>% 
  mutate(
    perc = round(n/sum(n) * 100, 1)
  ) %>% 
  arrange(desc(n))

# day_of_week     n  perc
# 1 Sunday      21983  17.9
# 2 Saturday    17978  14.6
# 3 Monday      17906  14.5
# 4 Friday      17061  13.9
# 5 Thursday    16657  13.5
# 6 Wednesday   16148  13.1
# 7 Tuesday     15377  12.5

# DAY OF YEAR
nuforc %>% 
  filter(
    !is.na(day_of_year)) %>%
  group_by(day_of_year) %>% 
  count() %>% 
  ungroup()  %>% 
  mutate(
    perc = round(n/sum(n) * 100, 1),
    day = as.Date(paste0("2023-", "01-01"), format = "%Y-%m-%d") + day_of_year - 1
  ) %>% 
  mutate(
    day = as.character(day) %>% str_remove("2023-"),
    day = str_replace_all(day, "-", "/")
  ) %>% 
  select(
    day, n, perc
  ) %>% 
  arrange(desc(n))

#   day     n     perc
# 1 07/04  1716   1.4
# 2 06/01   914   0.7
# 3 01/01   895   0.7
# 4 07/05   865   0.7
# 5 06/15   825   0.7

# Frequency

nuforc %>%
  group_by(is_positive) %>%
  count()

#   is_positive      n
#         <dbl>  <int>
# 1           0  17290
# 2           1 102862
# 3          NA   5261

nuforc %>%
  filter(!is.na(dominate_emotion)) %>%
  group_by(dominate_emotion) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n))

#   dominate_emotion     n
# 1 anticipation     43803
# 2 trust            31301
# 3 fear             15738
# 4 sadness           8653
# 5 joy               7014
# 6 anger             5863
# 7 surprise          3672
# 8 disgust           1515

# Alernate Emotion Aggregation

nuforc %>%
  filter(!is.na(dominate_emotion)) %>%
  summarise(
    perc_anger = mean(perc_anger, na.rm = TRUE),
    perc_anticipation = mean(perc_anticipation, na.rm = TRUE),
    perc_disgust = mean(perc_disgust, na.rm = TRUE),
    perc_fear = mean(perc_fear, na.rm = TRUE),
    perc_joy = mean(perc_joy, na.rm = TRUE),
    perc_sadness = mean(perc_sadness, na.rm = TRUE),
    perc_trust = mean(perc_trust, na.rm = TRUE),
    perc_surprise = mean(perc_surprise, na.rm = TRUE)
  )
  
# perc_anger perc_anticipation perc_disgust perc_fear perc_joy perc_sadness perc_trust perc_surprise
# 5.15       21.6              3.44         13.9      11.6     11.0         24.8       8.35

nuforc %>% 
  summarize(
    min = min(afinn_sentiment_score, na.rm = TRUE),
    max = max(afinn_sentiment_score, na.rm = TRUE),
    mean = mean(afinn_sentiment_score, na.rm = TRUE),
    q1 = quantile(afinn_sentiment_score, .25,na.rm = TRUE),
    q3 = quantile(afinn_sentiment_score, .75,na.rm = TRUE)
  )

d <- 
  nuforc %>% 
  count(afinn_sentiment_score) %>% 
  arrange(afinn_sentiment_score)

# LOW INFORMATION SCORE

nuforc %>% 
  group_by(low_information_score) %>% 
  count()

