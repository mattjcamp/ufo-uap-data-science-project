
library(tidyverse)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_usa.csv")  %>% 
  glimpse()

# Find Description Length Metrics

nuforc_reports <- 
  nuforc_reports %>% 
  mutate( # make metrics, 0 is low, 1 is high
    description_liz = ifelse(description_length <= 150, 0, 1),
    time_zone_liz = ifelse(is.na(time_zone), 0, 1),
    duraction_liz = ifelse(is.na(duration_time), 0, 1),
    location_liz = ifelse(is.na(latitude), 0, 1),
    shape_liz = ifelse(shape_bin == "unknowns" | is.na(shape_bin), 0, 1),
    date_posted_liz = ifelse(is.na(date_posted), 0, 1),
    dominate_emotion_liz = ifelse(is.na(dominate_emotion), 0, 1),
    afinn_sentiment_score_liz = ifelse(is.na(afinn_sentiment_score), 0, 1),
    low_information_score = 
      description_liz + time_zone_liz + duraction_liz + location_liz +
      shape_liz + date_posted_liz + dominate_emotion_liz + afinn_sentiment_score_liz
  )  %>% 
  select(
    key,
    low_information_score
  ) %>% 
  inner_join(
    nuforc_reports
  )  %>% 
  glimpse()

# Save CSV

nuforc_reports %>%
  write_csv("./01.build.dataset/nuforc_reports_usa.csv")

