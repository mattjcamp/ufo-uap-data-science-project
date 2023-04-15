#
# Sentiment Analysis of UFO reports using the NRC Lexicon
# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#
# NOTES
# - Words can be marked positive or negative but that doesn't mean they would
#   also include an emotion word

library(tidyverse)
library(tidytext)
library(textdata)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_v2.csv")

# Get word tokens from description field
tokens <- nuforc_reports %>%
  select(key, description) %>%
  unnest_tokens(word, description)

# Remove stop words
stop_words <- data.frame(word = stop_words$word, lexicon = "stop_words")
tokens <- tokens %>%
  anti_join(stop_words)

token_freq <-
  tokens %>%
  group_by(key) %>%
  count() %>%
  rename(num_words = n) %>%
  ungroup()

# Get the lexicon from the downloaded file
nrc_lexicon <- read_table("./02.text.mining/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")

# Join lexicon only for words with value = 1 indicating the emotion is present
#   <AssociationFlag> has one of two possible values: 0 or 1. 0 indicates that
#   the target word has no association with affect category, whereas 1 indicates
#   an association.

tokens_sentiment <- tokens %>%
  inner_join(nrc_lexicon %>% filter(value == 1),
    by = "word",
    relationship = "many-to-many"
  )

token_sentiment_freq_not_pos_neg <-
  tokens_sentiment %>%
  filter(!sentiment %in% c("negative", "positive")) %>%
  group_by(key) %>%
  count() %>%
  rename(num_words_in_lexicon_not_pos_neg = n) %>%
  ungroup()

sentiment_summary <- tokens_sentiment %>%
  group_by(key, sentiment) %>%
  summarize(sentiment_count = n()) %>%
  spread(sentiment, sentiment_count, fill = 0)

# Join back to dataset

nuforc_text_analysis <-
  nuforc_reports %>%
  left_join(sentiment_summary) %>%
  select(1:21, 27, 28, 22:26, 29:31) %>%
  left_join(token_freq) %>%
  left_join(token_sentiment_freq_not_pos_neg) %>%
  mutate(
    perc_positive = round(positive / (positive + negative) * 100, 1),
    perc_negative = round(negative / (positive + negative) * 100, 1),
    is_positive = ifelse(perc_positive > perc_negative, 1, 0),
    perc_anger = round(anger / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_anticipation = round(anticipation / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_disgust = round(disgust / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_fear = round(fear / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_joy = round(joy / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_sadness = round(sadness / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_surprise = round(surprise / num_words_in_lexicon_not_pos_neg * 100, 1),
    perc_trust = round(trust / num_words_in_lexicon_not_pos_neg * 100, 1)
  ) %>%
  rowwise() %>% # Find Dominate Emotion
  mutate(
    pos = list(which.max(c_across(perc_anger:perc_trust)))[1],
    dominate_emotion = ifelse(!is.na(num_words_in_lexicon_not_pos_neg),
      c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")[pos],
      NA
    )
  ) %>%
  ungroup() %>%
  select(-pos) %>%
  glimpse()

# Save CSV

nuforc_text_analysis %>%
  write_csv("./02.text.mining/nuforc_text_nrc_analysis.csv")

# Data Exploration

# validate text

nuforc_text_analysis %>%
  filter(key == 3) %>%
  select(description) %>%
  clipr::write_clip()
select(1, description, 22:33) %>%
  glimpse()

tokens %>%
  filter(key == 3)

tokens_sentiment %>%
  filter(key == 3) %>%
  arrange(sentiment)

tokens_sentiment %>%
  distinct(key) %>%
  left_join(
    tokens_sentiment %>%
      group_by(key) %>%
      filter(sentiment %in% c("negative", "positive")) %>%
      count() %>%
      rename(pos_or_neg = n)
  ) %>%
  left_join(
    tokens_sentiment %>%
      group_by(key) %>%
      filter(!sentiment %in% c("negative", "positive")) %>%
      count() %>%
      rename(not_pos_or_neg = n)
  ) %>%
  glimpse()

tokens_sentiment %>%
  filter(key == 3) %>%
  arrange(sentiment)

# Frequency

nuforc_text_analysis %>%
  group_by(is_positive) %>%
  count()

#   is_positive      n
#         <dbl>  <int>
# 1           0  17290
# 2           1 102862
# 3          NA   5261

nuforc_text_analysis %>%
  ungroup() %>%
  filter(!is.na(dominate_emotion)) %>%
  group_by(dominate_emotion) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n))

# A tibble: 8 × 2
#   dominate_emotion     n
#   <chr>            <int>
# 1 anticipation     43803
# 2 trust            31301
# 3 fear             15738
# 4 sadness           8653
# 5 joy               7014
# 6 anger             5863
# 7 surprise          3672
# 8 disgust           1515

# Look at one case

nuforc_text_analysis %>%
  filter(key == 915) %>%
  select(description) %>%
  clipr::write_clip()
