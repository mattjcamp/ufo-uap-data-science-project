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
  read_csv("./01.build.dataset/nuforc_reports_usa.csv")

# Get word tokens from description field
tokens <- nuforc_reports %>%
  select(key, description) %>%
  unnest_tokens(word, description)

# Remove stop words
stop_words <- data.frame(word = stop_words$word, lexicon = "stop_words")
token_words_usa <- tokens %>%
  anti_join(stop_words)

# Save token works

token_words_usa  %>% 
  write_csv("./01.build.dataset/nuforc_lexicon_usa.csv")

# Get the lexicon from the downloaded file
nrc_lexicon <- read_table("./01.build.dataset/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")

# Join lexicon only for words with value = 1 indicating the emotion is present
#   <AssociationFlag> has one of two possible values: 0 or 1. 0 indicates that
#   the target word has no association with affect category, whereas 1 indicates
#   an association.

tokens_sentiment <- token_words_usa %>%
  inner_join(nrc_lexicon %>% filter(value == 1),
    by = "word",
    relationship = "many-to-many"
  )

sentiment_summary <- tokens_sentiment %>%
  group_by(key, sentiment) %>%
  summarize(sentiment_count = n()) %>%
  spread(sentiment, sentiment_count, fill = 0)

token_freq <-
  token_words_usa %>%
  group_by(key) %>%
  count() %>%
  rename(num_words = n) %>%
  ungroup()

token_sentiment_freq_not_pos_neg <-
  tokens_sentiment %>%
  filter(!sentiment %in% c("negative", "positive")) %>%
  group_by(key) %>%
  count() %>%
  rename(num_words_in_lexicon_not_pos_neg = n) %>%
  ungroup()

# Join back to dataset

nuforc_reports <-
  nuforc_reports %>%
  left_join(sentiment_summary) %>%
  #select(1:21, 27, 28, 22:26, 29:31) %>%
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

nuforc_reports %>%
  write_csv("./01.build.dataset/nuforc_reports_usa.csv")

