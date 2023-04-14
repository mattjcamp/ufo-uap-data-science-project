library(tidyverse)
library(tidytext)
library(textdata)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_v2.csv") %>%
  select(key, description)

tokens <- nuforc_reports %>%
  unnest_tokens(word, description)

# Remove stop words
stop_words <- data.frame(word = stop_words$word, lexicon = "stop_words")
tokens <- tokens %>%
  anti_join(stop_words)

# Preview the resulting dataset
head(tokens)

tokens %>%
  count(word) %>%
  arrange(n) %>%
  tail(25) %>%
  View()

afinn <- tidytext::get_sentiments("afinn")

tokens_sentiment <- tokens %>%
  inner_join(afinn, by = "word")

sentiment_summary <- tokens_sentiment %>%
  group_by(key) %>%
  summarize(sentiment_score = sum(value))


mean(sentiment_summary$sentiment_score, na.rm = TRUE)

# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

nrc_lexicon <- read_table("./02.text.mining/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")

# Preview the first few rows of the lexicon
head(nrc_lexicon)


tokens_sentiment <- tokens %>%
  inner_join(nrc_lexicon %>% filter(value == 1), by = "word", relationship = "many-to-many")

sentiment_summary <- tokens_sentiment %>%
  group_by(key, sentiment) %>%
  summarize(sentiment_count = n()) %>%
  spread(sentiment, sentiment_count, fill = 0)
