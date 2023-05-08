library(tidyverse)
library(tidytext)
library(textdata)

read_csv("./01.build.dataset/nuforc_reports_bucks.csv") %>%
  tail(1) %>%
  glimpse()

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_bucks.csv") %>%
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

