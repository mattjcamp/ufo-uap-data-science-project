
library(tidyverse)
library(tidytext)
library(textdata)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_usa.csv")

tokens <- nuforc_reports %>%
  unnest_tokens(word, description)

# Remove stop words
stop_words <- data.frame(word = stop_words$word, lexicon = "stop_words")
tokens <- tokens %>%
  anti_join(stop_words)

afinn <- tidytext::get_sentiments("afinn")

tokens_sentiment <- tokens %>%
  inner_join(afinn, by = "word")

sentiment_summary <- tokens_sentiment %>%
  group_by(key) %>%
  summarize(afinn_sentiment_score = sum(value))

nuforc_reports <- 
  nuforc_reports %>% 
  left_join(sentiment_summary)

# Save CSV

nuforc_reports %>%
  write_csv("./01.build.dataset/nuforc_reports_usa.csv")

