library(tidyverse)
library(tidytext)
library(textdata)
library(topicmodels)
library(dplyr)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_v2.csv")

nuforc_reports <- # filter to make the processing quicker
  nuforc_reports %>%
  filter(state == "PA")

# Read in data
data <- nuforc_reports %>%
  select(key, description)

# Clean and tokenize the text
clean_data <- data %>%
  mutate(
    description = str_to_lower(description),
    description = str_remove_all(description, "[^[:alpha:][:space:]]+"),
    description = str_replace_all(description, "\\s+", " ")
  ) %>%
  unnest_tokens(word, description)

# Remove stop words
stop_words <- data.frame(word = stop_words$word, lexicon = "stop_words")
clean_data <- clean_data %>%
  anti_join(stop_words)

# Create document-term matrix
dtm <- clean_data %>%
  count(key, word) %>%
  cast_dtm(key, word, n)

# Fit LDA model
lda_model <- LDA(dtm, k = 5, method = "Gibbs", control = list(seed = 1234))

# Print top terms for each topic
terms_per_topic <- 10
lda_top_terms <- terms(lda_model, terms_per_topic)
for (i in seq_along(lda_top_terms)) {
  cat(paste("Topic", i, ":", sep = " "))
  cat(paste(lda_top_terms[[i]], collapse = ", "), "\n\n")
}

# https://www.tidytextmining.com/topicmodeling.html
