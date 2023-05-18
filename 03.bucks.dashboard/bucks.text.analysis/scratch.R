library(tidyverse)
library(tidytext)
library(textdata)

read_csv("./01.build.dataset/nuforc_reports_bucks.csv") %>%
  tail(1) %>%
  glimpse()
