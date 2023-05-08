
library(tidyverse)
library(tidytext)
library(textdata)

nuforc_reports <-
  read_csv("./01.build.dataset/nuforc_reports_usa.csv")



# Save CSV

nuforc_reports %>%
  write_csv("./01.build.dataset/nuforc_reports_usa.csv")

