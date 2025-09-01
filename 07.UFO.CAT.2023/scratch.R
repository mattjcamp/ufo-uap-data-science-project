library(tidyverse)
library(readxl)

# ufocat <-
#     read_excel("./07.UFO.CAT.2023/data/UFOCAT 2023.xlsx", sheet = "ufocat") %>%
#     glimpse()
# save(ufocat, file = "./07.UFO.CAT.2023/data/UFOCAT 2023.RData")

load(file = "./07.UFO.CAT.2023/data/UFOCAT 2023.RData")

ufocat %>% head(1) %>% glimpse()
