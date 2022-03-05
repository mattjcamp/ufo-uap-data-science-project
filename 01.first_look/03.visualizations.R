library(tidyverse)

load("nuforc_reports.rdata")

# CLEAN DATA

library(lubridate)

nuforc_reports <- 
  nuforc_reports %>% 
  mutate(date_time = ymd_hms(date_time),
         posted = ymd(posted))

# FREQUENCY BY DATE

library(ggplot2)

nuforc_reports %>%
  ggplot(aes(as.Date(date_time))) + 
  geom_area(stat = "bin", bins=50) + 
  scale_x_date(date_labels = "%Y", 
               date_breaks = "10 year") +
  labs(x="Date Reported", 
       y="Number of UFO Reports", 
       title="NUFORC Reports By Date") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 20))   

ggsave("nuforc_by_date.png", width = 5, height = 5) 

# SUMMARIZE DATE TIME 

nuforc_reports %>% 
  summarise(min = min(date_time, na.rm = TRUE),
            median = median(date_time, na.rm = TRUE),
            max = max(date_time, na.rm = TRUE)) %>% 
  rownames_to_column() %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

# WHAT SHAPES ARE PEOPLE SEEING?

d <- 
  nuforc_reports %>% 
  group_by(shape) %>% 
  count(shape) %>% 
  filter(!is.na(shape),
         shape != "unknown",
         shape != "changed",
         n > 20) %>% 
  arrange(desc(n))


ggplot(d, aes(x = shape, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y="Number of Times Reported", 
       x="UFO Shape", 
       title="UFO Shapes Observed") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 20)) 

ggsave("ufo_shapes.png", width = 5, height = 5) 

d <- 
nuforc_reports %>% 
  mutate(text_length = str_length(text)) %>% 
  select(text_length, text) %>% 
  arrange(desc(text_length)) %>% 
  head(1)


