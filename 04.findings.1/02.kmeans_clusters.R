
library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")
load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# CLEAN AND CATEGORIZE

ufo_categories <-
  nuforc_reports %>%
  mutate(
    time_in_seconds =
      case_when(
        duration_unit == "seconds" ~ duration_time,
        duration_unit == "minutes" ~ duration_time * 60,
        duration_unit == "hours" ~ duration_time * 60 * 60,
        TRUE ~ -1
      ),
    time_in_seconds = round(ifelse(time_in_seconds == -1, NA, time_in_seconds)),
    time_in_minutes = round(time_in_seconds / 60, 2),
    hour = hour(occurred),
    year = year(occurred),
    text_length = str_length(text)
  ) %>% 
  select(key, shape, time_in_minutes, hour, text_length) %>% 
  filter(!is.na(shape),
         !is.na(time_in_minutes),
         !is.na(hour),
         !is.na(text_length)) %>%
  mutate(shape = ifelse(shape == "unknown", "other", shape),
         shape = ifelse(shape == "changed", "changing", shape)) %>% 
  mutate(shape_value = 1) %>% 
  pivot_wider(names_from = shape, values_from = shape_value, values_fill = list(shape_value = 0))

# TRY SOME KMEANS

# d <- ufo_categories %>% na.omit()

df <- 
  ufo_categories %>% 
  # select(-key, -crescent, -dome) %>% # had to remove NaN and NA
  # select(-key) %>%
  # select(time_in_minutes, hour) %>%
  select(time_in_minutes, text_length) %>%
  scale()

# PLOT THAT STAT TO FIND OPTIMAL AMOUNT OF CLUSTERS

set.seed(123)
ratio_ss <- rep(0, 10)
for (k in 1:10) {
  km <- kmeans(df, k)
  ratio_ss[k] <- km$tot.withinss / km$totss
}
plot(ratio_ss, type = "b", xlab = "k")

km <- kmeans(df, centers = 4)
km$size
obs_with_clusters <- 
  # cbind(ufo_categories, cluster = km.res$cluster) %>%
  cbind(ufo_categories, cluster = km$cluster) %>% 
  select(key, cluster, text_length, time_in_minutes) %>% 
  inner_join(nuforc_reports, id = "key")

d <- 
  obs_with_clusters %>% 
  group_by(cluster) %>% 
  summarise(mean(text_length),
            mean(time_in_minutes),
            num = n())

obs_with_clusters %>% 
  # head(100) %>% 
  ggplot(aes(x = text_length, 
             y = time_in_minutes, 
             color = factor(cluster))) +
  geom_point(position = "jitter") +
  labs(x="text_length",
       y="time_in_minutes",
       title="Length by Time of Day") +
  theme_minimal() +
  scale_colour_manual(values = c("red", "blue", "green", "black"))

# NOTE you can try to convert each shape to a boolean field: ie 1/0 for light
# this will allow us to include shapes but it will not create arbtirary "distances"
# if we just coded each shape with a number
# 
# REF: http://rstudio-pubs-static.s3.amazonaws.com/186401_b8f0a5a173f74e69b9ee2c1becefcf4b.html
# What is your optimal k?
#   In the last exercise you made a scree plot, showing the ratio of the within cluster sum of squares to the total sum of squares.
# You want to choose k such that your clusters are compact and well separated. However, the ratio_ss keeps decreasing as k increases. Hence, if you were to minimize this ratio as function of k, you’d end up with a cluster for every school, which is a meaningless result. You should choose k such that when increasing it, the impact on ratio_ss is not significant. The elbow in the scree plot will help you identify this turning point.
# Can you tell which of the following values of k will provide a meaningful clustering with overall compact and well separated clusters? The ratio, ratio_ss, is still in your workspace.
# 3 or 4
# The plot shows a considerable drop for k equal to 3. Furthermore, the ratio_ss for k equal to 2 is higher than 20%.

