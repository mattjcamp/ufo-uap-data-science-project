
# NOTE you can try to convert each shape to a boolean field: ie 1/0 for light
# this will allow us to include shapes but it will not create arbtirary "distances"
# if we just coded each shape with a number

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
  select(key, shape, latitude, longitude, time_in_minutes, hour, text_length) %>% 
  filter(!is.na(shape),
         !is.na(time_in_minutes),
         !is.na(hour)) %>%
  mutate(shape = ifelse(shape == "unknown", "other", shape),
         shape = ifelse(shape == "changed", "changing", shape)) %>% 
  mutate(shape_value = 1) %>% 
  pivot_wider(names_from = shape, values_from = shape_value, values_fill = list(shape_value = 0))

# TRY SOME KMEANS

d <- ufo_categories %>% na.omit()

df <- 
  d %>% 
  select(-key, -latitude,-longitude, -crescent, -dome) %>% # had to remove NaN and NA
  scale()

# df <- na.omit(df)
# new_DF <- df[rowSums(is.na(df)) > 0,]

set.seed(123)
km.res <- kmeans(df, centers = 10)
print(km.res)
obs_with_clusters <- cbind(d, cluster = km.res$cluster)

# PLOT THAT STAT TO FIND OPTIMAL AMOUNT OF CLUSTERS

# REF: http://rstudio-pubs-static.s3.amazonaws.com/186401_b8f0a5a173f74e69b9ee2c1becefcf4b.html
# What is your optimal k?
#   In the last exercise you made a scree plot, showing the ratio of the within cluster sum of squares to the total sum of squares.
# You want to choose k such that your clusters are compact and well separated. However, the ratio_ss keeps decreasing as k increases. Hence, if you were to minimize this ratio as function of k, you’d end up with a cluster for every school, which is a meaningless result. You should choose k such that when increasing it, the impact on ratio_ss is not significant. The elbow in the scree plot will help you identify this turning point.
# Can you tell which of the following values of k will provide a meaningful clustering with overall compact and well separated clusters? The ratio, ratio_ss, is still in your workspace.
# 3 or 4
# The plot shows a considerable drop for k equal to 3. Furthermore, the ratio_ss for k equal to 2 is higher than 20%.

ratio_ss <- rep(0, 20)
for (k in 1:20) {
  km <- kmeans(df, k)
  ratio_ss[k] <- km$tot.withinss / km$totss
}
plot(ratio_ss, type = "b", xlab = "k")

# I would go with 12

km <- kmeans(df, centers = 12)
km$size
obs_with_clusters <- 
  cbind(d, cluster = km.res$cluster) %>% 
  select(key, cluster) %>% 
  inner_join(nuforc_reports, id = "key")

d <- 
  obs_with_clusters %>% 
  # group_by(cluster, shape) %>% 
  distinct(cluster, shape) %>% 
  group_by(cluster) %>% 
  count()

# EXAMPLE

library(cluster.datasets)
data(new.haven.school.scores)
# The dataset school_result is pre-loaded
# school_result <-  read.csv("school_result.csv")
school_result <- new.haven.school.scores
# Set random seed. Don't remove this line.
set.seed(100)
# Explore the structure of your data
str(school_result)
# Initialise ratio_ss. Initialize a vector of length 7, ratio_ss, that contains all zeros.
ratio_ss <- rep(0, 7)
school_result <- school_result %>% select(-school)
# Finish the for-loop
for (k in 1:7) {
  # Apply k-means to school_result: school_km
  school_km <- kmeans(school_result, k, nstart = 20)
  # Save the ratio between of WSS to TSS in kth element of ratio_ss. Save the corresponding ratio tot.withinss to totss in the vector ratio_ss at index k. These values are found in the school_km object.
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
}
# Make a scree plot with type "b" (connecting the points) and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")







