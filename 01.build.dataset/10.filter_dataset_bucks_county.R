# Builds a version of the UFO dataset that only includes sightings near
# Bucks County PA as defined by 25 miles away from Doylestown

library(tidyverse)
library(lubridate)
library(lutz)
library(maptools)
library(sf)

nuforc_reports_bucks <- 
  read_csv("./01.build.dataset/nuforc_reports_usa.csv")  %>% 
  glimpse()

# Load the geosphere package
library(geosphere)

# Define the location of interest (e.g., San Francisco, CA)
location <- c(-75.1299, 40.3101)

# Calculate the distances between each point in your dataset and the location of interest
distances <- distm(nuforc_reports_bucks[,c("longitude", "latitude")], 
                   location, 
                   fun = distHaversine)/1609.344  # divide by 1609.344 to convert meters to miles

# Identify the points within 100 miles of the location of interest
nuforc_reports_bucks <- subset(nuforc_reports_bucks, distances <= 25)

# View the resulting subset of points
print(nuforc_reports_bucks)

nuforc_reports_bucks %>%
  write_csv("./01.build.dataset/nuforc_reports_bucks.csv")
