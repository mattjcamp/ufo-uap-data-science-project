
# – by year, dots on map, colored by rank ordered timing
# – filter all years, or one year
# – include some other relevant stats in over dashboard as well as case viewer
# – Include filter from X miles from some point, maybe lat/long of city or current case

# https://fontawesome.com/icons
# https://fontawesome.com/search?s=solid&f=classic&o=r

library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(htmltools)
library(htmlwidgets)

nuforc <- 
  read_csv("./03.bucks.dashboards/Bucks_UFO_Browser_v3/nuforc_reports_past_10_years_bucks.csv") %>% 
  select(
    key,
    latitude,
    longitude,
    city,
    shape_bin,
    shape,
    time_occurred,
    duration,
    date_occurred,
    description
  ) %>%
  filter(
    year(date_occurred) == 2022
  ) %>%
  glimpse()


nuforc %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(
    radius = 10,
    color = ~
      case_when(
        shape_bin == "lights" ~ "red",
        shape_bin == "disks" ~ "green",
        shape_bin == "triangles" ~ "black",
        shape_bin == "cigars" ~ "blue",
        shape_bin == "teardrops" ~ "orange",
        TRUE ~ "gray"
      ),
    clusterOptions = markerClusterOptions(),
    label = ~sprintf("%s | %s | %s", city, shape_bin, date_occurred),
    popup = ~sprintf(
      "<h3>%s %s</h3>%s %s <br>%s | %s > %s %s</p><p>%s</p>", 
      city, str_to_title(shape), 
      date_occurred, format(strptime(time_occurred, format = "%HH %MM %SS"), format = "%I:%M %p"), 
      key, shape_bin, shape, duration,
      description)
  )

content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions()
  )

  
# CUSTOM ICON OPTIONS

greenLeafIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)

leafIcons <- icons(
  iconUrl = ifelse(nuforc$shape_bin == "lights",
                   "https://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "https://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38,
  iconHeight = 95,
  iconAnchorX = 22,
  iconAnchorY = 94
)

time_occurred <- '10H 0M 0S'

format(strptime(time_occurred, format = "%HH %MM %SS"), format = "%I:%M %p")

# Format the datetime object as desired
formatted_time <- format(time, format = "%I:%M %p")

# save smaller version of dataset

nuforc_reports <- read_csv(file = "./03.bucks.dashboards/Bucks_UFO_Browser_v3/nuforc_reports_past_10_years_bucks.csv",
                           col_types = cols(.default = "c")) %>% 
  select(
    key,
    date_occurred,
    day_of_week,
    time_occurred,
    shape,
    shape_bin,
    duration_in_minutes,
    duration,
    city,
    latitude,
    longitude,
    description,
    perc_positive,
    perc_anger,
    perc_anticipation,
    perc_disgust,
    perc_fear,
    perc_surprise,
    perc_trust,
    perc_joy,
    perc_sadness,
    dominate_emotion
  ) %>%
  glimpse()


nuforc_reports %>% 
  write_csv(file = "./03.bucks.dashboards/Bucks_UFO_Browser_v3/nuforc_reports.csv")

# YEARS FOR UI

years <- 
  read_csv(file = "./03.bucks.dashboards/Bucks_UFO_Browser_v3/nuforc_reports.csv") %>%
  select(date_occurred) %>% 
  mutate(
    year = year(date_occurred)
  ) %>%
  distinct(year) %>% 
  arrange(year) %>% 
  mutate(year = as.character(year))

years %>% 
  write_csv(file = "./03.bucks.dashboards/Bucks_UFO_Browser_v3/years.csv")

