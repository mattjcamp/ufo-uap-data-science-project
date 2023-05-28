
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

nuforc <- 
  read_csv("./03.bucks.dashboards/Bucks_UFO_Browser_v3/nuforc_reports_past_10_years_bucks.csv") %>% 
  select(
    key,
    latitude,
    longitude,
    city,
    shape_bin,
    date_occurred
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
        shape_bin == "disks" ~ "blue",
        shape_bin == "triangles" ~ "darkgray",
        shape_bin == "cigars" ~ "yellow",
        shape_bin == "teardrops" ~ "orange",
        TRUE ~ "black"
      ),
    clusterOptions = markerClusterOptions(),
    label = ~sprintf("Case %s %s %s %s", key, city, shape_bin, date_occurred)
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


