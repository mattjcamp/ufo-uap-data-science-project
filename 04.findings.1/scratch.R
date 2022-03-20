
# NOTE key = 75379 contains very interesting case from Newtown Square that may related to 
# 
# the Kecksburg incident. Very 
# key = 35585 is a possible hudson valley style ufo near dover air force base

library(tidyverse)

datadir <- sprintf("%s/%s", here::here(), "04.findings.1/data")

load(file = sprintf("%s/%s", datadir, "nuforc_reports.rdata"))

# Average Time of Encounter

nuforc_reports <- 
  nuforc_reports %>% 
  mutate(duration_time_in_seconds = 
          case_when(
            duration_unit == "seconds" ~ duration_time,
            duration_unit == "minutes" ~ duration_time * 60,
            duration_unit == "hours" ~ duration_time * 60 * 60,
            TRUE ~ -1
           ),
         duration_time_in_seconds = ifelse(duration_time_in_seconds == -1, NA, duration_time_in_seconds),
         duration_time_in_minutes = duration_time_in_seconds / 60,
         duration_time_in_hours = duration_time_in_minutes / 60)

# Remove time values that I found are clearly wrong

nuforc_reports <- 
  nuforc_reports %>% 
  filter(!key %in% c('31427','75379','44692'),
         duration_time_in_seconds != -1)

# Summary stats duration in seconds
#  NOTE day or more is removed from calculation

summarize_duration <- function(s){
  if(!is.na(s)){
    d <- 
      nuforc_reports %>% 
      filter(shape == s)
  } else {
    d <- 
      nuforc_reports
  }
  d %>% 
    summarise(minutes_p01 = quantile(duration_time_in_minutes, 0.01, na.rm = TRUE),
              minutes_p05 = quantile(duration_time_in_minutes, 0.05, na.rm = TRUE),
              minutes_p10 = quantile(duration_time_in_minutes, 0.10, na.rm = TRUE),
              minutes_p50 = quantile(duration_time_in_minutes, 0.50, na.rm = TRUE),
              minutes_p90 = quantile(duration_time_in_minutes, 0.90, na.rm = TRUE),
              minutes_p95 = quantile(duration_time_in_minutes, 0.95, na.rm = TRUE),
              minutes_p99 = quantile(duration_time_in_minutes, 0.99, na.rm = TRUE),
              minutes_max = max(duration_time_in_minutes))
}

summarize_duration(NA)

# Look at shapes vs duration

d <- 
  nuforc_reports %>% 
  count(shape) %>% 
  arrange(desc(n))

summarize_duration("light")
summarize_duration("circle")
summarize_duration("triangle")
summarize_duration("fireball")

# Use map to do this for all

# Histogram to show duration

nuforc_reports %>%
  filter(duration_time_in_minutes >= .025,
         duration_time_in_minutes <= 210) %>%
  ggplot(aes(duration_time_in_minutes)) +
  geom_histogram(bins = 30) +
  labs(x="Minutes Observed",
       y="Number of UFO Reports",
       title="Duration of UFO Sightings") +
  theme_minimal()


# Code to pull report text by key
clipr::clear_clip()
nuforc_reports %>%
  filter(key == 3) %>% 
  select(text) %>% clipr::write_clip()

library(corrr)

nuforc_cor <- 
  nuforc_reports %>% 
  select(latitude,longitude) %>% 
  correlate(use = "na.or.complete")

tbl = matrix(data=c(55, 45, 20, 30), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

