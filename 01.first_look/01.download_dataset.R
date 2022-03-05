
install.packages("data.world")

library(data.world)
saved_cfg <- save_config(Sys.getenv("data.world_apikey")) # API key stored in .Renviron file
set_config(saved_cfg)

nuforc_reports <- query(
  qry_sql("
    SELECT * FROM nuforc_reports"),
  dataset = "https://data.world/timothyrenner/ufo-sightings"
)

save(nuforc_reports, file="nuforc_reports.rdata")

load("nuforc_reports.rdata")
