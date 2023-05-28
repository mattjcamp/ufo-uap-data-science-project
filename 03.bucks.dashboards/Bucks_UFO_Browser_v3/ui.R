#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(shiny)

# d <- read_csv(file = "./03.bucks.dashboards/Bucks_UFO_Browser_v2/nuforc_reports_past_10_years_bucks.csv") %>% 
#   select(date_occurred) %>% 
#   mutate(
#     year = year(date_occurred)
#   ) %>% 
#   glimpse()


years <- 
  read_csv(file = "nuforc_reports_past_10_years_bucks.csv") %>%
    mutate(
      year = year(date_occurred)
    ) %>%
  distinct(year) %>% 
  arrange(year)

# Define UI for application that draws a histogram
fluidPage(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
  
    titlePanel("Bucks County UFO Case Viewer By Date"),

    sidebarLayout(
        sidebarPanel(
          
          selectizeInput("year", "Year",
                         choices = years,
                         multiple = FALSE),

          uiOutput("reactiveControls"),
        ),

        mainPanel(
          htmlOutput("title"),
          htmlOutput("detail"),
        ),
        

        
    ),
    
    br(),
    hr(),
    
    htmlOutput("description", class = "description_textbox"),
    br(),
    htmlOutput("textAnalysis"),
    tableOutput("nprTable"),

    
)
