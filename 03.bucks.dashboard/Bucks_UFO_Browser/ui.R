#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(shiny)

cities <- 
  read_csv(file = "nuforc_reports_past_10_years_bucks.csv") %>% 
  distinct(city) %>% 
  arrange(city)

# Define UI for application that draws a histogram
fluidPage(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
  
    titlePanel("Bucks County UFO Case Viewer"),

    sidebarLayout(
        sidebarPanel(
          
          # selectizeInput("city", "City/Town",
          #                choices = cities,
          #                multiple = FALSE),

          uiOutput("reactiveControls"),
        ),

        mainPanel(
          htmlOutput("title"),
          htmlOutput("detail"),
        ),
        

        
    ),
    
    br(),
    hr(),
    
    htmlOutput("description"),
    br(),
    htmlOutput("textAnalysis"),
    tableOutput("nprTable"),

    
)
