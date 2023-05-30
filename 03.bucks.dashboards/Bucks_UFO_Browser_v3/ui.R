#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(shiny)

years <- 
  read_csv(file = "years.csv") %>% 
  mutate(year = as.character(year))
years <- 
  tibble(year = "All") %>% 
  bind_rows(years)

shapes <- 
  read_csv(file = "shapes.csv")
shapes <- 
  tibble(shape_bin = "All") %>% 
  bind_rows(shapes)


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

          # uiOutput("reactiveControls"),
          selectizeInput("shape", "Shape",
                         choices = shapes,
                         multiple = FALSE),
        ),

        mainPanel(
          plotOutput("shape_freq", height = 200),
        ),
        

        
    ),
    br(),
    hr(),
    leafletOutput(
      "map",
      width = "100%", 
      height = 600),
    br(),
    hr(),
    plotOutput("wordcloud"),
    br(),
    hr(),
    uiOutput("reactiveControls"),
    htmlOutput("title"),
    htmlOutput("detail"),
    br(),
    hr(),
    htmlOutput("description", class = "description_textbox"),
    br(),
    hr(),
    htmlOutput("textAnalysis"),
    tableOutput("nprTable"),

    
)
