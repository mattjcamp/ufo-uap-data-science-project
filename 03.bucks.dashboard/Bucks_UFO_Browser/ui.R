#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(shiny)
library(here)

nuforc_reports <- 
  read_csv(file = "nuforc_reports_past_10_years_bucks.csv")

pos <- 1



# len <- nrow(nuforc_reports) %>% as.numeric()

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    # titlePanel(sprintf("Bucks County UFO Case Viewer %i", len)),
    titlePanel("Bucks County UFO Case Viewer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectizeInput("num_case", "Case Number", 
                         choices = nuforc_reports$key, 
                         multiple = FALSE)
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("title"),
          htmlOutput("detail"),
          htmlOutput("textAnalysis"),
          htmlOutput("description")
        )
    )
)
