#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(here)
library(lubridate)

nuforc_reports <- 
  read_csv(file = "nuforc_reports_past_10_years_bucks.csv",col_types = cols(.default = "c"))

cities <- 
  nuforc_reports %>% 
  distinct(city) %>% 
  arrange(city)

function(input, output, session) {
  
  filteredCases <- reactive({
    
    cases <- 
      nuforc_reports %>% 
      filter(city == input$city) %>% 
      select(case = key) %>% 
      arrange() %>% 
      as.list()
  
    cases$case
    
  })
  
  output$reactiveControls <- renderUI({
    tagList(
  
      selectizeInput("num_case", "Case Number",
                     choices = filteredCases(),
                     multiple = FALSE)
      
    )
  })
  
    output$description <- renderText({
      if(!is.na(input$num_case)){
        nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(description) %>% 
          as.character()
      }
      
      formatted <- paste(
        "<span style='font-weight: bold;'>",
        "Report",
        "</span><br/>",
        
        nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(description) %>% 
          as.character()
        
      )
      
      HTML(formatted)
      
      
    })

    output$title <- renderText({
      if(!is.na(input$num_case)){
        city <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(city) %>% 
          as.character()
        
        date_occurred <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(date_occurred) %>% 
          as.character()
        
        time_occurred <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(time_occurred) %>% 
          as.character()
        
        t <- as.POSIXlt("2023-05-12") + as.period(time_occurred)
        
        t <- format(t, "%I:%M%p")
        
        day_occurred <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(day_of_week) %>% 
          as.character()
        
        formatted <- paste(
          "<span style='font-weight: bold;'>",
          city,
          "</span>",
          "<br/><span style='font-weight: light;font-size:smaller'>",
          day_occurred, date_occurred, t,
          "</span>"
        )
        
        HTML(formatted)
        
      }
    })
    
    output$detail <- renderText({

        shape_bin <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(shape_bin) %>% 
          as.character()
        
        shape <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(shape) %>% 
          as.character()
        
        d <- ifelse(shape_bin == shape, shape_bin, sprintf("%s -> %s", shape_bin, shape))
        
        duration <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(duration_in_minutes) %>% 
          as.character()
        
        formatted <- paste(
          "<span style='font-weight: bold;'>",
          "Shape",
          "</span>",
          d,
          "<br/><span style='font-weight: bold;'>",
          "Duration",
          "</span>",
          duration,
          "minutes"
          
        )
        
        HTML(formatted)
      
      
    })
    
    output$textAnalysis <- renderText({
      if(!is.na(input$num_case)){
        perc_positive <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(perc_positive) %>% 
          as.character()
        
        dominate_emotion <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(dominate_emotion) %>% 
          as.character()
        
        afinn_sentiment_score <- 
          nuforc_reports %>% 
          filter(key == input$num_case) %>% 
          select(afinn_sentiment_score) %>% 
          as.character()
        
        formatted <- paste(
          "<span style='font-weight: bold;'>",
          "Text Analysis",
          "</span><br/>",
          "<span style='font-weight: bold;font-size:smaller;'>Perc Positive</span>",
          paste0(perc_positive, "%"),
          "<br/>",
          "<span style='font-weight: bold;font-size:smaller;'>Dominate Emotion</span>",
          dominate_emotion,
          "<br/>",
          "<span style='font-weight: bold;font-size:smaller;'>Sentiment Score</span>",
          afinn_sentiment_score
        )
        
        HTML(formatted)
        
      }
    })
    
}
