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
library(DT)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(reshape2)
library(wordcloud)
library(tidytext)
library(lubridate)

nuforc_reports <-
  read_csv(file = "nuforc_reports.csv", 
           col_types = cols(.default = "c"))

cities <-
  nuforc_reports %>%
  distinct(city) %>%
  arrange(city)

function(input, output, session) {
  filteredCases <- reactive({
    
    cases <- 
      nuforc_reports %>%
      arrange(date_occurred)
    
    # ONLY YEAR SELECTED
    if(!input$year == "All" && input$shape == "All"){
      
      message("ONLY YEAR SELECTED")
      
      cases <-
        cases %>%
        filter(
          year(date_occurred) == input$year
          )
    }
    
    # ONLY SHAPE SELECTED
    if(input$year == "All" && !input$shape == "All"){
      
      message("ONLY SHAPE SELECTED")
      
      cases <-
        cases %>%
        filter(
          shape_bin == input$shape
        )
    }
    
    # BOTH YEAR AND SHAPE SELECTED
    
    if(!input$year == "All" && !input$shape == "All"){
      
      message("BOTH YEAR AND SHAPE SELECTED")
      
      cases <-
        cases %>%
        filter(
          shape_bin == input$shape,
          year(date_occurred) == input$year
        )
    }
    
    cases <-
      cases %>%
      arrange(date_occurred) %>% 
      select(case = key) %>% 
      as.list()
    
    cases$case
    
  })
  
  output$reactiveControls <- renderUI({
    tagList(
      selectizeInput(
        "num_case",
        "Case Number",
        choices = filteredCases(),
        multiple = FALSE
      )
      
    )
  })
  
  output$description <- renderText({
    if (!is.na(input$num_case)) {
      nuforc_reports %>%
        filter(key == input$num_case) %>%
        select(description) %>%
        as.character()
      
      
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
      
    }
    
  })
  
  output$title <- renderText({
    if (!is.na(input$num_case)) {
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
        "<span style='font-weight: bold;font-size:larger;'>",
        city,
        "</span>",
        "<br/><span style='font-weight: light;font-size:smaller'>",
        day_occurred,
        date_occurred,
        t,
        "</span>"
      )
      
      HTML(formatted)
      
    }
  })
  
  output$detail <- renderText({
    if (!is.na(input$num_case)) {
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
      
      d <-
        ifelse(shape_bin == shape,
               shape_bin,
               sprintf("%s -> %s", shape_bin, shape))
      
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
    }
    
  })
  
  output$textAnalysis <- renderText({
    if (!is.na(input$num_case)) {
      
      report <- 
        nuforc_reports %>%
        filter(key == input$num_case)
      
      perc_positive <-
        report %>%
        select(perc_positive) %>%
        as.character()
      
      dominate_emotion <-
        nuforc_reports %>%
        filter(key == input$num_case) %>%
        select(dominate_emotion) %>%
        as.character()
      
      formatted <- paste(
        "<span style='font-weight: bold;'>",
        "Text Analysis",
        "</span><br/>",
        "<span style='font-weight: bold;font-size:smaller;'>Perc Positive</span>",
        paste0(perc_positive, "%"),
        "<br/>",
        "<span style='font-weight: bold;font-size:smaller;'>Dominate Emotion</span>",
        dominate_emotion
      )
      
      HTML(formatted)
      
    }
  })
  
  
  output$nprTable <- renderTable({
    # Replace "your_data" with your actual data frame or data source
    
    report <- 
      nuforc_reports %>%
      filter(key == input$num_case) %>% 
      select(
      perc_anger, perc_anticipation, perc_disgust, perc_fear,
      perc_joy, perc_sadness, perc_surprise, perc_trust)
    
    report 
  })
  
  
  output$map <- renderLeaflet({
    
    nuforc_reports %>% 
      filter(key %in% filteredCases()) %>% 
      mutate(
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
      ) %>% 
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
        # clusterOptions = markerClusterOptions(),
        label = ~sprintf("%s | %s | %s", city, shape_bin, date_occurred),
        popup = ~sprintf(
          "<h3>%s %s</h3>%s %s <br>%s | %s > %s %s</p><p>%s</p>", 
          city, str_to_title(shape), 
          date_occurred, format(strptime(time_occurred, format = "%HH %MM %SS"), format = "%I:%M %p"), 
          key, shape_bin, shape, duration,
          description)
      )
  })
  
  output$shape_freq <- renderPlot({
    
    shape_freq <-
      nuforc_reports %>%
      filter(key %in% filteredCases()) %>% 
      mutate(
        shape_bin = ifelse(is.na(shape_bin), "unknowns", shape_bin)
      ) %>%
      count(shape_bin) %>%
      arrange(desc(n))
    
    # colors <- c("red", "blue", "green", "yellow", "orange")
    
    ggplot(shape_freq, aes(x = reorder(shape_bin, n), y = n, fill = shape_bin)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("darkred", "darkgreen", "black", "darkblue", "darkorange", "gray", "gray", "gray", "gray", "gray"), 
                        breaks = c("lights", "disks", "triangles", "cigars", "teardrops", "unknowns", "changing", "sphere", "formation", "cross"), 
                        labels = c("lights", "disks", "triangles", "cigars", "teardrops", "unknowns", "changing", "sphere", "formation", "cross")) +
      xlab("UFO Shape") +
      ylab("Frequency") +
      ggtitle("UFO Shapes") +
      theme_minimal()
    
  })
  
  output$wordcloud <- renderPlot({
    
    tidy_cases <-
      nuforc_reports %>% 
      filter(key %in% filteredCases()) %>% 
      select(case = key, description) %>% 
      ungroup() %>%
      unnest_tokens(word, description)
    
    tidy_cases %>%
      filter(word != "object") %>% 
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("darkred", "darkgreen"),
                       max.words = 300)
    
  })
  
}
