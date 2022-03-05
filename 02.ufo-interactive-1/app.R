#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)

load("nuforc_reports.rdata")
nuforc_reports <- 
    nuforc_reports %>% 
    mutate(date_time = ymd_hms(date_time),
           posted = ymd(posted))
d <- 
    nuforc_reports %>% 
    group_by(shape) %>% 
    count(shape) %>% 
    filter(!is.na(shape),
           shape != "unknown",
           shape != "changed",
           n > 20) %>% 
    arrange(desc(n))

s <- 
    nuforc_reports %>% 
    distinct(state) %>% 
    filter(!is.na(state),
           !is.null(state),
           state != "NULL")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NUFORC Dataset Interactive"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            selectInput("input_states",
                        label = "US State", 
                        choices = s,
                        multiple = FALSE,
                        selectize = TRUE, 
                        width = 200) 
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("shapePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$shapePlot <- renderPlot({
        
        m <- 
            nuforc_reports %>% 
            filter(state == input$input_states) %>% 
            group_by(shape) %>% 
            count(shape) %>% 
            filter(!is.na(shape),
                   shape != "unknown",
                   shape != "changed") %>% 
            arrange(desc(n))
            
        ggplot(m, aes(x = shape, y = n)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(y = "Number of Times Reported", 
                 x = "UFO Shape", 
                 title = paste("UFO Shapes Observed in " , input$input_states)) + 
            theme_minimal() + 
            theme(axis.text = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  title = element_text(size = 20)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
