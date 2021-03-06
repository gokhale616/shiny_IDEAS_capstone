# The simplest bargraph 
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)

app <- shinyApp(
  ui = fluidPage(
    checkboxGroupInput(inputId = "mamOrd", 
                       label = "Mammal order:",
                       choices = c("Carnivores" = "carnivores",
                                   "Ungulates" = "ungulates"),
                       selected = c("carnivores", "ungulates")),
    checkboxGroupInput(inputId = "contin", 
                       label = "Continent:",
                       choices = c("Africa" = "Africa",
                                   "Asia" = "Asia",
                                   "Australia" = "Australia",
                                   "Europe" = "Europe",
                                   "North America" = "North America"), 
                       selected = c("Africa", "Asia", "Australia", "Europe", "North America")),
    selectInput(inputId = "grp", 
                label = "Grouping variable:",
                choices = c("Family" = "HostFamily", 
                            "Environment" = "HostEnvironment")),
    
    plotOutput("plot")
  ),
  server = function(input, output) {
    mammals <- read.csv("./paige/distinct-hosts.csv")
    
    dat <- reactive({
      
      foo <- mammals %>%
        filter(Group %in% input$mamOrd) %>%
        filter(continent %in% input$contin) %>%
        # unclear why this .dots arg is neccessary but it is... 
        group_by(.dots=input$grp, type) %>% 
        tally() %>% data.frame()
      
      return(foo)
    })
    
    output$plot <- renderPlot(
      # needs to be evaluated as a string
      ggplot(dat(), aes_string(fill="type", y="n", x=input$grp)) + 
        geom_bar(stat="identity", position = "fill", color="black") + 
        labs(x="", y="", fill="")  + 
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
              axis.text.y = element_text(size=20),
              legend.text=element_text(size=rel(1.5))) 
    )
  }
)

runApp(app)

