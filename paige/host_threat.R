# The simplest bargraph 
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
#setwd("~/Desktop/shiny_IDEAS_capstone-master/paige")
################################################################################################# 
######## with continuous variable side bars #####################################################
#################################################################################################
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
    
    
    radioButtons(inputId = "compare", 
                 label = "Compare distributions:", 
                 choices = c("No", "Continent", "Group"), 
                 selected = c("No")), 
    
    
    selectInput(inputId = "grp",
                label = "Grouping variable:",
                choices = c("Family" = "HostFamily",
                            "Environment" = "HostEnvironment")),
    
    sliderInput(inputId = "homerange",
                label = "Mammal home range (km sqrd.; log scale)",
                value = c(log(0.02), log(77500)), min = floor(log(0.02)), max = ceiling(log(77500))),
    
    sliderInput(inputId = "masskg",
                label = "Mammal Mass (Kg):",
                value = c(0.5, 1000), min =0.5, max = 1000),

    plotlyOutput("plot")
  ),
  server = function(input, output) {
    mammals <- read.csv("distinct-hosts.csv")

    dat <- reactive({

      if(input$compare == "No") {
        foo <- mammals %>%
          filter(Group %in% input$mamOrd) %>%
          filter(continent %in% input$contin) %>%
          filter(hostHomeRange >= exp(input$homerange[1]) & hostHomeRange <= exp(input$homerange[2])) %>%
          filter(massKG >= input$masskg[1] & massKG <= exp(input$masskg[2])) %>%
          # unclear why this .dots arg is neccessary but it is...
          group_by(.dots=input$grp, type) %>%
          tally() %>% data.frame()
        
       } else if (input$compare == "Continent") {
        foo <- mammals %>%
          filter(Group %in% input$mamOrd) %>%
          filter(continent %in% input$contin) %>%
          filter(hostHomeRange >= exp(input$homerange[1]) & hostHomeRange <= exp(input$homerange[2])) %>%
          filter(massKG >= input$masskg[1] & massKG <= input$masskg[2]) %>%
          # unclear why this .dots arg is neccessary but it is...
          group_by(.dots=input$grp, continent, type) %>%
          tally() %>% data.frame()
       } else {
         foo <- mammals %>%
           filter(Group %in% input$mamOrd) %>%
           filter(continent %in% input$contin) %>%
           filter(hostHomeRange >= exp(input$homerange[1]) & hostHomeRange <= exp(input$homerange[2])) %>%
           filter(massKG >= input$masskg[1] & massKG <= input$masskg[2]) %>%
           # unclear why this .dots arg is neccessary but it is...
           group_by(.dots=input$grp, Group, type) %>%
           tally() %>% data.frame()
      }
      
      
      
      

      return(foo)
    })

    output$plot <- renderPlotly(
      # needs to be evaluated as a string
      if(input$compare == "No") { 
      ggplot(dat(), aes_string(fill = "type", y = "n", x = input$grp)) +
        geom_bar(stat = "identity", color = "black") + 
        labs(x="", y="", fill="")  +
        theme_void() + 
        theme(legend.text=element_text(size=rel(1.5)),
              strip.text = element_text(size=18), 
              axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
              panel.spacing = unit(1.5, "lines"))
      } else if (input$compare == "Continent") {
        ggplot(dat(), aes_string(fill = "type", y = "n", x = input$grp)) +
          geom_bar(stat = "identity", color = "black") + 
          labs(x="", y="", fill="")  +
          facet_wrap(~continent) +
          theme_void() + 
          theme(legend.text=element_text(size=rel(1.5)),
                strip.text = element_text(size=18), 
                axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
                panel.spacing = unit(1.75, "lines"))
      } else {
        ggplot(dat(), aes_string(fill = "type", y = "n", x = input$grp)) +
          geom_bar(stat = "identity", color = "black") + 
          labs(x="", y="", fill="")  +
          facet_wrap(~Group) +
          theme_void() + 
          theme(legend.text=element_text(size=rel(1.5)),
                strip.text = element_text(size=18), 
                axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
                panel.spacing = unit(1.75, "lines"))
      }
        
       
        

    )
  }
)

runApp(app)


