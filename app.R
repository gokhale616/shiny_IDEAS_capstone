# The simplest bargraph 
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
# setwd("~/Desktop/shiny_IDEAS_capstone-master/paige")
################################################################################################# 
######## with continuous variable side bars #####################################################
#################################################################################################
# app <- shinyApp(
#   ui = fluidPage(
# 
#     sliderInput(inputId = "masskg",
#                 label = "Less than (Kg):",
#                 value = 5, min = 0.5, max = 1000),
# 
#     sliderInput(inputId = "homerange",
#                 label = "Less than (km sqrd.)",
#                 value = 10, min = 0.02, max = 77500),
# 
#     checkboxGroupInput(inputId = "mamOrd",
#                        label = "Mammal order:",
#                        choices = c("Carnivores" = "carnivores",
#                                    "Ungulates" = "ungulates"),
#                        selected = c("carnivores", "ungulates")),
# 
#     checkboxGroupInput(inputId = "contin",
#                        label = "Continent:",
#                        choices = c("Africa" = "Africa",
#                                    "Asia" = "Asia",
#                                    "Australia" = "Australia",
#                                    "Europe" = "Europe",
#                                    "North America" = "North America"),
#                        selected = c("Africa", "Asia", "Australia", "Europe", "North America")),
# 
#     selectInput(inputId = "grp",
#                 label = "Grouping variable:",
#                 choices = c("Family" = "HostFamily",
#                             "Environment" = "HostEnvironment")),
# 
#     plotOutput("plot")
#   ),
#   server = function(input, output) {
#     mammals <- read.csv("distinct-hosts.csv")
# 
#     dat <- reactive({
# 
#       foo <- mammals %>%
#         filter(Group %in% input$mamOrd) %>%
#         filter(continent %in% input$contin) %>%
#         filter(hostHomeRange <= input$homerange) %>%
#         filter(massKG <= input$masskg) %>%
#         # unclear why this .dots arg is neccessary but it is...
#         group_by(.dots=input$grp, type) %>%
#         tally() %>% data.frame()
# 
#       return(foo)
#     })
# 
#     output$plot <- renderPlot(
#       # needs to be evaluated as a string
#       ggplot(dat(), aes_string(fill="type", y="n", x=input$grp)) +
#         geom_bar(stat="identity", position = "fill") +
#         labs(x="", y="", fill="")  +
#         theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15))
# 
#     )
#   }
# )
# 
# runApp(app)

#########################################################
######### generate a plot with a hover event ############
#########################################################


app <- shinyApp(
  ui = fluidPage(

    sliderInput(inputId = "masskg",
                label = "Less than (Kg):",
                value = 5, min = 0.5, max = 1000),

    sliderInput(inputId = "homerange",
                label = "Less than (km sqrd.)",
                value = 10, min = 0.02, max = 77500),

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

    plotOutput("barplot")
  ),
  server = function(input, output) {
    mammals <- read.csv("distinct-hosts.csv")

    dat <- reactive({

      foo <- mammals %>%
        filter(Group %in% input$mamOrd) %>%
        filter(continent %in% input$contin) %>%
        filter(hostHomeRange <= input$homerange) %>%
        filter(massKG <= input$masskg) %>%
        # unclear why this .dots arg is neccessary but it is...
        group_by(.dots=input$grp, type) %>%
        tally() %>% data.frame()

      return(foo)
    })

    output$barplot <- renderPlotly({
        # needs to be evaluated as a string
        ggplot(dat(), aes_string(fill = "type", y = "n", x = input$grp)) +
        geom_bar(stat="identity", position = "fill") +
        labs(x="", y="", fill="")  +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) -> p

      
        ggplotly(p)

    })
  }
)

runApp(app)



