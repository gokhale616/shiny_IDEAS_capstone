# The simplest bargraph 
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)

app <- shinyApp(
  ui = fluidPage(
    checkboxGroupInput(inputId = "mamOrd", 
                       label = "Mammal order:",
                       choices = c("Carnivores" = "Carnivora",
                                   "Ungulates" = "Artiodactyla", 
                                   "Dasyuromorphia" = "Dasyuromorphia", 
                                   "Peramelemorphia" = "Peramelemorphia",
                                   "Rodentia" = "Rodentia"),
                       selected = c("Carnivora", "Artiodactyla")),

    plotOutput("plot")
  ),
  server = function(input, output) {
    parDat <- read.csv("paige/host-par.csv") 

    dat <- reactive({
      foo <- parDat %>%
        filter(order %in% input$mamOrd) %>% 
        group_by(type, order) %>% 
        summarise(tot=sum(n)) %>%
        arrange(desc(tot)) %>%
        
        # for labels of pie later
        mutate(lab.ypos = cumsum(tot) - 0.5*tot) %>% 
        data.frame()
      return(foo)
    })
    
    output$plot <- renderPlot(
      
      dat() %>% 
        
        # meat of the plot
        ggplot(aes(x="", y=tot, fill=type)) + 
        geom_bar(width = 1, stat = "identity", position="fill", color="black") + 
        coord_polar("y", start=0) + 
        facet_wrap(~order) + 
        
        # make it not look so terrible
        labs(x="", y="", fill="") + 
        theme_void() + 
        theme(legend.text=element_text(size=rel(1.5)),
              strip.text = element_text(size=25)) 
    )
  }
)

runApp(app)

