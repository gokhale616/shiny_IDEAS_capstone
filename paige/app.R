# The simplest bargraph 

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
    selectInput(inputId = "grpVar", 
                label = "Grouping variable:",
                choices = c("Host family" = "HostFamily"), 
                selected = c("Africa", "Asia", "Australia", "Europe", "North America")),
    
    plotOutput("plot")
  ),
  server = function(input, output) {
    mammals <- read.csv("distinct-hosts.csv")
    
    dat <- reactive({
      
      foo <- mammals %>%
        filter(Group %in% input$mamOrd) %>%
        filter(continent %in% input$contin) %>%
        group_by(HostFamily, type) %>% 
        tally()
      
      return(foo)
    })
    
    output$plot <- renderPlot(
      ggplot(dat(), aes(fill=type, y=n, x=HostFamily)) + 
        geom_bar(stat="identity", position = "fill") + 
        labs(y="", fill="")  + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
)

runApp(app)

