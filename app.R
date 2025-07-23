#Required Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(plotly)

#Load source data
#Tornado data
tornado <- read_csv("C:/Users/mikeh/Desktop/DATA 824/Final Project/Tornado Data.csv")

#Conver date column to Date type
tornado$date <- as.Date(tornado$date)

#UI
UI <- fluidPage( titlePanel("U.S. Tornado Explorer"),
  sidebarLayout
  (
    #Create sidebar panel display
    sidebarPanel
    (
      #Create slider for year range selection
      #Default is set to select years range 2000-2020
      sliderInput("yearRange", "Select Year Range:",
                  min = min(tornado$yr, na.rm = TRUE),
                  max = max(tornado$yr, na.rm = TRUE),
                  value = c(2000, 2020),
                  sep = ""),
      
      #Create drop down menu for State selection
      #Default is set to select state of KS
      #Only states with tornado data are selectable
      selectInput("states", "Select States:",
                  choices = sort(unique(na.omit(tornado$st))),
                  selected = "KS",
                  multiple = TRUE),
      
      #Create slider for tornado magnitude range
      #Default is set to select magnitudes 0-5
      sliderInput("magRange", "Select Magnitude Range:",
                  min = 0,
                  max = 5,
                  value = c(0, 5),
                  step = 1),
    ),
    
    #Create main panel display
    mainPanel
    (
      tabsetPanel
      (
        #Create tab for map
        tabPanel("Map", leafletOutput("tornadoMap", height = 600)),
        #Create tab for yearly trends
        tabPanel("Trends", plotlyOutput("yearPlot")),
        #Create tab for state stats
        tabPanel("State Impact", plotlyOutput("statePlot")),
        #Create tab for magnitude distribution
        tabPanel("Magnitude Distribution", plotlyOutput("magPlot"))
      )
    )
  )
)

#Server
Server <- function(input, output, session) 
{
  
  filtered_data <- reactive({
    tornado %>%
      filter(
        yr >= input$yearRange[1],
        yr <= input$yearRange[2],
        mag >= input$magRange[1],
        mag <= input$magRange[2],
        st %in% input$states
      )
  })
  
  #Output for map that displays data by chosen State(s)
  output$tornadoMap <- renderLeaflet({
    data <- filtered_data()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~slon,
        lat = ~slat,
        color = ~colorNumeric("Reds", mag)(mag),
        radius = ~ifelse(mag > 0, mag * 2, 2),
        popup = ~paste0("Date: ", date, "<br>",
                        "Magnitude: ", mag, "<br>",
                        "Injuries: ", inj, "<br>",
                        "Fatalities: ", fat)
      )
  })
  
  #Output for the graph that displays yearly count by chosen State(s)
  output$yearPlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(yr) %>%
      summarise(count = n())
    
    p <- ggplot(data, aes(x = yr, y = count)) +
      geom_line(color = "darkblue") +
      geom_point() +
      labs(title = "Tornadoes per Year", x = "Year", y = "Count")
    
    ggplotly(p)
  })
  
  #Output for the barplot that displays fatalities and injuries by chosen State(s)
  output$statePlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(st) %>%
      summarise(
        Injuries = sum(inj, na.rm = TRUE),
        Fatalities = sum(fat, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(
        cols = c(Injuries, Fatalities),
        names_to = "Type",
        values_to = "Count"
      )
    
    p <- ggplot(data, aes(x = st, y = Count, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = c("Injuries" = "blue", "Fatalities" = "red")
      ) +
      labs(
        title = "Injuries and Fatalities by State",
        x = "State",
        y = "Count",
        fill = "Impact Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  #Output for the distribution of number of tornadoes at each magnitude by chosen State(s)
  output$magPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = factor(mag), fill = factor(mag))) +
      geom_bar(color = "black") +
      scale_fill_brewer(palette = "Set1", name = "Magnitude") +
      labs(
        title = "Magnitude Distribution",
        x = "Magnitude",
        y = "Count"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(UI, Server)

