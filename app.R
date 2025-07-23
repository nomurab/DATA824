library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(plotly)

# Load data
tornado <- read_csv("Tornado Data.csv")

# Convert date column to Date type
tornado$date <- as.Date(tornado$date)

# UI
ui <- fluidPage(
  titlePanel("U.S. Tornado Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Year Range:",
                  min = min(tornado$yr, na.rm = TRUE),
                  max = max(tornado$yr, na.rm = TRUE),
                  value = c(1990, 2020),
                  sep = ""),
      
      selectInput("states", "Select States:",
                  choices = sort(unique(na.omit(tornado$st))),
                  selected = "OK",
                  multiple = TRUE),
      
      sliderInput("magRange", "Select Magnitude Range:",
                  min = 0,
                  max = 5,
                  value = c(0, 5),
                  step = 1),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("tornadoMap", height = 600)),
        tabPanel("Trends", plotlyOutput("yearPlot")),
        tabPanel("State Impact", plotlyOutput("statePlot")),
        tabPanel("Magnitude Distribution", plotlyOutput("magPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
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
  
  output$yearPlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(yr) %>%
      summarise(count = n())
    
    p <- ggplot(data, aes(x = yr, y = count)) +
      geom_line(color = "steelblue") +
      geom_point() +
      labs(title = "Tornadoes per Year", x = "Year", y = "Count")
    
    ggplotly(p)
  })
  
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

# Run the app
shinyApp(ui, server)