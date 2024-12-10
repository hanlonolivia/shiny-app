library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load the dataset
data <- read.csv("ncaamplayerstats24.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("College Basketball Player Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Select Position", 
                  choices = c("All", unique(data$Position)), 
                  selected = "All"),
      sliderInput("mpgRange", "Filter by Minutes Per Game (MPG):",
                  min = min(data$MPG), max = max(data$MPG), 
                  value = c(min(data$MPG), max(data$MPG))),
      sliderInput("fgaRange", "Filter by Field Goals Attempted (FGA):",
                  min = min(data$FGA), max = max(data$FGA), 
                  value = c(min(data$FGA), max(data$FGA))),
      downloadButton("downloadFiltered", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", 
                 DTOutput("dataView")),
        tabPanel("Summary Statistics",
                 verbatimTextOutput("summaryStats")),
        tabPanel("Interactive Visualizations",
                 plotOutput("scatterPlot"),
                 plotOutput("facetPlot"),
                 plotOutput("heatmapPlot")  # Heatmap added here
        ),
        tabPanel("Model Summaries",
                 tableOutput("modelSummary")),
        tabPanel("Outlier Analysis",
                 dataTableOutput("outliers"))
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Filtered data
  filteredData <- reactive({
    data %>% 
      filter(
        MPG >= input$mpgRange[1], MPG <= input$mpgRange[2],
        FGA >= input$fgaRange[1], FGA <= input$fgaRange[2],
        if (input$position != "All") Position == input$position else TRUE
      )
  })
  
  # Dataset Overview
  output$dataView <- renderDataTable({
    filteredData()
  })
  
  # Summary Statistics
  output$summaryStats <- renderPrint({
    summary(filteredData())
  })
  
  # Scatterplot
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes(x = MPG, y = FGA, color = Position)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Relationship between FGA and MPG",
        x = "Minutes Per Game (MPG)",
        y = "Field Goals Attempted (FGA)"
      ) +
      theme_minimal()
  })
  
  # Faceted Scatterplot
  output$facetPlot <- renderPlot({
    ggplot(filteredData(), aes(x = MPG, y = FGA)) +
      geom_point(alpha = 0.6, aes(color = Position)) +
      geom_smooth(method = "lm", se = FALSE, aes(color = Position)) +
      facet_wrap(~ Position, scales = "free") +
      labs(
        title = "FGA vs. MPG by Position",
        x = "Minutes Per Game (MPG)",
        y = "Field Goals Attempted (FGA)"
      ) +
      theme_minimal()
  })
  
  # Heatmap
  output$heatmapPlot <- renderPlot({
    ggplot(filteredData(), aes(x = MPG, y = FGA)) +
      geom_bin2d(bins = 30) +  # Adjust the number of bins for resolution
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
      labs(
        title = "Heatmap of FGA by MPG",
        x = "Minutes Per Game (MPG)",
        y = "Field Goals Attempted (FGA)"
      ) +
      theme_minimal()
  })
  
  # Model Summaries
  output$modelSummary <- renderTable({
    filteredData() %>%
      group_by(Position) %>%
      summarise(
        Intercept = coef(lm(FGA ~ MPG, data = cur_data()))[1],
        Slope = coef(lm(FGA ~ MPG, data = cur_data()))[2]
      )
  })
  
  # Outlier Analysis
  output$outliers <- renderDataTable({
    filteredData() %>%
      mutate(PredictedFGA = predict(lm(FGA ~ MPG, data = .))) %>%
      filter(abs(FGA - PredictedFGA) > 2)  # Threshold for outlier
  })
  
  # Download Filtered Data
  output$downloadFiltered <- downloadHandler(
    filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

