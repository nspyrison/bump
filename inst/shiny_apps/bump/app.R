library(shiny)
library(gtrendsR)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Google Trends Over Time"),
  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Enter a keyword:", value = "Shiny"),
      dateRangeInput("date_range", "Select date range:", 
                     start = Sys.Date() - 365, 
                     end = Sys.Date())
    ),
    mainPanel(
      plotOutput("trendPlot")
    )
  )
)

# Define server logic for the application
server <- function(input, output) {
  output$trendPlot <- renderPlot({
    req(input$keyword)
    trends <- gtrends(keyword = input$keyword, 
                      time = paste(input$date_range[1], input$date_range[2]))
    trend_data <- trends$interest_over_time
    ggplot(trend_data, aes(x = date, y = hits)) +
      geom_line() +
      labs(title = paste("Google Trends for:", input$keyword),
           x = "Date", y = "Hits")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
