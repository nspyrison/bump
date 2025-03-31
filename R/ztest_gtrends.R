# devtools::install_github("PMassicotte/gtrendsR")
"CRAN release wasn't working, Dev version works"

library(gtrendsR)
library(ggplot2)


trends <- gtrends(keyword = "Covid19", 
                  time = "today 12-m"
                    #paste(input$date_range[1], input$date_range[2])
                  )
trend_data <- trends$interest_over_time
ggplot(trend_data, aes(x = date, y = hits)) +
  geom_line() +
  labs(title = paste("Google Trends for:", "Covid19"),
       x = "Date", y = "Hits")
})