library(shiny)
library(shinydashboard)

function(input, output) {

        output$text1 <- renderText({
                paste("Top 25 industries in New York City by ", input$industry_plot)
        })

        output$plot1 <- renderPlot({
                x <- input$industry_plot
                if (x == "overall rating") {
                        overall_rating_plot
                } else if (x == "culture and values") {
                        culture_rating_plot
                } else if (x == "compensation and benefits") {
                        compensation_rating_plot
                } else if (x == "career opportunities") {
                        career_opportunities_plot
                } else if (x == "work-life balance") {
                        worklife_plot
                } else if (x == "overall rating (and hiring data scientists)") {
                        hiring_plot
                }
        })

}

