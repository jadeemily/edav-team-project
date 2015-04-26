library(shiny)
library(shinydashboard)

function(input, output) {

        output$plot2 <- renderPlot({
                withProgress(message = 'Making plot...',
                             value = 0, {
                                     for (i in 1:15) {
                                             incProgress(1/15)
                                             Sys.sleep(0.25)
                                     }
                             })
                singleton(overall_rating_plot)
        })

}

