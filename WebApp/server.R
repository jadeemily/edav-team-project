library(shiny)

shinyServer(function(input, output, session) {
        defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
        
        series <- structure(
                lapply(defaultColors, function(color) { list(color=color) }),
                names = levels(data$Industry)
        )
        
        ratingData <- reactive({
                # Filter to the desired rating, and put the columns
                # in the order that Google's Bubble Chart expects
                # them (name, x, y, color, size). Also sort by industry
                # so that Google Charts orders and colors the industries
                # consistently.
                df <- data %.%
                        filter(Rating == input$rating) %.%
                        select(Country, Health.Expenditure, Life.Expectancy,
                               Region, Population) %.%
                        arrange(Industry)
        })
        
        output$chart <- reactive({
                list(
                        data = googleDataTable(ratingData()),
                        options = list(
                                title = sprintf(
                                        "Company ratings vs. industry, %s",
                                        input$rating),
                                series = series
                        )
                )
        })
})