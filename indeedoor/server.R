library(shiny)
library(shinydashboard)

library(leaflet)
library(maps)
library(scales)
library(rgdal)

function(input, output, session) {

        ##-------------------
        ## Industry analysis
        ##-------------------
        output$text1 <- renderText(paste("Top 25 industries in New York City by", input$industry_plot))

        output$plot1 <- renderPlot({
                x <- input$industry_plot
                withProgress({
                        setProgress(message = "Processing...")
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
                        } else if (x == "overall rating (hiring data scientists)") {
                                hiring_plot
                        }
                })
        })

        ##-------------------
        ## Job map
        ##-------------------
        scrape_pop <- paste0("<strong>City: </strong>", map_df$city,
                             "<br><strong>Job Title: </strong>", map_df$job_title,
                             "<br><strong>Company: </strong>", map_df$company,
                             "<br><strong>Date posted: </strong>", map_df$date,
                             "<br><strong>Glassdoor industry: </strong>", map_df$industry,
                             "<br><strong>Glassdoor number of reviews: </strong>", map_df$number_of_reviews,
                             "<br><strong>Glassdoor overall rating: </strong>", map_df$overall_rating)

        m2 <- leaflet(data = map_df) %>% addTiles('http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
        attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(-73.9983273, 40.7471983, zoom = 12)%>%
        addCircles(lat = ~ map_df[,"latitude"], lng = ~ map_df[ ,"longitude"], #color = ~pal2,
        radius = 200,
        popup= scrape_pop,
        weight = 3)

        withProgress({
                setProgress(message = "Processing...")
                output$ClistMap = renderLeaflet(m2)
        })

        ##-------------------
        ## Skills cloud
        ##-------------------
        # Define a reactive expression for the document term matrix
        terms <- reactive({
                # Change when the "update" button is pressed...
                input$update
                # ...but not for anything else
                isolate({
                        withProgress({
                                setProgress(message = "Processing corpus...")
                                getTermMatrix(input$selection)
                        })
                })
        })

        # Make the wordcloud drawing predictable during a session
        wordcloud_rep <- repeatable(wordcloud)

        output$plot2 <- renderPlot({
                v <- terms()
                wordcloud_rep(names(v), v, scale=c(4,0.5),
                              min.freq = input$freq, max.words=input$max,
                              colors=brewer.pal(8, "Dark2"))
        })

        ##--------------------------------------------
        ## Cluster analysis of ratings
        ##--------------------------------------------
        cl_terms <- reactive({
                # Change when the "update" button is pressed...
                input$update
                # ...but not for anything else
                isolate({
                        withProgress({
                                setProgress(message = "Processing...")
                                getRegressionAnalysis(as.numeric(input$choice1), as.numeric(input$choice2))
                        })
                })
        })

        output$plot3 <- renderPlot({
                v <- cl_terms()
                ggobj <- ggplot(v$plotdata,aes(x=v1, y = v2, size=8, color=v3))+geom_point()+ scale_colour_gradient(low="red", high = "blue") + ggtitle("Clusters and the Cluster Variables")
                print(ggobj)
        })

        output$clusters <- renderTable({
                v <- cl_terms()
                v$name
        })

}

