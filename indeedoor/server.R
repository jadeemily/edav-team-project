library(shiny)
library(shinydashboard)
library(data.table)

library(leaflet)
library(maps)
library(scales)
library(rgdal)
library(rCharts)
#library(ggvis)


function(input, output, session) {
        ##--------------------------------
        ## Industry analysis and job list
        ##--------------------------------
        output$text1 <- renderText(paste("Top industries in New York City by", input$industry_plot))

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

        output$joblist <- renderDataTable(as.data.table(jobdt), options = list(paging=TRUE), escape = FALSE)

        ##-------------------
        ## Job map
        ##-------------------
        scrape_pop <- paste0("<strong>City: </strong>", jobmap$city,
                             "<br><strong>Job Title: </strong>", jobmap$job_title,
                             "<br><strong>Company: </strong>", jobmap$company,
                             "<br><strong>When posted: </strong>", jobmap$posted_at,
                             "<br><strong>Glassdoor industry: </strong>", jobmap$industry,
                             "<br><strong>Glassdoor number of reviews: </strong>", jobmap$number_of_reviews,
                             "<br><strong>Glassdoor overall rating: </strong>", jobmap$overall_rating)

        #m2 <- leaflet(data = map_df) %>% addTiles('http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
        #                                          attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        m2 <- leaflet(data = jobmap) %>% addTiles('http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
              attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
              setView(-73.9983273, 40.7471983, zoom = 12) %>%
              addCircles(lat = ~ jitter(jobmap[,"lat"],10), lng = ~ jitter(jobmap[ ,"long"],10), color = '#4d4698',
                         radius = 200, popup = scrape_pop, weight = 3)

        withProgress({
                setProgress(message = "Processing...")
                output$ClistMap = renderLeaflet(m2)
        })

        output$joblist2 <- renderDataTable(as.data.table(jobdt), options = list(paging=TRUE), escape = FALSE)

        ##-------------------
        ## Skills cloud
        ##-------------------
        # Define a reactive expression for the document term matrix
        terms <- reactive({
                # Change when the "update" button is pressed...

                # ...but not for anything else

                        withProgress({
                                setProgress(message = "Processing corpus...")
                                getTermMatrix(input$selection)
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
                withProgress({
                        setProgress(message = "Processing...")
                        getRegressionAnalysis(input$choice1, input$choice2, as.numeric(input$k),input$typeofcluster)
                })

        })

        output$myChart <- renderChart({
                v <- cl_terms()

                #validate(
                #  need(!is.null(input$choice1), "Processing..")
                #)
                xlabel <- ratingsVariables1[[input$choice1]]
                ylabel <- ratingsVariables1[[input$choice2]]
                #  plot(v$plotdata[["v1"]],v$plotdata[["v2"]],
                #         col = v$plotdata[["v3"]],
                #         pch = 20, cex = 3, xlab=xlabel, ylab=ylabel)
                #  legend('topleft', legend = c(1:input$k), lty = 1, lwd = 4, col=c(1:input$k) ,  bty='n', cex=1.5)
                xx<-v$plotdata
                displaycolor<-xx$v3
                r1 <- rPlot(v2 ~ v1, data = xx, color = "Clusters", type = 'point', tooltip = "#! function(item){return item.v4 } !#")
                r1$guides(x = list(title = xlabel, max = 1.1*max(xx$v1), min = 1.1*min(xx$v1) ))
                r1$guides(y = list(title = ylabel, max = 1.1*max(xx$v2), min = 1.1*min(xx$v2) ))
                r1$addParams( dom = 'myChart')


                #r1$set(dom = 'myChart')
                return(r1)
        })



        output$clusters <- renderTable({
                v <- cl_terms()
                v$name
        })

        output$parameterControls1 <- renderUI({
          xx <- getVariables(input$typeofcluster)
          selectInput("choice1", "Choose Ratings Variable #1:",
                      choices = xx, selected = "cultureAndValuesRating")
          #     selectInput("choice2", "Choose a Ratings Variable:",
          #                choices = ratingsVariables1, selected = "cultureAndValuesRating")

        })
        output$parameterControls2 <- renderUI({
                xx <- getVariables(input$typeofcluster)
                selectInput("choice2", "Choose Ratings Variable #2:",
                            choices = xx, selected = "pctApprove")
                #     selectInput("choice2", "Choose a Ratings Variable:",
                #                choices = ratingsVariables1, selected = "cultureAndValuesRating")

        })
}

