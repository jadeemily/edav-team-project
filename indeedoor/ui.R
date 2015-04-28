library(shiny)
library(shinydashboard)
library(leaflet)

dashboardPage(
        skin="purple",
        dashboardHeader(title = "indeedoor"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Industries", tabName="industries", icon=icon("bar-chart")),
                        menuItem("Jobs", tabName="jobs", icon=icon("map-marker")),
                        menuItem("Skills", tabName="skills", icon=icon("asterisk")),
                        menuItem("Ratings Analysis", tabName="friends", icon=icon("users"))
                )
        ),

        dashboardBody(
                tabItems(
                        tabItem(tabName="industries",
                                fluidRow(
                                        box(
                                          width = 12,
                                          radioButtons("industry_plot", "Sort by:",
                                          c("Overall rating (hiring data scientists)" = "overall rating (hiring data scientists)",
                                            "Overall rating" = "overall rating",
                                            "Culture and values" = "culture and values",
                                            "Compensation and benefits" = "compensation and benefits",
                                            "Career opportunities" = "career opportunities",
                                            "Work-life balance" = "work-life balance"),
                                            selected = "overall rating (hiring data scientists)",
                                            inline = TRUE)
                                        )
                                ),
                                fluidRow(
                                        box(
                                          title = h3(textOutput("text1")),
                                          width = 12,
                                          plotOutput("plot1", height="800px", width="1200px"),
                                          hr(),
                                          withTags({
                                                  a(href='http://www.glassdoor.com/index.htm',
                                                    'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                          })
                                        )
                                )
                        ),

                        tabItem(tabName='jobs',
                                fluidPage(
                                        box(
                                          title = h3('Data science job postings by location'),
                                          width = 12,
                                          #column(width = 10, offset= 1,
                                          #h3('Data science related job postings by location'),
                                          leafletOutput("ClistMap", "100%", 800),
                                          hr(),
                                          withTags({
                                                span(
                                                 a(href='http://www.indeed.com/','jobs by',
                                                   a(href  = 'http://www.indeed.com/',
                                                     title = 'Job Search',
                                                     img(src = 'http://www.indeed.com/p/jobsearch.gif',
                                                     style='border: 0;vertical-align: middle')
                                                   )))
                                          })
                                        )
                                )
                        ),

                        tabItem(tabName="skills",
                                fluidRow(
                                        box(
                                                title = h3("Selection"),
                                                width = 3,
                                                height = 800,
                                                selectInput("selection", "Choose a city:",
                                                            choices = dbcities),
                                                actionButton("update", "Change"),
                                                hr(),
                                                sliderInput("freq",
                                                            "Minimum Frequency:",
                                                            min = 1,  max = 20, value = 3),
                                                sliderInput("max",
                                                            "Maximum Number of Keywords:",
                                                            min = 1,  max = 50,  value = 20),
                                                hr(),
                                                withTags({
                                                        span(
                                                                a(href='http://www.indeed.com/','jobs by',
                                                                  a(href  = 'http://www.indeed.com/',
                                                                    title = 'Job Search',
                                                                    img(src = 'http://www.indeed.com/p/jobsearch.gif',
                                                                        style='border: 0;vertical-align: middle')
                                                                  )))
                                                })
                                        ),
                                        box(
                                                title = h3("Frequent words found in Data Science job postings"),
                                                width = 9,
                                                height = 800,
                                                plotOutput("plot2")
                                        )
                                )
                        ),

                        tabItem(tabName="friends",
                                fluidRow(
                                        box(
                                                title = h3("Selection"),
                                                width = 3,
                                                height = 1000,
                                                selectInput("choice1", "Choose a Ratings Variable:",
                                                            choices = ratingsVariables, selected = 1),
                                                selectInput("choice2", "Choose a Ratings Variable:",
                                                            choices = ratingsVariables, selected = 2),
                                                actionButton("update", "Change"),
                                                hr(),
                                                withTags({
                                                        a(href='http://www.glassdoor.com/index.htm',
                                                          'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                                })
                                        ),
                                        box(
                                                title = h3("Cluster analysis of Glassdoor ratings"),
                                                width = 9,
                                                height = 1000,
                                                plotOutput("plot3"),
                                                h4("Clusters and top 2 important factors within industry"),
                                                tableOutput("clusters")
                                        )
                                )
                        )
                ),

                tags$head(
                        tags$link(rel = "stylesheet", type="text/css", href="custom.css")
                )
        )
)


