library(shiny)
library(shinydashboard)
library(leaflet)
library(rCharts)

dashboardPage(
        skin="purple",
        dashboardHeader(title = "indeedoor"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Industries", tabName="industries",  icon=icon("bar-chart")),
                        menuItem("Jobs", tabName="jobs", icon=icon("map-marker")),
                        menuItem("Skills", tabName="skills", icon=icon("asterisk")),
                        menuItem("Ratings analysis", tabName="friends", icon=icon("users")),
                        menuItem(HTML("R&eacutesum&eacute help"), tabName="resume_help", icon=icon("wrench")),
                        menuItem("Contact us", icon=icon("envelope"), href="mailto:info@indeedoor.com")
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
                                            "Culture and values" = "culture and values rating",
                                            "Compensation and benefits" = "compensation and benefits rating",
                                            "Work-life balance" = "work-life balance rating",
                                            "Career opportunities" = "career opportunities rating"),
                                            selected = "overall rating (hiring data scientists)",
                                            inline = TRUE)
                                        )
                                ),
                                fluidRow(
                                        box(
                                          width = 12,
                                          title = textOutput("text1"),
                                          plotOutput("plot1", height="700px", width="100%"),
                                          hr(),
                                          withTags({
                                                  a(href='http://www.glassdoor.com/index.htm',
                                                    'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))
                                          })
                                        )
                                ),
                                fluidRow(
                                        box(
                                          width = 12,
                                          title = "Data science jobs in the New York City metro area",
                                          dataTableOutput('joblist'),
                                          hr(),
                                          withTags({
                                                span(
                                                    a(href='http://www.indeed.com/','jobs by',
                                                    a(href  = 'http://www.indeed.com/',
                                                    title = 'Job Search',
                                                    img(src = 'http://www.indeed.com/p/jobsearch.gif',
                                                        style='border: 0;vertical-align: middle')))
                                                )
                                          }),
                                          br(),
                                          withTags({
                                                  a(href='http://www.glassdoor.com/index.htm',
                                                    'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                          })
                                        )
                                )
                        ),

                        tabItem(tabName='jobs',
                                fluidRow(
                                        box(
                                          title = "Data science job postings by location*",
                                          "*Locations are approximate",
                                          width = 12,
                                          leafletOutput("ClistMap", "100%", 600),
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
                                ),
                                fluidRow(
                                        box(
                                          width = 12,
                                          title = "Data science jobs in the New York City metro area",
                                          dataTableOutput('joblist2'),
                                          hr(),
                                          withTags({
                                                span(
                                                a(href='http://www.indeed.com/','jobs by',
                                                  a(href  = 'http://www.indeed.com/',
                                                    title = 'Job Search',
                                                    img(src = 'http://www.indeed.com/p/jobsearch.gif',
                                                    style='border: 0;vertical-align: middle')
                                                 )))
                                          }),
                                          br(),
                                          withTags({
                                                  a(href='http://www.glassdoor.com/index.htm',
                                                    'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                          })
                                        )
                                )
                        ),

                        tabItem(tabName="skills",
                                fluidRow(
                                        box(
                                                title = "Selection",
                                                width = 3,
                                                height = 800,
                                                selectInput("selection", "Choose a city:",
                                                            choices = dbcities),
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
                                                title = "Frequent words found in Data Science job postings",
                                                hr(),
                                                width = 9,
                                                height = 800,
                                                plotOutput("plot2")
                                        )
                                )
                        ),

                        tabItem(tabName="friends",
                                fluidRow(
                                        box(
                                                title = "Selection",
                                                width = 3,
                                                height = 500,
                                                sliderInput("k",
                                                            "Number of Clusters:",
                                                            min = 2,  max = 8, value = 3),
                                                selectInput("typeofcluster", "Choose Analysis Variable",
                                                            choices = analysisvariables, selected = 1),
                                                uiOutput("parameterControls1"),
                                                uiOutput("parameterControls2"),

                                                hr(),
                                                withTags({
                                                        a(href='http://www.glassdoor.com/index.htm',
                                                          'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                                })
                                        ),
                                        box(
                                                title = "What makes people recommend companies?",
                                                print("We perform cluster analysis of the companies.
                                                 Overall Rating or Recommend To Friends is modeled as a function of the remaining ratings factors and the similar industries are then clustered.
                                                  We plot the clusters as a function of two of the factors and also list out the top two most important factors that influence
                                                  the ratings within an industry type.
                                                  You can vary the number of clusters and the plotting variables to observe what drives people in different kinds of industries."),
                                                width = 9,
                                                showOutput("myChart","polycharts"),
                                                h4("Clusters and top 2 important factors within industry"),
                                                tableOutput("clusters")
                                        )
                                )
                        ),

                        tabItem(tabName="resume_help",
                                fluidRow(
                                        box(
                                                title = HTML("Helpful keywords to have on your r&eacutesum&eacute"),
                                                width = 12,
                                                height = 1000,
                                                HTML("This is a prototype for a feature that would read a job posting and suggest important r&eacutesum&eacute; keywords that a
                                                qualified applicant should have.  The results shown here are based on a random sample of data science job postings in the New York City area on a given day in April 2015."),
                                                hr(),
                                                radioButtons("resume_category", "Show:",
                                                             c("Skills" = "specific skills",
                                                               "Coursework" = "relevant coursework",
                                                               "Expertise" = "expertise",
                                                               "At work" = "work related",
                                                               "Buzzwords" = "buzzwords"),
                                                             selected = "specific skills",
                                                             inline = TRUE),
                                                hr(),
                                                tags$strong(textOutput("rwtext")),
                                                br(),
                                                tableOutput("rwtable")
                                        )
                                )
                        )
                ),

                tags$head({
                        tags$script(src = "http://gdc.indeed.com/ads/apiresults.js")
                        tags$link(rel = "stylesheet", type="text/css", href="custom.css")
                })
        )
)
