library(shiny)
library(shinydashboard)

dashboardPage(
        skin="purple",
        dashboardHeader(title = "indeedoor"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Industries", tabName="industries", icon=icon("bar-chart")),
                        menuItem("Jobs", tabName="jobs", icon=icon("map-marker")),
                        menuItem("Skills", tabName="skills", icon=icon("asterisk"))
                )
        ),

        dashboardBody(
                tabItems(
                        tabItem(tabName="industries",
                                fluidRow(
                                        box(
                                          width = 12,
                                          radioButtons("industry_plot", "Sort by:",
                                          c("Overall rating + hiring data scientists" = "overall rating (and hiring data scientists)",
                                            "Overall rating" = "overall rating",
                                            "Culture and values" = "culture and values",
                                            "Compensation and benefits" = "compensation and benefits",
                                            "Career opportunities" = "career opportunities",
                                            "Work-life balance" = "work-life balance"),
                                            selected = "overall rating (and hiring data scientists)",
                                            inline = TRUE)
                                        )
                                ),
                                fluidRow(
                                        box(
                                          title = textOutput("text1"),
                                          width = 12,
                                          plotOutput("plot1", height="800px", width="1200px"),
                                          withTags({
                                                  a(href='http://www.glassdoor.com/index.htm',
                                                    'powered by', img(src='http://www.glassdoor.com/static/img/api/glassdoor_logo_80.png'))

                                          })
                                        )
                                )
                        )
                ),

                tags$head(
                        tags$link(rel = "stylesheet", type="text/css", href="custom.css")
                )
        )
)


