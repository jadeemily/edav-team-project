library(shiny)
library(shinydashboard)
install.packages("BH")
library(BH)

dashboardPage(
        skin="purple",
        dashboardHeader(title = "indeedoor"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Industries", tabName="industries", icon=icon("area-chart")),
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
                                                radioButtons("plot2", "Sort by:",
                                                             c("Overall rating",
                                                               "Culture and values",
                                                               "Compensation and benefits",
                                                               "Career opportunities",
                                                               "Work-life balance"),
                                                             selected = "Overall rating",
                                                             inline = TRUE)
                                        )
                                ),
                                fluidRow(
                                        box(
                                          "Industries ordered by overall rating",
                                          title = "Glassdoor ratings by industry for companies in New York City",
                                          width = 12,
                                          plotOutput("plot2", height="800px", width="1200px")
                                        )
                                )
                        )
                ),

                tags$head(
                        tags$link(rel = "stylesheet", type="text/css", href="custom.css")
                )
        )
)


