library(shiny)
#---------------------------------------------------------------
# UI for Exploratory Data Analysis & Visualization team project
#---------------------------------------------------------------
shinyUI(fluidPage(
        
        # Application title
        titlePanel("JobKit: NYC"),
        
        # Sidebar with a slider input for the number of bins
        #sidebarLayout(position="left",
        #        sidebarPanel(
        #        ),
                
        # Show a plot of the top-rated companies based in NYC
        #        mainPanel(
        #                #plotOutput("distPlot")
        fluidRow(
                column(2,
                        textInput(),
                        actionButton("search", label="Search"),
                        br(),
                        br()
        #        )
        #)
))
 # Who is hiring?
 # Job search
 # Values Match
        
        