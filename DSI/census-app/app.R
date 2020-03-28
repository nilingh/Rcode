#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(maps)
library(mapproj)

# Load data ----
counties <- readRDS("data/counties.rds")

# Source helper functions / Source R code
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("censusVis"),
                
                sidebarLayout(
                    sidebarPanel(
                        helpText("Create demographic maps with
               information from the 2010 US Census."),
                        
                        selectInput(
                            "color",
                            label = "Choose a variable to display",
                            choices = c(
                                "Percent White",
                                "Percent Black",
                                "Percent Hispanic",
                                "Percent Asian"
                            ),
                            selected = "Percent White"
                        ),
                        
                        sliderInput(
                            "range",
                            label = "Range of interest:",
                            min = 0,
                            max = 100,
                            value = c(0, 100)
                        )
                        
                        # Output can be put in sidebarPanel as well
                        #textOutput("selected_var")
                        
                    ),
                    
                    mainPanel(
                        textOutput("color"),
                        textOutput("range"),
                        plotOutput("map")
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$color <- renderText({
        paste("You have selected", input$color)
    })
    output$range <- renderText({
        paste("You have chosen a range that goes from",
              input$range[1],
              "to",
              input$range[2])
        
    })
    output$map <- renderPlot({
        data <- switch(
            input$color,
            "Percent White" = counties$white,
            "Percent Black" = counties$black,
            "Percent Hispanic" = counties$hispanic,
            "Percent Asian" = counties$asian
        )
        
        color <- switch(input$color, 
                        "Percent White" = "darkgreen",
                        "Percent Black" = "black",
                        "Percent Hispanic" = "darkorange",
                        "Percent Asian" = "darkviolet")
        
        legend <- switch(input$color, 
                         "Percent White" = "% White",
                         "Percent Black" = "% Black",
                         "Percent Hispanic" = "% Hispanic",
                         "Percent Asian" = "% Asian")
        
        percent_map(
            data,
            color,
            legend ,
            input$range[1] ,
            input$range[2]
        )})
}

# Run the application
shinyApp(ui = ui, server = server)
