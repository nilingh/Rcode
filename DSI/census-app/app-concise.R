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
    args <- switch(input$var,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
  
    do.call(percent_map, args)
    
    })
}

# Run the application
shinyApp(ui = ui, server = server)
