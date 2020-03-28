#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            # Added in the Lab
            numericInput(inputId = "A", label = "A", min = 0, max = 100, value = 1, width = 50), # Don't forget the comma here!!!
            
            # Added again 
            numericInput(inputId = "B", label = "B", min = 0, max = 100, value = 2, width = 50), # Don't forget the comma here!!!
            
            # Added output variable
            textOutput(outputId="Mult")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            # Added again 
            numericInput(inputId = "C", label = "C", min = 0, max = 100, value = 3) # Don't forget the comma here!!!
            
        )
    )
))
