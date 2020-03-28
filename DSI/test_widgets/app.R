#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("censusVis"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with information from the 2010 US Census."),
            
            selectInput(
                "select_color",
                "Choose a vriable to display",
                choices = c(
                    "Percent White" = "white",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "hispanci",
                    "Percent Asian" = "Asian"
                ),
                selected = "Percent White"
            ),
            
            sliderInput(
                "range",
                "Range of interest:",
                min = 0,
                max = 100,
                value = c(0,100)
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("color"),
            textOutput("select_var")
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$color <- renderText({
        paste("You have selected", input$select_color)
        
    })
    
    output$select_var <- renderText({
        paste("You have chosen a range that goes from", input$range[1], "to", input$range[2])
    })
}

# Run the application
shinyApp(ui = ui, server = server)
