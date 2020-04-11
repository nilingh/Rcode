#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    # Basic layout with titlePanel and siderbarLayout
    # App title ----
    titlePanel("My Star Wars App"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar Panel for put elements with inputs or ...
        sidebarPanel(
            # Subtitle in siderbar
            h4("Fourth level title in siderbarPanel"),
            # Widgets
            helpText("Create demographic maps with 
               information from the 2010 US Census."),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = list("Percent White", 
                                       "Percent Black",
                                       "Percent Hispanic", 
                                       "Percent Asian"),
                        selected = "Percent White"),
            
            # Copy the line below to make a slider range 
            sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                        max = 100, value = c(40, 60))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3("Third level title in mainPanel"),
            h2("Second level title in mainPanel"),
            h1("First level title in mainPanel"),
            plotOutput("distPlot"),
            verbatimTextOutput("range")
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({
        
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "orange",
             xlab = "Waiting time to next eruption (in mins)",
             main = "Histogram of waiting times")
        
    })
    
    output$range <- renderPrint({ input$slider2[1] })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
