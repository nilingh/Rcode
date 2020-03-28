#

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    navbarPage(title = "COVID-19 NZ",
        tabPanel("Overview","contents"),
        tabPanel("Polpulation Comparisons",
                 sidebarLayout(
                     sidebarPanel(
                         textOutput("SideContent")
                     ),
                     
                     mainPanel(plotOutput("PopulationDist"), width = 8)
                 )
                 ),
        tabPanel("Current Caeses",
                 DT::dataTableOutput(outputId = "RawData"),
                 downloadButton("DownloadCsv", "Download as CSV"),
                 br(),
                 "Information about confirmed and probable cases of COVID-19 in New Zealand. Published by ",
                 tags$a(href="https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases", 
                        "Ministry of Health")
        )
    )
    
))
