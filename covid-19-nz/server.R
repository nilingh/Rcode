#

library(shiny)

shinyServer(function(input, output) {
    
    output$PopulationDist <- renderPlot({
        ggplot(data = my_grouped_data, aes(fill=GENDER, y=NUMBER_OF_CASES, x=AGE)) +
            geom_bar(position = "dodge", stat="identity")
    })
    
    output$SideContent <- renderText({
        "Content shown soon"
    })
    
    # Display Raw Data
    output$RawData <- DT::renderDataTable({
        DT::datatable(data = my_raw_data,rownames = FALSE,filter = "top") 
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_NZ_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(my_raw_data, file)
        }
    )
})
