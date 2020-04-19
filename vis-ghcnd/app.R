############################
# Assigment 1
# DATA420-20S1 Scalable
# Zhen Huang
# 74093323
###########################

if(!require(shiny)) install.packages("shiny")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(visdat)) install.packages("visdat")
if(!require(lubridate)) install.packages("lubridate")
if(!require(leaflet)) install.packages("leaflet")
if(!require(rgdal)) install.packages("rgdal")
if(!require(RColorBrewer)) install.packages("RColorBrewer")

# load data
daily_nz <- read_csv('ana_data/daily_nz_t.csv',col_types = cols(
    ID = col_character(),
    DATE = col_date(format = '%Y%m%d'),
    ELEMENT = col_character(),
    VALUE = col_double(),
    MEASUREMENT_FLAG = col_character(),
    QUALITY_FLAG = col_character(),
    SOURCE_FLAG = col_character(),
    OBSERVATION_TIME = col_character()
))

def_start <- parse_date_time("1990-03-04","ymd")
def_end <- parse_date_time("2020-03-05","ymd")

# daily data
daily_tran1 <- daily_nz %>% select(ID, DATE, ELEMENT, VALUE) %>% mutate(VALUE = VALUE/10) %>% spread(ELEMENT, VALUE) %>% arrange(DATE)
col_max <- "#69b3a2"
col_min <- "#3aa0e8"
daily_tran2 <- daily_tran1 %>% group_by(DATE) %>% summarise(TMAX=mean(TMAX, na.rm=TRUE), TMIN=mean(TMIN, na.rm=TRUE))

# load prcp data
prcp <- read_csv(
    'ana_data/prcp_country.csv', 
    col_types = cols(
        COUNTRY_NAME = col_character(),
        YEAR = col_character(),
        AVG_PRCP = col_double(),
        NUMBER_OBS = col_integer()
    )
) %>% arrange(COUNTRY_NAME, YEAR)

prcp <- prcp %>% mutate(AVG_PRCP = round(AVG_PRCP/10,2)) %>% group_by(COUNTRY_NAME) %>% 
    mutate(
        CMEAN_PRCP = order_by(YEAR, cummean(AVG_PRCP)),
        CSUM_PRCP = order_by(YEAR, cumsum(AVG_PRCP))
    )

# shape data
world_spdf <- rgdal::readOGR( 
    dsn= paste0(getwd(),"/ana_data/world_shape_file/") , 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
)



# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE,
    title="Visualisation of Assignmen 1",
                 
    # Sidebar with a slider input for number of bins 
    tabPanel("Time Series",
             h4("Daily TMIN and TMAX of each NZ stations"),
             fluidRow(
                 column(3, dateRangeInput("dates", label = "Date range (1940-2020)", start = def_start, end = def_end)),
                 column(3, 
                        selectizeInput("stat", label = "Choose a NZ stations",
                                       choices = c('NZ000093012',
                                                   'NZ000093292',
                                                   'NZ000093417',
                                                   'NZ000093844',
                                                   'NZ000093994',
                                                   'NZ000933090',
                                                   'NZ000936150',
                                                   'NZ000937470',
                                                   'NZ000939450',
                                                   'NZ000939870',
                                                   'NZM00093110',
                                                   'NZM00093439',
                                                   'NZM00093678',
                                                   'NZM00093781',
                                                   'NZM00093929'),
                                       selected = "NZ000093012"),
                 )
             ),
             plotOutput("timeSeries"),
             hr(),
             h4("Average TMIN and TMAX for the entire country"),
             plotOutput("averageTime")
    ),
    tabPanel("Cumulative Rainfall",
             #div(class="outer",
             #tags$head(includeCSS("styles.css")),
             leafletOutput("choropleth",  height = "92vh"),
             
             absolutePanel(
                           bottom = 4, left = 220, width = 320,
                           draggable = TRUE,style = "opacity: 0.7",
                           wellPanel(
                           radioButtons("cmethod", label = ("Choose a method"),
                                        choices = list("Cumulative Average" = "CMEAN_PRCP", "Cumulative Sum" = "CSUM_PRCP"), 
                                        selected = "CMEAN_PRCP"),
                           sliderInput("syear", label = ("Years Range"), min = 1764, 
                                       max = 2020, value = c(1764,2020))
                           )
             )
            # )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$timeSeries <- renderPlot({
        start_date = input$dates[1]
        end_date = input$dates[2]
        daily_plot <- daily_tran1 %>% filter(ID==input$stat, DATE >= start_date, DATE <= end_date)
        ggplot(daily_plot, aes(x=DATE)) +
            geom_point(aes(y = TMAX), color=col_max, size=1) + 
            geom_point(aes(y = TMIN), color=col_min, size=1) +
            labs(x = "", y = "Temperature (Celsius Degree)") +
            theme(legend.position = "top")
    })
    
    output$averageTime <- renderPlot({
        start_date = input$dates[1]
        end_date = input$dates[2]
        daily_plot <- daily_tran2 %>% filter(DATE >= start_date, DATE <= end_date)
        ggplot(daily_plot, aes(x=DATE)) +
            geom_line(aes(y = TMAX), color=col_max, size=1) + 
            geom_line(aes(y = TMIN), color=col_min, size=1) +
            labs(x = "", y = "Temperature (Celsius Degree)") +
            theme(legend.position = "top")    
    })
    
    output$choropleth <- renderLeaflet({
        
        prcp_year <- prcp %>% filter(YEAR>=input$syear[1],YEAR<=input$syear[2]) %>% 
            group_by(COUNTRY_NAME) %>% 
            mutate(LAST_PRCP = last(eval(parse(text = input$cmethod)), order_by = YEAR)) %>%
            select(COUNTRY_NAME, LAST_PRCP) %>% distinct()
        
        world_spdf@data <- world_spdf@data %>% left_join(prcp_year, by=c("NAME"="COUNTRY_NAME"))
        
        if (input$cmethod=="CMEAN_PRCP") {
            c_bins <- c(0,200,400,600,800,1000,1500,2000,3000,4000,7000)
        }
        else {
            c_bins <- c(0,1000,5000,10000,20000,30000,40000,50000,100000,200000,300000)
        }
        mypalette <- colorBin(palette="YlGn", domain=world_spdf@data$LAST_PRCP, na.color="transparent", bins=c_bins)
        
        # Prepare the text for tooltips:
        mytext <- paste(
            "Country: ", world_spdf@data$NAME,"<br/>", 
            "Area: ", world_spdf@data$AREA, "<br/>", 
            "PRCP: ", round(world_spdf@data$LAST_PRCP, 2), 
            sep="") %>%
            lapply(htmltools::HTML)
        
        # Final Map
        leaflet(world_spdf) %>% 
            addTiles()  %>% 
            setView(lat=10, lng=20, zoom=3) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            # fitBounds(~-80,-80,~80,80) %>%
            addPolygons( 
                fillColor = ~mypalette(LAST_PRCP), 
                stroke=TRUE, 
                fillOpacity = 0.9, 
                color="white", 
                weight=0.3,
                label = mytext,
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                )
            ) %>%
            addLegend(pal=mypalette, values=~LAST_PRCP, opacity=0.8, title = "Cumulative PRCP: (mm)", position = "bottomleft" )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
