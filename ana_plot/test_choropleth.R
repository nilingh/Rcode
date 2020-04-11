if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(visdat)) install.packages("visdat")
library(RColorBrewer)

prcp <- read_csv(
  'ana_data/prcp_annual.csv', 
  col_names = c("COUNTRY_NAME","YEAR","AVG_PRCP","NUMBER_OBS"), 
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


prcp_year <- prcp %>% filter(YEAR<=2020) %>% 
  group_by(COUNTRY_NAME) %>% 
  mutate(LAST_PRCP = last(CMEAN_PRCP, order_by = YEAR)) %>%
  select(COUNTRY_NAME, LAST_PRCP) %>% distinct()

# shape data
world_spdf <- rgdal::readOGR( 
  dsn= paste0(getwd(),"/ana_data/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_spdf@data <- world_spdf@data %>% left_join(prcp_year, by=c("NAME"="COUNTRY_NAME"))

# world_spdf@data %>% write_csv("ana_data/world_data.csv")
# prcp %>% write_csv("ana_data/prcp_country.csv")

#CSUM_PRCP CMEAN_PRCP

world_spdf@data %>% 
  ggplot( aes(x=as.numeric(LAST_PRCP))) + 
  geom_histogram(bins=30, fill='#69b3a2', color='white') +
  xlab("CMEAN_PRCP (M)") + 
  theme_bw()

csum_bins <- c(0,1000,5000,10000,20000,30000,40000,50000,100000,200000,300000)
cmean_bins <- c(0,200,400,600,800,1000,1500,2000,3000,4000)
mypalette <- colorBin( palette="YlGn", domain=world_spdf@data$LAST_PRCP, na.color="transparent", bins=cmean_bins)

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
  setView( lat=10, lng=0 , zoom=2) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
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
  addLegend( pal=mypalette, values=~LAST_PRCP, opacity=0.9, title = "Cumulative PRCP: (mm)", position = "bottomleft" )
  

