#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#year_index = seq(2015,2019)
#names(year_index) = seq(1,5)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # output$plot <- renderPlot({
  #   ## First filter data for selected neighborhood
  #   if(input$year>0){
  #     df_year=df_result_omit%>%
  #       filter(year %in% as.numeric(input$year))
  #   }
  #   hist(df_year$average_score)
  # })
  output$map <- renderLeaflet({
    # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
    NYCzipcodes <- readOGR("ZIP_CODE_040114.shp",
                           #layer = "ZIP_CODE", 
                           verbose = FALSE)
    df_result_zip <- df_result_omit
    
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% df_result_zip$region)
    
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
    
    # ----- save the data slot
    subdat_data=subdat@data[,c("ZIPCODE", "POPULATION")]
    subdat.rownames=rownames(subdat_data)
    subdat_data=
      subdat_data%>%left_join(count.df, by=c("ZIPCODE" = "region"))
    rownames(subdat_data)=subdat.rownames
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = subdat$value
    )
    
    leaflet(subdat) %>%
      addTiles()%>%
      addPolygons(
        stroke = T, weight=1,
        fillOpacity = 0.6,
        color = ~pal(value)
      )
  })
}
    
)
  




