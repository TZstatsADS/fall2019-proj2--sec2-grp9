#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(rgdal)
library(dplyr)
library(leaflet)

load(file="score_dist.RData")
quarter.scores <- read.csv("../output/Quarter_scores.csv")


df_result_omit <- df_result_omit %>%
  filter(as.numeric(zipcode)>0) %>%
  mutate(region=as.character(zipcode))

count.df=df_result_omit %>%
  group_by(region) %>%
  summarise(
    value=mean(average_score)
  )


popup_labels <- sprintf(
  "Zip Code: <strong>%s</strong><br/> Average Score: <strong>%s</strong><br/>", 
  as.character(df_result_omit$zipcode), df_result_omit$average_score) %>% lapply(htmltools::HTML)

#####################################

shinyServer(function(input, output) {
  
  ####HISTOGRAM####
  output$plot <- renderPlot({
    if(input$year>0){
      
      df_year=df_result_omit%>%
        filter(year %in% as.numeric(input$year))
    }
    
    ggplot(df_year,aes(x=average_score)) +
      geom_histogram(aes(y=..density..),color="black",fill="white") +
      geom_density(alpha=0.2,fill="orchid4")
    
  })  
  
  ####TIME GRAPH#####
  output$plot2 <- renderPlot({
    
    df_cuisine <- quarter.scores %>%
      filter(CUISINE.DESCRIPTION %in% input$cuisine) 
    
    ggplot(df_cuisine, aes(x=QUARTER, y=AVG_SCORE, group=CUISINE.DESCRIPTION, color=CUISINE.DESCRIPTION)) +
      geom_line() + 
      ggtitle("Average Score Across Time") +
      ylab("Average Score") + 
      xlab("Quarter") +
      guides(color=guide_legend(title="Cuisine Type")) +
      theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8),
            plot.title = element_text(hjust=0.5, face="bold"))
    
    
  })
  
  ####MAP####
  output$map<-renderLeaflet({
    # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
    
    NYCzipcodes <- readOGR("ZIP_CODE_040114.shp",
                           #layer = "ZIP_CODE", 
                           verbose = FALSE)
    df_result_omit =
      df_result_omit %>%
      filter(as.numeric(zipcode)>0) %>%
      mutate(region=as.character(zipcode)) 
    
    count.df=df_result_omit %>%
      group_by(region) %>%
      summarise(
        value = mean(average_score)
      )
    
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
      #addProviderTiles("Wikimedia") %>%
      #addProviderTiles("Stamen.TonerHybrid") %>%
      addProviderTiles("MapBox") %>%
      addPolygons(
        stroke = T, weight=1,
        label = popup_labels,
        fillOpacity = 0.6,
        color = ~pal(value)
      ) %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
  
}

)
