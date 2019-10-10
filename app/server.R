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
library(RColorBrewer)


load("../output/score_dist.RData")
load("../output/Quarter_scores.RData")
load("../output/violation.RData")
load("../output/top10violations.RData")



df_result_omit <- df_result_omit %>%
  filter(as.numeric(zipcode)>0) %>%
  mutate(region=as.character(zipcode))

df_result_omit$average_score <- round(df_result_omit$average_score,2)

count.df <- df_result_omit%>%
  group_by(region)%>%
  summarise(value=mean(average_score))

dt3 <- dt3 %>%
  mutate(inspection_year = format(as.Date(INSPECTION.DATE, "%m/%d/%Y"), "%Y"))

top10 <- dt3 %>%
  filter(dt3$VIOLATION.CODE %in% top10violations$Var1)


popup_label <- sprintf(
  "Zip Code: <strong>%s</strong><br/>Average Score: <strong>%s</strong><br/>Average ",
  as.character(df_result_omit$region), df_result_omit$average_score) %>%
  lapply(htmltools::HTML)

#####################################

### Graph 1 ####
shinyServer(function(input, output) {

  output$plot<-renderPlot({
    
    df <- read.csv("../output/insp_freq.csv")
    
    freq <- df %>%
      group_by(Year, Month) %>%
      count() %>%
      filter(Year >= 2016)
    freq <- freq[-46,]
      
    ggplot(data = freq, aes(x =  as.numeric(Month), y = n, color = as.factor(Year)))+
      geom_line() + geom_point() + scale_x_continuous(breaks = seq(1,12))+
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust=0.5, face="bold")) +
      xlab("Month") + 
      ylab("Frequency") + 
      ggtitle("Inspection Frequency")
  })
  
  #### GRAPH 2 #####
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

  #### GRAPH 3 ####
  output$plot3 <- renderPlot({
    
    if(input$year2>0){
      df_year_violation = top10 %>%
        filter(inspection_year %in% as.numeric(input$year2))
    }
    
  ggplot(df_year_violation, aes(x= factor(VIOLATION.CODE),group=inspection_year))+
    geom_bar(aes(y = (..count..)/sum(..count..)),fill="skyblue4")+
    #scale_fill_brewer(palette="Purples")+
    ggtitle("Frequncy of 10 Most Frequent Violation Types")+
    xlab("Types of Violation")+
    ylab("Frequency")+
    theme(legend.position = "none")
  })

  
  #### MAP ####
  output$map<-renderLeaflet({
    # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
    
    NYCzipcodes <- readOGR("../data/ZIP_CODE_040114.shp",
                           #layer = "ZIP_CODE", 
                           verbose = FALSE)
    df_result_omit=
      df_result_omit%>%
      filter(as.numeric(zipcode)>0)%>%
      mutate(region=as.character(zipcode)) 
    if(input$year2>0){
      df_year_map=df_result_omit%>%
        filter(year %in% as.numeric(input$year2))
    }
    count.df=df_result_omit%>%
      group_by(region)%>%
      summarise(
        value = mean(average_score)
      )
    
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% df_year_map$region)
    
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
      palette = "Blues",
      domain = subdat$value
    )
    
    leaflet(subdat) %>%
      addTiles()%>%
      #addProviderTiles("Wikimedia") %>%
      #addProviderTiles("Stamen.TonerHybrid") %>%
      addProviderTiles("MapBox") %>%
      addPolygons(
        stroke = T, weight=1,
        fillOpacity = 0.6,
        label = popup_label,
        color = ~pal(value)
      ) %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                position = "topleft")
    
    
    
  })
  
  
  
  output$table<-renderTable({
    violation
  })
  
}

)


