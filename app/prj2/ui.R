#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Restaurant",
  
  
    tabPanel("Plots",
    sidebarLayout(sidebarPanel(selectInput("year",label="year",
                             choices=list("2015"=2015,"2016"=2016,"2017"=2017,"2018"=2018,"2019"=2019)),
                 hr(),
                 hr(),
                 hr(),
                 checkboxGroupInput("cuisine",label="Cuisine",choices=list('American'='American',
                              'Chinese'='Chinese','Cafe/Coffee/Tea'='Cafe/Coffee/Tea',
                              'Pizza'='Pizza','Italian'='Italian','Mexican'='Mexican',
                              'Latin (Cuban, Dominican, Puerto Rican, South & Central American)'
                              ='Latin (Cuban, Dominican, Puerto Rican, South & Central American)',
                               'Japanese'='Japanese','Bakery'='Bakery','Caribbean'='Caribbean'))),
                mainPanel(plotOutput("plot"),
                          plotOutput("plot2")))),
  
    #verbatimTextOutput("select"),
      
      tabPanel("Map",leafletOutput("map",width=800,height=500)),
     tabPanel("Violation Type",
              sidebarLayout(sidebarPanel(selectInput("year",label="year",
                        choices=list("2015"=2015,"2016"=2016,"2017"=2017,"2018"=2018,"2019"=2019))),
              mainPanel(plotOutput("plot3"))))
              
  
))

