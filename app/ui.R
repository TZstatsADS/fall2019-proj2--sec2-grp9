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
    sidebarLayout(sidebarPanel(
                  h5("Each violation of a regulation gets a certain number of points. 
                  The inspection score is the sum of the points."),
                 
                 checkboxGroupInput("cuisine",
                                    label="Cuisine",
                                    choices=list('American'='American',
                                                 'Chinese'='Chinese',
                                                 'Cafe'='Cafe/Coffee/Tea',
                                                 'Pizza'='Pizza',
                                                 'Italian'='Italian',
                                                 'Mexican'='Mexican',
                                                 'Latin' ='Latin (Cuban, Dominican, Puerto Rican, South & Central American)',
                                                 'Japanese'='Japanese',
                                                 'Bakery'='Bakery',
                                                 'Caribbean'='Caribbean'))),
                mainPanel(plotOutput("plot"),
                          plotOutput("plot2")))),
    
    
    #verbatimTextOutput("select"),
    
    tabPanel("Violation Type",
             sidebarLayout(sidebarPanel(selectInput("year2",label="year",
                                                    choices=list("2015"=2015,"2016"=2016,"2017"=2017,"2018"=2018,"2019"=2019))),
                           mainPanel(plotOutput("plot3"),
                                     tableOutput("table")))),
    
    
    tabPanel("Zipcode Details",
             sidebarLayout(sidebarPanel(selectInput("year2",label="year_for_map",
                                                            choices=list("2016"=2016,"2017"=2017,
                                                                         "2018"=2018,"2019"=2019))),
                                   mainPanel(leafletOutput("map",width=800,height=800))))
    

              
  
))

