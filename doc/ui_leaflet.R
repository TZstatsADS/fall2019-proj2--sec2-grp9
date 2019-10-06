#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Restaurant"),
  # sidebarLayout(
  #   sidebarPanel(selectInput("year",label="year",
  #                            choices=list("2015"=2015,"2016"=2016,"2017"=2017,"2018"=2018,"2019"=2019)),
  #                sliderInput("slider","slider",min=0, max=20,value=10)),
                 
    #verbatimTextOutput("select"),
    #mainPanel("main",plotOutput("plot"))
    mainPanel( 
      #this will create a space for us to display our map
      leafletOutput(outputId = "map",width = "100%", height = "400px")
  )
))

