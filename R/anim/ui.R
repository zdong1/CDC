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
  
  # Application title
  titlePanel("GPS Records Visualizer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    radioButtons("map", "Map View:",
                   c("Cluster Map" = "clu",
                     "Proportion Map" = "prop",
                     "L1 Error Map" = "l1e")),
    sliderInput("bins",
                   "Proportion Map/ L1 Error Map Observation:",
                   min = 1,
                   max = max(person6106$week),
                   value = 30)
    ,
    sliderInput("range", 
                label = "Observation Weeks",
                min = 0, max = max(person6106$week), value = c(0, max(person6106$week)))
    ,
    numericInput("num1", 
                 h5("Elimination Policy: Least Number of Observations"), 
                 value = 10)  
    ,
    numericInput("num2", 
                 h5("Grid Size Specification: Easting (default 4,000)"), 
                 value = 4000)  
    ,
    numericInput("num3", 
                 h5("Grid Size Specification: Northing (default 4,000)"), 
                 value = 4000) 
    ,
    numericInput("num4", 
                 h5("Easting Lower Limit"), 
                 value = 4000)  
    ,
    numericInput("num5", 
                 h5("Easting Upper Limit"), 
                 value = 365936.1) 
    ,
    numericInput("num6", 
                 h5("Northing Upper Limit"), 
                 value = 5165759.3) 
    ,
    numericInput("num7", 
                 h5("Northing Lower Limit"), 
                 value = 5117012.8) 
    ,
    xfrom = 284374.8, xto = 365936.1, 
    selectInput("var", 
                label = "Queen Option",
                choices = list("True" = TRUE, 
                               "False" = FALSE),
                selected = TRUE)
    ,
    selectInput("var2", 
                label = "Zero Policy",
                choices = list("True" = TRUE, 
                               "False" = FALSE),
                selected = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
