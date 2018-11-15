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
  titlePanel("Clustering Output"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
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
    ,
    sliderInput("range", 
                label = "Observation Weeks",
                min = 0, max = max(person6106$week), value = c(0, max(person6106$week)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
