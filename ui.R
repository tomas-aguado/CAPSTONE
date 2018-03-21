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
  titlePanel("N-GRAMS word predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Enter a sentence, the system will complete the next word automatically, no need to push enter"),
      textInput("entryText", "ENTER THE TEXT TO PREDICT",value = ""),
      helpText("Use the slider to define how many possible texts you want the system to predict"),
      sliderInput("numResults",
                  "Number of possible results:",
                  min = 1,
                  max = 5,
                  value = 3)
    ),
    
    mainPanel(
      strong("Predicted sentence:"),
      verbatimTextOutput("prediction")
    )
  )
))
