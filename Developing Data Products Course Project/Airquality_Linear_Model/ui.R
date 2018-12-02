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
  titlePanel("Predict Ozone Level with Linear Model"),
  
  sidebarLayout(
    sidebarPanel(
        h4("Please check the variables you wish to use as predictors and click Submit"),
        p("If you check only one variable, you will also get a fitted plot"),
        checkboxInput("check1", "Solar.R", value = FALSE),
        checkboxInput("check2", "Wind", value = FALSE),
        checkboxInput("check3", "Temp", value = FALSE),
        checkboxInput("check4", "Month", value = FALSE),
        submitButton("Submit"),
        h4("Please specify the value of your chosen predictors and click Submit"),
        sliderInput("solar", "Specify Solar.R value:", min = 0, max = 350, value = 175, step = 1),
        sliderInput("wind", "Specify Wind value:", min = 0, max = 25, value = 12.5, step = 0.1),
        sliderInput("temp", "Specify Temp value:", min = 55, max = 100, value = 78, step = 1),
        sliderInput("month", "Specify Month:", min = 5, max = 9, value = 7, step = 1),
        submitButton("Submit")
       ),
    
    mainPanel(
        h3("Build a Naive Linear Model with airquality Dataset and Predict Ozone value with it"),
        p("This app allows you to choose the predictors you want from the airquality dataset, and use
          them to build a simple linear model to predict Ozone level. From the side par, check the 
          variables you wish to use, then the app will build a linear model based on them. If you
          pick only one variable, this app will also plot its relationship with Ozone and draw a 
          fitted line over it. After that, you can specify the levels of variables you chose, 
          and this app will provide a prediction based on your input."),
        htmlOutput("hint"),
        plotOutput("plot"),
        htmlOutput("var"),
        htmlOutput("pred"),
        p("This app only builds a simple linear model based on selected predictors and input, 
          which may give negative or weird Ozone values due to its limitations. More advanved regression
          model is required to precisely predict Ozone values.")
    )
  )
))
