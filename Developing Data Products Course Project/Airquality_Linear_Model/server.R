#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    data("airquality")

    model <- reactive({
        airquality <- airquality[which(complete.cases(airquality)), ] # Remove incomplete obs
        airquality$Month <- as.factor(airquality$Month)
        newdata <- subset(airquality, select = c(1,
                                                if (input$check1) {2},
                                                if (input$check2) {3},
                                                if (input$check3) {4},
                                                if (input$check4) {5}
                                                ))
        fit <- lm(Ozone ~ ., data = newdata)
        pred <- predict(fit, newdata = data.frame(Solar.R = input$solar,
                                                  Wind = input$wind,
                                                  Temp = input$temp,
                                                  Month = as.factor(input$month)))
    })
        
    output$hint <- renderText({
        if (sum(input$check1, input$check2, input$check3, input$check4) == 0)
            "<b>You did not specify any predictors</b>"
        else if (sum(input$check1, input$check2, input$check3, input$check4) == 1)
            paste("<b>Predictor chosen:", "<br>",
                  if (input$check1) {"Solar.R"},
                  if (input$check2) {"Wind"},
                  if (input$check3) {"Temp"},
                  if (input$check4) {"Month"},
                  "<br>",
                  "Here's the fitted plot:</b>")
        else paste("<b>Predictor chosen:", "<br>",
                   if (input$check1) {"Solar.R"},
                   if (input$check2) {"Wind"},
                   if (input$check3) {"Temp"},
                   if (input$check4) {"Month"},
                   "<br>",
                   "</b>")
    })
    
    output$plot <- renderPlot({
        if (sum(input$check1, input$check2, input$check3, input$check4) == 1){
             if (input$check1){
                 with(airquality, plot(Solar.R, Ozone))
                 fit1 <- lm(Ozone ~ Solar.R, airquality)
                 abline(fit1, lwd = 2, col = "red")
             }
             if (input$check2){
                 with(airquality, plot(Wind, Ozone))
                 fit2 <- lm(Ozone ~ Wind, airquality)
                 abline(fit2, lwd = 2, col = "red")
             }
             if (input$check3){
                 with(airquality, plot(Temp, Ozone))
                 fit3 <- lm(Ozone ~ Temp, airquality)
                 abline(fit3, lwd = 2, col = "red")
             }
             if (input$check4){
                 with(airquality, plot(Month, Ozone))
                 fit4 <- lm(Ozone ~ Month, airquality)
                 abline(fit4, lwd = 2, col = "red")
             }
        }
    })
    
    output$var <- renderText({
        paste("<b>Predictor value input:", "<br>",
              if (input$check1) {paste("Solar.R", input$solar)}, "<br>",
              if (input$check2) {paste("Wind", input$wind)}, "<br>",
              if (input$check3) {paste("Temp", input$temp)}, "<br>",
              if (input$check4) {paste("Month", input$month)}, "<br>",
              "</b>"
              )
    })
    
    output$pred <- renderText({
        paste("<b>The predicted Ozone value based on your chosen predictors and input values is:",
              "<br>",
              format(model(), digits = 5),
              "</b>")
    })


})
