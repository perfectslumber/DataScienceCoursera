library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
library(readr)
library(data.table)

SOS_word_candidate <- NULL
word_freq <- NULL
two_gram_freq <- NULL
three_gram_freq <- NULL
four_gram_freq <- NULL

readData <- function(session, word_freq, two_gram_freq, three_gram_freq, four_gram_freq) {
    progress <- Progress$new(session)
    progress$set(value = 0, message = "Loading start of sentence words ... 1/5")
    SOS_word_candidate <<- read_csv("final_freqs/SOS_word_candidate.csv")
    progress$set(value = 0.05, message = "Loading unigram frequency ... 2/5")
    word_freq <<- read_csv("final_freqs/word_freq.csv")
    progress$set(value = 0.1, message = "Loading bigram frequency ... 3/5")
    two_gram_freq <<- read_csv("final_freqs/two_gram_freq.csv")
    progress$set(value = 0.3, message = "Loading trigram frequency ... 4/5")
    three_gram_freq <<- read_csv("final_freqs/three_gram_freq.csv")
    progress$set(value = 0.6, message = "Loading quadragram frequency... 5/5")
    four_gram_freq <<- read_csv("final_freqs/four_gram_freq.csv")
    progress$set(value = 1, message = 'Loading...')
    progress$close()
}

shinyServer(function(input, output, session) {
    
    output$loading <- renderText({
        if(!is.null(four_gram_freq)) "<b> All required data loaded. </b>"
    })
    
    if(is.null(four_gram_freq)){
        readData(session, word_freq, two_gram_freq, three_gram_freq, four_gram_freq)
    }
    
    output$user_text1 <- renderText({
        paste("<b>", input$user_input1, "</b>")
    })
    output$user_text2 <- renderText({
        paste("<b>", input$user_input2, "</b>")
    })
    
    SBO_pred <- reactive({
        req(input$user_input1)
        SBO_word_prediction(input$user_input1,four_gram_freq, three_gram_freq, two_gram_freq, word_freq)
    })
    
    output$SBO_can1 <- renderText({SBO_pred()$Prediction[1]})
    observeEvent(input$SBOcan1, {
        updated_text <- paste0(input$user_input1, " ", SBO_pred()$Prediction[1])
        updateTextAreaInput(session, "user_input1", value = updated_text)
    })
    output$SBO_can2 <- renderText({SBO_pred()$Prediction[2]})
    observeEvent(input$SBOcan2, {
        updated_text <- paste0(input$user_input1, " ", SBO_pred()$Prediction[2])
        updateTextAreaInput(session, "user_input1", value = updated_text)
    })
    output$SBO_can3 <- renderText({SBO_pred()$Prediction[3]})
    observeEvent(input$SBOcan3, {
        updated_text <- paste0(input$user_input1, " ", SBO_pred()$Prediction[3])
        updateTextAreaInput(session, "user_input1", value = updated_text)
    })
    output$SBO_can4 <- renderText({SBO_pred()$Prediction[4]})
    observeEvent(input$SBOcan4, {
        updated_text <- paste0(input$user_input1, " ", SBO_pred()$Prediction[4])
        updateTextAreaInput(session, "user_input1", value = updated_text)
    })
    output$SBO_can5 <- renderText({SBO_pred()$Prediction[5]})
    observeEvent(input$SBOcan5, {
        updated_text <- paste0(input$user_input1, " ", SBO_pred()$Prediction[5])
        updateTextAreaInput(session, "user_input1", value = updated_text)
    })
    
    KN_pred <- reactive({
        req(input$user_input2)
        KN_word_prediction(input$user_input2, four_gram_freq, three_gram_freq, two_gram_freq, word_freq)
    })
    
    output$KN_can1 <- renderText({KN_pred()$Prediction[1]})
    observeEvent(input$KNcan1, {
        updated_text <- paste0(input$user_input2, " ", KN_pred()$Prediction[1])
        updateTextAreaInput(session, "user_input2", value = updated_text)
    })
    output$KN_can2 <- renderText({KN_pred()$Prediction[2]})
    observeEvent(input$KNcan2, {
        updated_text <- paste0(input$user_input2, " ", KN_pred()$Prediction[2])
        updateTextAreaInput(session, "user_input2", value = updated_text)
    })
    output$KN_can3 <- renderText({KN_pred()$Prediction[3]})
    observeEvent(input$KNcan3, {
        updated_text <- paste0(input$user_input2, " ", KN_pred()$Prediction[3])
        updateTextAreaInput(session, "user_input2", value = updated_text)
    })
    output$KN_can4 <- renderText({KN_pred()$Prediction[4]})
    observeEvent(input$KNcan4, {
        updated_text <- paste0(input$user_input2, " ", KN_pred()$Prediction[4])
        updateTextAreaInput(session, "user_input2", value = updated_text)
    })
    output$KN_can5 <- renderText({KN_pred()$Prediction[5]})
    observeEvent(input$KNcan5, {
        updated_text <- paste0(input$user_input2, " ", KN_pred()$Prediction[5])
        updateTextAreaInput(session, "user_input2", value = updated_text)
    })
    
    output$SBO_output <- renderTable({SBO_pred()})
    output$KN_output<- renderTable({KN_pred()})
    
})

