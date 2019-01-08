library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict the Next Word by Qinan Hu"),
    sidebarLayout(
        sidebarPanel(
               img(src = "typing.gif", height = 180, width = 180),
               br(),
               htmlOutput("loading")
               ),
        mainPanel(
               h4("This is my app for Coursera's Data Science Specialization Capstone Project. Based on 4269678 pieces of documents from English blogs, news and Twitter, it uses either a Stupid Backoff algorithm or Kneser-Ney smoothing algorithm to predict the next word based on user's input."),
               tabsetPanel(
                   tabPanel("SBO",
                            br(),
                            textAreaInput("user_input1", "Input", width = "600px", height = "200px", placeholder = "Start typing ..."),
                            br(),
                            strong("Your input:"),
                            htmlOutput("user_text1"),
                            br(),
                            actionButton("SBOcan1", textOutput("SBO_can1")),
                            actionButton("SBOcan2", textOutput("SBO_can2")),
                            actionButton("SBOcan3", textOutput("SBO_can3")),
                            actionButton("SBOcan4", textOutput("SBO_can4")),
                            actionButton("SBOcan5", textOutput("SBO_can5")),
                            br(),
                            strong("SBO prediction:"),
                            tableOutput("SBO_output"),
                            br(),
                            p("Stupid Backoff (SBO) algorithm predicts the next word based on the longest ngrams with an observed frequency before it, without considering the probability of shorter ngrams. In my implementation, the algorithm will first fetch the last three words (trigram), and predict the next word based on how frequent the combined four words (quadragram). If the number of results is less than 5, it will back off to last two words (bigram), predict the next words based on the frequency of combined trigrams, and so on. In the worst case scenario where even a bigram frequency is not available, it will predict the 5 most common words in the training set. In addition, the algorithm will also recognize the start of a sentence and make predictions based on most frequent words at the start of a sentence, instead of words in the previous sentence.")
                            ), 
                   tabPanel("KN (slow)",
                            br(),
                            textAreaInput("user_input2", "Input", width = "600px", height = "200px", placeholder = "Start typing ..."),
                            br(),
                            strong("Your input:"),
                            htmlOutput("user_text2"),
                            br(),
                            actionButton("KNcan1", textOutput("KN_can1")),
                            actionButton("KNcan2", textOutput("KN_can2")),
                            actionButton("KNcan3", textOutput("KN_can3")),
                            actionButton("KNcan4", textOutput("KN_can4")),
                            actionButton("KNcan5", textOutput("KN_can5")),
                            br(),
                            strong("KN prediction:"),
                            tableOutput("KN_output"),
                            br(),
                            strong("KN smoothing will take significantly longer time to calculate than SBO."),
                            p("Smoothing involves 'discounting' the frequency of ngrams to consider the frequency of (n-1)grams. Kneser-Ney smoothing algorithm uses a iterative method to take shorter ngrams into account. In my implementation, the algorithm will first consider the frequency of the last trigram, then back off to bigram and unigram input. It will then take all the frequency into account and make a prediction based on discounted probability. Other implementations are similar to SBO.")
                            )
                   )
               )
        )
    )
)
