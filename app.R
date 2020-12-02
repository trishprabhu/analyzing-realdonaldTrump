# Download relevant libraries, including the sentimentr library, so I can
# complete sentiment analysis!

library(shiny)
library(readr)
library(sentimentr)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(colourpicker)
library(wordcloud2)
library(tm)
library(gt)
library(magrittr)
library(dplyr)
library(ggthemes)
library(quanteda)
library(MASS)
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(ggrepel)
library(shinythemes)

# Reading in the data.

trumptweets <- read_csv("data/Trump_tweets (1).csv")
summary(trumptweets)

hillarytweets <- read_csv("data/hillarytweets.csv")
summary(hillarytweets)

# Rather than calculate sentiment scores for all of the Tweets (thousands of
# observations, which would substantially slow things down, I took a subset
# of observations).

trump_sentiment_scores <- sentiment(trumptweets$text[1:100])
hillary_sentiment_scores <- sentiment(hillarytweets$text[1:100])

dataframe_options <- c("Hillary Clinton", "Donald Trump")

# Define UI for application that draws a histogram
ui <- navbarPage("Analyzing @realDonaldTrump: A Deep Dive Into Donald Trump's 
                 Tweets",
    tabPanel("Tweet Analysis",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Sentiment Analysis: A Glimpse At The Data"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(inputId = "dataset",
                                           label = "Choose a Twitter account:",
                                           choices = c("Hillary Clinton", 
                                                       "Donald Trump")),
                               numericInput(inputId = "obs",
                                            label = "Number of observations to 
                                            view:",
                                            value = 10),
                               
# Originally, I just had a numericInput() box; at Dan's suggestion, I added a
# slider, so folks who visit my Shiny App can more easily look at the desired
# number of observations.
                               
                               sliderInput("obs", 
                                           "Slide to the number of observations 
                                           to view:",
                                           min = 0, max = 300, value = 30
                               )),
                           mainPanel(
                               verbatimTextOutput("summary"),
                               tableOutput("view"),
                           )),
                       br(),
                       br(),
                       br(),
                       br(),
                       sidebarPanel(
                           numericInput("tweetread", 
                                        "Pick the Tweet you'd like to view:",
                                        value = 5
                           )),
                       mainPanel(
                           gt_output(outputId = "tweetread")
                       ),

# The sidebars were great spots to both 1) provide some context around the
# graphics, and 2) align/style the page so that the graphs were aesthetically 
# appealing.
                       
                       sidebarPanel(
                           p("Here, I visualize the distributions
                        of Trump and Clinton's Tweets' sentiment scores. 
                        On average, they are both relatively neutral on Twitter, 
                        but it's clear: Trump's Tweets see much more variation
                        in sentiment; by comparison, Clinton rarely reaches the 
                        most extreme sentiment scores (1 and -1)."),
                           selectInput(inputId = "candidate", 
                                       label = "Choose a Twitter account:",
                                       choices = dataframe_options)),
                       mainPanel(
                           br(),
                           br(),
                           br(),
                           br(),
                           plotOutput(outputId = "bothPlot"),
                           sliderInput("bins", 
                                       "Set the number of bins:",
                                       min = 0, max = 50, value = 20
                           )))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           
# As I learned, the values below correspond to the choices argument above -- 
# important to ensure that everything stays consistent, or your code will break
# (as mine did, until I figured this out)!
           
           "Hillary Clinton" = hillary_sentiment_scores,
           "Donald Trump" = trump_sentiment_scores)
  })
  
  candidateInput <- reactive({
    switch(input$candidate,
           "Hillary Clinton" = hillary_sentiment_scores,
           "Donald Trump" = trump_sentiment_scores)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    tib <- dataset %>%
      rename("Tweets" = "element_id",
             "Sentence Number" = "sentence_id",
             "Word Count" = "word_count",
             "Sentiment" = "sentiment") 
    summary(tib)
  })
  
  output$view <- renderTable({
    dataset <- datasetInput()
    nicetib <- dataset %>%
      rename("Tweets" = "element_id",
             "Sentence Number" = "sentence_id",
             "Word Count" = "word_count",
             "Sentiment" = "sentiment") 
    head(nicetib, n = input$obs)
  })
  
  output$bothPlot <- renderPlot({
    candidate <- candidateInput()
    candidate %>%
      ggplot(aes(x = sentiment)) +
      geom_histogram(bins = input$bins,
                     color = "white",
                     fill = "dodgerblue") +
      labs(x = "Sentiment Score",
           y = "Count",
           subtitle = "Overall, Hillary is very neutral in her Tweets; Trump is too, but with more variation",
           title = "Sentiment Expressed In Tweets",
           caption = "Source: Trump Twitter Archive") +
      
# I thought that explicitly graphing the mean of both Trump and Clinton's
# sentiment scores could help viewers better visualize the distribution overall
# (I also thought it was interesting that, on average, they are both very
# neutral -- likely a result of Trump's more positive Tweets "canceling out"
# his more negative Tweets).
      
      geom_vline(xintercept = mean(candidate$sentiment),
                 linetype = "dashed") +
      theme_classic()
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
