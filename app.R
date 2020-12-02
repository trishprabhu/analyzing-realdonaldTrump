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
                           )))),
tabPanel("Models",
         titlePanel("How/why does Trump's sentiment on Twitter change?"),
         sidebarPanel(
           titlePanel("Approval Rating"),
           p("Here, I look at Donald Trump's daily approval
                     ratings and Twitter sentiment scores (the average sentiment
                     of his Tweets on a given day) over a 1 month period -- 
                     09/12/20 - 10/13/20. As we'd expect, Trump's approval 
                     ratings and sentiment scores seem to be weakly positively
                     correlated (as his approval rating increases, he also
                     becomes more positive on Twitter -- perhaps as he becomes
                     more popular, it puts him in a sunny mood). One must be 
                     cautious in drawing any conclusions, though -- not only is 
                     the relationship relatively weak, this is also a relatively 
                     short period of time; a longer period (like 1 year) -- with 
                     more datapoints -- would likely be more telling."),
           br(),
           br(),
           br(),
           br(),
           br(),
           p("In this graph, we visualize the posterior distributions for
                 Trump's daily Twitter sentiment score in 3 hypothetical 
                 universes: one in which he has a 30% approval rating, one
                 in which he has a 45% approval rating, and one in which he has
                 a 60% approval rating. The distributions reflect the linear
                 relationship we observed above -- the hypothetical Trump with a
                 60% approval rating has a posterior distribution for sentiment
                 scores that is skewed to the right (more positive). It's also 
                 clear that we have a much more precise estimate for the 
                 hypothetical Trump with a 45% approval rating, given the data; 
                 while, on average, the 30% and 60% approval rating scenarios 
                 are less and more positive, respectively, the distributions are 
                 rather wide, so we wouldn't be surprised if the Trump with a 
                 30% approval rating had a positive daily Twitter sentiment 
                 score."),
           br(),
           br(),
           br(),
           br(),
           titlePanel("Stock Market"),
           p("Here, I look at daily stock market opening/closing differences
               and Donald Trump's corresponding Twitter sentiment scores over a 
               1 month period (09/12 - 10/13). Interestingly, the S&P 500's 
               opening/closing differences and Trump's sentiment scores seem to 
               be very weakly negatively correlated -- indeed the regression results 
               (which you can view below, in the interactive table!) produce a 
               coefficient for difference which is very small/negative. Overall, then, 
               it seems that the stock market doesn't greatly influence 
               Donald Trump's sentiment on Twitter, and any influence is such 
               that as the difference becomes more positive (a higher closing 
               index relative to the opening index) Trump becomes a bit more 
               negative on Twitter (perhaps he feels vindicated?).
               While the relationship does seem to be very weak, we can still
               use this dependent variable as a control in our regression of
               Trump's sentiment scores on his approval ratings -- as we do 
              below."),
           br(),
           br(),
           titlePanel("Interactive Regression Results"),
           p("See an interpretation of these results in the Discussion
               tab."),
           selectInput(inputId = "regressiontable",
                       label = "Choose a variable:",
                       choices = c("Approval Rating", 
                                   "Stock Market",
                                   "Interaction")),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           titlePanel("Readability"),
           p("Here, I look at the relationship between the readability of
               Donald Trump's Tweets and the sentiment of those Tweets.
               Interestingly, readability seems to have close to no relationship 
               with sentiment; regression results confirm this. The
               visualization does pull out another trend, however; by only
               displaying the text for those Tweets below a certain length
               of characters, it seems that Trump's shorter tweets (generally)
               tend to be more positive. Clearly, he doesn't like to brag!")),
         mainPanel(
           plotOutput(outputId = "approvalSentiment"),
           plotOutput(outputId = "approvalPosterior"),
           plotOutput(outputId = "stockSentiment"),
           br(),
           br(),
           gt_output(outputId = "regressiontable"),
           br(),
           br(),
           br(),
           br(),
           plotOutput(outputId = "readability"))
))

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
  
  output$approvalSentiment <- renderPlot({
    
    finalgraphtib %>%
      ggplot(aes(x = (approval_ratings/100), y = meanofmeans)) +
      geom_point() +
      geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
      
      # I know that the lines below surpasses the 80 character limit, but cutting them
      # off was not aesthetically appealing on my graph. Apologies!
      
      labs(title = "Trump's daily approval ratings and sentiment scores on Twitter, 09/12 - 10/13",
           subtitle = "Trump's approval ratings and sentiment scores seem to be weakly positively correlated",
           x = "Approval Rating",
           y = "Sentiment Score",
           caption = "Source: Trump Twitter Archive") +
      scale_x_continuous(labels = scales::percent_format()) +
      theme_bw()
    
  })
  
  output$approvalPosterior <- renderPlot({
    
    approvalratingdistribution <- pp %>%
      rename(`30` = `1`) %>%
      rename(`45` = `2`) %>%
      rename(`60` = `3`) %>%
      pivot_longer(cols = `30`:`60`,
                   names_to = "parameter",
                   values_to = "score") %>%
      ggplot(aes(x = score, fill = parameter)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.7,
                     bins = 100,
                     color = "white",
                     position = "identity") +
      labs(title = "Posterior Distributions for Sentiment Score",
           subtitle = "We have a much more precise estimate for the hypothetical Trump with a 45% approval rating, given the data",
           x = "Sentiment Score",
           y = "Proportion",
           caption = "Source: Trump Twitter Archive, FiveThirtyEight") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(name = "Approval Rating (%)",
                        values = c("dodgerblue", "salmon", "green")) +
      theme_bw()
    
    approvalratingdistribution
    
  })
  
  output$stockSentiment <- renderPlot({
    
    stockgraph <- finalstocktib %>%
      ggplot(aes(x = range, y = meanofmeans)) +
      geom_point() +
      geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
      
      # I know that the lines below surpasses the 80 character limit, but cutting them
      # off was not aesthetically appealing on my graph. Apologies!
      
      labs(title = "Stock opening/closing differences and Trump's daily sentiment scores on Twitter, 09/12 - 10/13",
           subtitle = "The S&P 500's opening/closing differences and Trump's sentiment scores seem to be very, very weakly negatively correlated",
           x = "Difference",
           y = "Sentiment Score",
           caption = "Source: Trump Twitter Archive; CBOE Volatility Index") +
      theme_bw()
    
    stockgraph
    
  })
  
  regressiontableInput <- reactive({
    switch(input$regressiontable,
           
"Approval Rating" = formula(finalstocktib$meanofmeans ~ finalstocktib$approval_ratings),
"Stock Market" = formula(finalstocktib$meanofmeans ~ finalstocktib$range),
"Interaction" = formula(finalstocktib$meanofmeans ~ finalstocktib$approval_ratings * finalstocktib$range))
  
  })
  
  output$regressiontable <- render_gt({
    
    formula <- regressiontableInput()
    
    set.seed(10)
    fit_obj <- stan_glm(formula,
                        data = finalstocktib, 
                        family = gaussian(),
                        refresh = 0)
    
    fit_obj %>%
      tidy() %>%
      mutate(confidencelow = estimate - (std.error * 2)) %>%
      mutate(confidencehigh = estimate + (std.error * 2)) %>%
      gt() %>%
      cols_label(term = "Predictor",
                 estimate = "Beta",
                 std.error = "Standard Error",
                 confidencelow = "CI Low",
                 confidencehigh = "CI High") %>%
      tab_header(title = "Regression of Trump's Twitter Sentiment Scores") %>% 
      tab_source_note("Source: Trump Twitter Archive") 
    
  }) 
  
  
  output$tweetread <- render_gt({
    
    tweetib1 %>%
      filter(element_id == input$tweetread) %>%
      ungroup() %>%
      select(text, sentimentmeans, Flesch) %>%
      rename("Tweet" = "text",
             "Sentiment" = "sentimentmeans",
             "Readability" = "Flesch") %>%
      gt() %>%
      tab_header(title = "Sentiment and Readability of Trump's Tweets", 
                 subtitle = "Readability: 0 - 100, 100 is most readable; Sentiment: -1 to 1, 1 is most positive") %>% 
      tab_source_note("Source: Trump Twitter Archive") %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen")
        ),
        locations = cells_body(
          rows = Sentiment > 0)
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "red")
        ),
        locations = cells_body(
          rows = Sentiment < 0)
      )
    
  })
  
  output$readability <- renderPlot({
    
    tweetgraph <- tweetib1 %>%
      ggplot(aes(x = Flesch, y = sentimentmeans, color = str_length(text))) +
      geom_point() +
      geom_label_repel(aes(label = ifelse(str_length(text) < 35, as.character(text),'')),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
      labs(title = "Readability and Sentiment of Trump's Tweets (09/12/20 - 10/13/20)",
           subtitle = "Readability has little relationship with Trump's sentiment on Twitter",
           x = "Readability (0 - 100; 0 is the least readable)",
           y = "Sentiment Score",
           caption = "Source: Trump Twitter Archive",
           color = "Character Count") +
      xlim(0, 100) +
      ylim(-1, 1) +
      theme_bw()
    
    tweetgraph
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
