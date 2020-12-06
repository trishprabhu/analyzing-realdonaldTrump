
# Load the relevant libraries:

library(MASS)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)
library(ggrepel)

# Create a stan_glm model:

fit_obj <- stan_glm(meanofmeans ~ approval_ratings,
              data = finalgraphtib, 
              family = gaussian(),
              refresh = 0)

print(fit_obj, view = FALSE, digits = 5)

# Create a table of the regression results; using as_gt() allows us to convert
# into a gt table that we can format:

fit_obj %>%
  tbl_regression() %>%
  as_gt() %>%
  tab_header(title = "Regression of Trump's Twitter Sentiment Scores", 
  
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my table. Apologies!      
             
             subtitle = "The Effect of Approval Ratings on Trump's Twitter Sentiment Score") %>% 
  tab_source_note("Source: Trump Twitter Archive") 

# What sentiment score would we expect on 3 different days, with Donald Trump's
# approval rating at 30%, 45%, and 60%, respectively? We can use
# posterior_predict to find out:

new <- tibble(approval_ratings = c(30, 45, 60))

set.seed(27)
pp <- posterior_predict(fit_obj, newdata = new) %>%
  as_tibble() %>%
  mutate_all(as.numeric)

head(pp, 10)

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
       subtitle = "We have a much more precise estimate for a hypothetical Trump 
       with a 45% approval rating, given the data",
       x = "Sentiment Score",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = "Approval Rating",
                    values = c("dodgerblue", "salmon", "green")) +
  theme_bw()

approvalratingdistribution

# Read in stock data (another variable that could potentially influence Trump's
# daily Twitter score/can serve as a control):

stock_data <- read_csv("data/current_stock_data.csv")

# (Substantially) clean (yikes!) and subset the data to the relevant date range:

colnames(stock_data) <- c("Date", 
                "open", 
                "high",
                "low",
                "close")

stock_data <- stock_data[-1, ]

# When first looking at the data, I noticed something odd; it seemed as if
# several days were missing! I then realized: the markets are closed on the
# weekends (Saturday/Sunday), so we must account for these "missing days" (which
# in essence, carry over the values from the Friday) when cleaning the data.

updated_stock_data <- stock_data %>%
  mutate(id = 1:4245) %>%
  filter(id >= 4203 & id <= 4225) %>%
  mutate(newdates = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(open = as.numeric(open)) %>%
  mutate(high = as.numeric(high)) %>%
  mutate(low = as.numeric(low)) %>%
  mutate(close = as.numeric(close)) %>%
  mutate(range = close - open) %>%
  select(newdates, open, high, low, close, range) %>%
  add_row(newdates = as.Date("09/12/2020", "%m/%d/%Y"), 
          open = 28.6,
          high = 29.7,
          low = 26.5,
          close = 26.9,
          range = -1.76,
          .before = 2) %>%
  add_row(newdates = as.Date("09/13/2020", "%m/%d/%Y"), 
          open = 28.6,
          high = 29.7,
          low = 26.5,
          close = 26.9,
          range = -1.76,
          .before = 3) %>%
  add_row(newdates = as.Date("09/19/2020", "%m/%d/%Y"), 
          open = 26.6,
          high = 28.1,
          low = 25.3,
          close = 25.8,
          range = -0.82,
          .before = 9) %>%
  add_row(newdates = as.Date("09/20/2020", "%m/%d/%Y"), 
          open = 26.6,
          high = 28.1,
          low = 25.3,
          close = 25.8,
          range = -0.82,
          .before = 10) %>%
  add_row(newdates = as.Date("09/26/2020", "%m/%d/%Y"), 
          open = 28.17,
          high = 30.43,
          low = 26.02,
          close = 26.38,
          range = -1.79,
          .before = 16) %>%
  add_row(newdates = as.Date("09/27/2020", "%m/%d/%Y"), 
          open = 28.17,
          high = 30.43,
          low = 26.02,
          close = 26.38,
          range = -1.79,
          .before = 17) %>%
  add_row(newdates = as.Date("10/03/2020", "%m/%d/%Y"), 
          open = 28.87,
          high = 29.90,
          low = 26.93,
          close = 27.63,
          range = -1.24,
          .before = 23) %>%
  add_row(newdates = as.Date("10/04/2020", "%m/%d/%Y"), 
          open = 28.87,
          high = 29.90,
          low = 26.93,
          close = 27.63,
          range = -1.24,
          .before = 24) %>%
  add_row(newdates = as.Date("10/10/2020", "%m/%d/%Y"), 
          open = 26.20,
          high = 26.22,
          low = 24.03,
          close = 25.00,
          range = -1.20,
          .before = 30) %>%
  add_row(newdates = as.Date("10/11/2020", "%m/%d/%Y"), 
          open = 26.20,
          high = 26.22,
          low = 24.03,
          close = 25.00,
          range = -1.20,
          .before = 31)


final_stock_data <- updated_stock_data[-1, ]

# By joining finalgraphtib and final_stock_data, we now have a tibble with
# daily approval ratings, Twitter sentiment scores, and stock differences.

finalstocktib <- inner_join(finalgraphtib, final_stock_data, by = "newdates")

# Create a "robust" model. This is a form of weighted least squares (mm 
# estimator; not as efficient, given the size of data, but it's going to handle 
# any outliers):

stock_obj_rlm <- rlm(meanofmeans ~ range, 
                 data = finalstocktib,
                 method = "MM")

summary(stock_obj_rlm)

# Create a stan_glm model to examine stock volatility/Trump's (the results
# suggest a similar coefficient to the one above):

stock_obj <- stan_glm(meanofmeans ~ range,
                    data = finalstocktib, 
                    refresh = 0)

print(stock_obj, view = FALSE, digits = 5)

# Create a visualization of the relationship between range (stock volatility)
# Trump's sentiment scores (over the same period):

stockgraph <- finalstocktib %>%
  ggplot(aes(x = range, y = meanofmeans)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my graph. Apologies! 
  
  labs(title = "Stock opening/closing differences and Trump's daily sentiment scores on Twitter, 09/12 - 10/13",
       subtitle = "The S&P 500's opening/closing differences and Trump's 
       sentiment scores seem to be very, very weakly negatively correlated",
       x = "Difference",
       y = "Sentiment Score",
       caption = "Source: Trump Twitter Archive; CBOE Volatility Index") +
  theme_bw()

stockgraph

# Let's now return to our initial model, and bring in stock volatility as a 
# control variable (we do this by adding the variable to the equation):

fit_obj <- stan_glm(meanofmeans ~ approval_ratings + range,
                    data = finalstocktib, 
                    refresh = 0)

print(fit_obj, view = FALSE, digits = 5)


# Create a visualization of the relationship between readability and sentiment
# in Tweets:

tweetgraph <- tweetib1 %>%
  ggplot(aes(x = Flesch, y = sentimentmeans, color = str_length(text))) +
  geom_point() +
  geom_label_repel(aes(label = ifelse(str_length(text) < 35, as.character(text),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  
# I know that the lines below surpasses the 80 character limit, but cutting them
# off was not aesthetically appealing on my graph. Apologies! 
  
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

# Let's create a model looking at regressing sentiment in Tweets on readability:

Flesch_obj <- stan_glm(sentimentmeans ~ Flesch,
                    data = tweetib1, 
                    refresh = 0)

print(Flesch_obj, view = FALSE, digits = 5)

# Let's create a model looking at regressing sentiment in Tweets on character
# count:

character_obj <- stan_glm(sentimentmeans ~ str_length(text),
                       data = tweetib1, 
                       refresh = 0)

print(character_obj, view = FALSE, digits = 5)

# Create a visualization of the relationship between character count and 
# sentiment in Tweets:

charactergraph <- tweetib1 %>%
  ggplot(aes(x = str_length(text), y = sentimentmeans)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my graph. Apologies! 
  
  labs(title = "Character Count and Sentiment of Trump's Tweets (09/12/20 - 10/13/20)",
       subtitle = "",
       x = "Character Count",
       y = "Sentiment Score",
       caption = "Source: Trump Twitter Archive") +
  theme_bw()

charactergraph

# Create a visualization of the relationship between character count and 
# readability in Tweets:

charactergraph2 <- tweetib1 %>%
  ggplot(aes(x = Flesch, y = str_length(text))) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my graph. Apologies! 
  
  labs(title = "Readability and Character Count of Trump's Tweets (09/12/20 - 10/13/20)",
       subtitle = "",
       x = "Readability",
       y = "Character Count",
       caption = "Source: Trump Twitter Archive") +
  theme_bw()

charactergraph2

# Create a visualization of character count for both Donald Trump and Hillary
# Clinton:

characterhist <- tweetib1 %>%
  ggplot(aes(x = str_length(text))) +
  geom_histogram(binwidth = 20,
                 color = "white") +
  labs(title = "Character Count of Trump's Tweets (09/12/20 - 10/13/20)",
       subtitle = "",
       x = "Character Count",
       y = "Frequency",
       caption = "Source: Trump Twitter Archive") +
  theme_classic()

characterhist 

# Write RDS. Only need to do once!

# write_rds(finalstocktib, "finalstock.rds")
# write_rds(pp, "pp.rds")
