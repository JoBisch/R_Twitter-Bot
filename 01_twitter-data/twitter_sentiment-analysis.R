########################################
## JoBisch                            ##
## last update: may 2020              ##
##                                    ##
## fetches twitter data and           ##
## analyses the sentiment             ##
########################################

## clear the cache _____________________
rm(list = ls())

########################################
## Resources                          ##
########################################

# twitter sentiment analysis: https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967

########################################
## Install & load new packages        ##
########################################

# packages _____________________________
packages <- c("rtweet"
              ,"dplyr"
              ,"tidyr"
              ,"tidytext"
              ,"ggplot2"
              ,"purrr"
              ,"tibble"
              ,"treemap"
              ,"hrbrthemes"
              ,"magick"
              ,"Cairo")

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage ________________________________
ipak(packages)

########################################
## Sets working directory             ##
########################################

# sets working directory to RScript location
# attention all chunks and scripts must be in the same path as the scripts
#wd <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(wd)

# sets working directory to RScript location
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/Twitter_Sentiment-Analysis'
}

setwd(wd)

# variable today _______________________
today <- Sys.Date()

########################################
## Twitter Api                        ##
########################################

twitter <- config::get("twitter")

# twitter login credentuials ___________
appname <- "gtrendsc"
consumerKey <- twitter$consumerKey
consumerSecret <- twitter$consumerSecret
accessToken <- twitter$accessToken
accessTokenSecret <- twitter$accessTokenSecret

# authenticate via access token ________
create_token(app = appname,
             consumer_key = consumerKey,
             consumer_secret = consumerSecret,
             access_token = accessToken,
             access_secret = accessTokenSecret)

# fetch all tweets data ________________
twitter.data.bitcoin <- search_tweets(
  "#Bitcoin", n = 15000, include_rts = FALSE, type = 'recent'
)

# only select tweets ___________________
tweets.bitcoin <- twitter.data.bitcoin %>% select(screen_name, text)

########################################
## Data Preparation                   ##
########################################

# remove http elements manually ________
tweets.bitcoin$stripped_text <- gsub("http\\S+","",tweets.bitcoin$text)

# use unnest_tokens() function to convert to lowercase
# remove punctuation and add id for each tweet
tweets.bitcoin.stem <- tweets.bitcoin %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# unnecessary words fpr #Bitcoin
bag.btc <- c("bitcoin","btc") #"crypto","cryptocurrency","blockchain","money"
bag.btc <- enframe(bag.btc, name = "name", value = "word")

# remove stop words from your list of words
tweets.bitcoin.clean <- tweets.bitcoin.stem %>%
  anti_join(stop_words) %>%
  anti_join(bag.btc)


########################################
## Sentiment Analysis                 ##
########################################

# append positive and negative words to bing
yoshi.sentiment.words <- read.csv2('custom/yoshi_sentiment-words.csv')
data.sentiment <- rbind(get_sentiments("bing"), yoshi.sentiment.words)

# bin sentiment analysis
bing.sentiment <- tweets.bitcoin.clean %>%
  inner_join(data.sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# save data as csv
bing.sentiment$date <- today
write.csv(bing.sentiment, paste0('data/', today, '_twitter_sentiment.csv'))
bing.sentiment$date <- NULL

########################################
## Plot Sentiment Analysis            ##
########################################

Cairo::Cairo(
  24, #length
  16, #width
  file = paste("twitter_bitcoin_sentiment", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

# plot bing sentiment analysis
bing.sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free_y") +
         labs(title = "Contribution To Sentiment",
              subtitle = "Recent 15.000 Tweets containing #Bitcoin",
              y = "Count",
              x = NULL,
              caption = "@data99076083"
              ) +
         coord_flip() + 
         scale_y_continuous(expand = c(0, 0), breaks = seq(0, nrow(bing.sentiment), 100)) +
         theme_ipsum() +
         theme(legend.position = "none", 
               plot.title = element_text(color = "#f7931b"),
               plot.subtitle = element_text(color = "#3b3b3b"),
               plot.caption = element_text(color = "#646464", face = 'bold')) #f7931b


# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(0.5, 'inches'))
dev.off()



#ggsave("twitter_bitcoin_sentiment.png", plot = last_plot())

# treemap
#bing.sentiment %>%
#  group_by(sentiment) %>%
#  top_n(10) %>%
#  ungroup() %>%
#  mutate(word = reorder(word, n)) %>%
#  treemap(
#        index="word",
#        vSize="n",
#        type= "categorical",
#        vColor = "sentiment",
#        palette = "Spectral"
#        )


# sentiment score

########################################
## Post Tweet                         ##
########################################

# twitter text length max 140
twitter.text <- paste0("Contribution To Sentiment Of Recent 15.000 Tweets containing #Bitcoin ", as.character(today), " #BTC")

# post tweet ___________________________
post_tweet(
  status = twitter.text,
  media = ("twitter_bitcoin_sentiment.png"),
  
)

