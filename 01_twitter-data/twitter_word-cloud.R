########################################
## JoBisch                            ##
## last update: may 2020              ##
##                                    ##
## fetches twitter data and           ##
## displaying word cloud              ##
########################################

## clear the cache _____________________
rm(list = ls())

########################################
## Resources                          ##
########################################

# twitter sentiment analysis: https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967
# word cloud: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

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
              ,"ggwordcloud"
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
  wd <- '/home/R_Twitter-Bot/01_twitter-data'
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

# unnecessary words for #Bitcoin
bag.btc <- read.csv2('custom/yoshi_unnecessary-words.csv')
bag.btc <- as.vector(bag.btc$words)
bag.btc <- enframe(bag.btc, name = "name", value = "word")

# remove stop words from your list of words
tweets.bitcoin.clean <- tweets.bitcoin.stem %>%
  anti_join(stop_words) %>%
  anti_join(bag.btc)

# top 25 words
words.25 <- tweets.bitcoin.clean %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n))

# save data as csv
words.100 <- tweets.bitcoin.clean %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  mutate(word = reorder(word, n))
words.100$date <- today
write.csv(words.100, paste0('data/', today, '_twitter_words100.csv'))
rm(words.100)

########################################
## Data Understanding                 ##
########################################

# top 25 words
#ggplot(words.25,aes(x = word, y = n)) +
#         geom_col() +
#         xlab(NULL) +
#         coord_flip() +
#         labs(x = "Unique Words",
#              y = "Count"
#              # = "Unique words counts found in recent 15.000 #Bitcoin tweets",
#              #caption = as.character(today)
#              ) +
#         theme_bw()
#
#ggsave("twitter_bitcoin_uw.png", plot = last_plot())

Cairo::Cairo(
  20, #length
  15, #width
  file = paste("twitter_bitcoin_word-cloud", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

# top 25 words word cloud
ggplot(words.25, aes(label = word, size = n, color = n)) +
        geom_text_wordcloud_area(shape = 'diamond') +
        scale_size_area(max_size = 24) +
        scale_color_gradient(low = "lightgrey", high = "black") +

        labs(title = "Top Unique Word Counts",
             subtitle = "Found In Recent 15.000 #Bitcoin Tweets",
             y = NULL,
             x = NULL,
             caption = paste0('@data99076083 | Count: Min ', min(words.25$n),'; Max ', max(words.25$n))
        ) +

          theme_ipsum() +
          theme(legend.position = "none", 
                plot.title = element_text(color = "#f7931b"),
                plot.subtitle = element_text(color = "#3b3b3b"),
                plot.caption = element_text(color = "#646464", face = 'bold')) #f7931b


# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(0.5, 'inches'))
dev.off()


#ggsave("twitter_bitcoin_uw_word-cloud.png", plot = last_plot())


########################################
## Post Tweet                         ##
########################################

# twitter text length max 140
twitter.text <- paste0("Top Unique Word Counts Found In Recent 15.000 #Bitcoin Tweets ", as.character(today), " #BTC")

# post tweet ___________________________
post_tweet(
  status = twitter.text,
  media = ("twitter_bitcoin_word-cloud.png"),

)

