##**************************************
## JoBisch                            ##
## last update: march 2021            ##
##                                    ##
## fetches google trends data         ##
##                                    ##
##**************************************

## clear the cache _____________________

rm(list = ls())

##**************************************
## Install & load new packages      ----
##**************************************

# packages _____________________________
packages <- c(
  "telegram.bot"
  ,
  "rtweet"
  ,
  "gtrendsR"
  ,
  "lubridate"
  ,
  "config"
  ,
  "rstudioapi"
  ,
  "tidyverse"
  ,
  "viridis"
  ,
  "spData"
  ,
  "tmap"
  ,
  "hrbrthemes"
  ,
  "magick"
  ,
  "Cairo"
)

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage ________________________________
ipak(packages)

##**************************************
## Sets working directory           ----
##**************************************

# sets working directory to RScript location
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/02_google-data'
}

setwd(wd)

##**************************************
## Global Variables                 ----
##**************************************

# variable today _______________________

today <- Sys.Date()

today.5y <- today %m+% years(-5)

##**************************************
## Google Trends API                ----
##**************************************

# gtrends variables ____________________
# define the keywords __________________
keywords = c("Bitcoin")

# set the geographic area: DE = Germany
country = c("")

# set the time window __________________
time = (paste(today.5y, today, sep = " "))

# set channels _________________________
channel = c("web", "news", "images", "froogle", "youtube")

# get google trends data _______________
gtrends.data <-
  gtrends(
    keyword = keywords,
    geo = country,
    time = time,
    gprop = channel,
    category = 0,
    hl = "en-US",
    low_search_volume = FALSE,
    cookie_url = "http://trends.google.com/Cookies/NID",
    tz = 0,
    onlyInterest = FALSE
  )

# select only interst over time ________
gtrends.data.country <- gtrends.data$interest_by_country

#
countries <- spData::world %>%
  left_join(y = gtrends.data.country,
            by = c("name_long" = "location"),
            keep = T)

##**************************************
## Plot                             ----
##**************************************

# package tmap
map <- tm_shape(countries) +
  tm_fill(
    "hits",
    title = "Search Interest",
    legend.reverse = T,
    id = "name_long",
    popup.vars = c(
      Name = "name_long",
      Search.Interest = "hits",
      Population = "pop",
      Life.Expectancy = "lifeExp",
      GDP.per.capita = "gdpPercap"
    )
  ) +
  tm_polygons() +
  tm_credits(text = "@data_bitcoin | Source: Google Trends (https://www.google.com/trends)")

map


# save plot ____________________________
tmap_save(map, "google_trends_world_map.png")


##**************************************
## Twitter Api                      ----
##**************************************

twitter <- config::get("twitter")

# twitter login credentuials ___________
appname <- "gtrendsc"
consumerKey <- twitter$consumerKey
consumerSecret <- twitter$consumerSecret
accessToken <- twitter$accessToken
accessTokenSecret <- twitter$accessTokenSecret

# authenticate via access token ________
create_token(
  app = appname,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessTokenSecret
)

##**************************************
## Post Tweet                       ----
##**************************************

text <-
  paste0(
    paste(head(countries[order(-countries$hits),]$location, 3), collapse = ", "),
    " - Countries With The Highest Worldwide Google #Bitcoin Interest."
  )


nchar(paste0(text, " | https://t.me/data_bitcoin"))

# post tweet ___________________________
post_tweet(status = paste0(text, " | https://t.me/data_bitcoin"),
           media = "google_trends_world_map.png")

##**************************************
## Telegram API                     ----
##**************************************

telegram <- config::get("telegram")

# create bot
bot <- Bot(token = telegram$token)

# check bot connection
print(bot$getMe())


# send text
bot$sendPhoto(
  chat_id = telegram$channel_id,
  photo = "google_trends_world_map.png",
  caption = paste0(text, "\n\n\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"),
  parse_mode = "HTML"
)
