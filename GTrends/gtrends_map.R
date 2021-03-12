########################################
## JoBisch                            ##
## last update: march 2021            ##
##                                    ##
## fetches google trends data         ##
##                                    ##
########################################

## clear the cache _____________________

rm(list = ls())

########################################
## Install & load new packages        ##
########################################

# packages _____________________________
packages <- c("gtrendsR"
              ,"lubridate"
              ,"config"
              ,"rstudioapi"
              ,"tidyverse"
              ,"viridis"
              ,"hrbrthemes"
              ,"mapdata"
              ,"hexbin"
              ,"spData"
              ,"tmap")

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
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/GTrends'
}

setwd(wd)

########################################
## Global Variables                   ##
########################################

# variable today _______________________

today <- Sys.Date()

today.5y <- today %m+% years(-5)

########################################
## Google Trends API                  ##
########################################

# gtrends variables ____________________
# define the keywords __________________
keywords=c("Bitcoin")

# set the geographic area: DE = Germany 
country=c("")

# set the time window __________________
time=(paste(today.5y, today, sep = " "))

# set channels _________________________
channel=c("web", "news", "images", "froogle", "youtube")

# get google trends data _______________
gtrends.data <- gtrends(keyword = keywords, geo = country, time = time,
                        gprop = channel,
                        category = 0, hl = "en-US", low_search_volume = FALSE,
                        cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
                        onlyInterest = FALSE)

# select only interst over time ________
gtrends.data.country <- gtrends.data$interest_by_country

# 
countries <- spData::world %>%
  left_join(y=gtrends.data.country,by = c("name_long" = "location"),keep=T)

########################################
## Plot                               ##
########################################

# package tmap
map <- tm_shape(countries) +
  tm_fill("hits",
          title = "Search Interest",
          legend.reverse = T,
          id = "name_long", 
          popup.vars=c(Name = "name_long",Search.Interest = "hits",Population = "pop",Life.Expectancy = "lifeExp", GDP.per.capita = "gdpPercap")) +
  tm_polygons()


# save plot ____________________________
tmap_save(map, "gtrends_bitcoin_world_map.png")


########################################
## Twitter Api                        ##
########################################

twitter <- config::get("twitter")

# twitter login credentuials ___________
consumerKey <- twitter$consumerKey
consumerSecret <- twitter$consumerSecret
accessToken <- twitter$accessToken
accessTokenSecret <- twitter$accessTokenSecret

# connect to twitter ___________________
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

# post tweet ___________________________
tweet(text = paste0("Worldwide Bitcoin Interest (Google Trends) ", as.character(today), " #Bitcoin #BTC"), mediaPath = ("gtrends_bitcoin_world_map.png"))


