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
packages <- c("gtrendsR", "ggplot2", "lubridate", "config", "rstudioapi")

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
gtrends.data.interest <- gtrends.data$interest_over_time

########################################
## Plot                               ##
########################################

# bitcoin google trends plot 5 years ___
plot.5y <- ggplot(data=gtrends.data.interest, aes(x=date, y=hits)) +
  geom_line(color = "black", size=1) +
  ylab('Relative Interest') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_bw() +
  labs(caption="Source: Google Trends (https://www.google.com/trends)") +
  theme(legend.position = "none", axis.title.x=element_blank())

plot.5y

# save plot ____________________________
ggsave("gtrends_bitcoin_5y.png", plot = last_plot())


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
tweet(text = paste0("Worldwide Bitcoin Interest Over Time (Google Trends) ", as.character(today), " #Bitcoin #BTC"), mediaPath = ("gtrends_bitcoin_5y.png"))


