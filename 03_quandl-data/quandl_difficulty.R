########################################
## JoBisch                            ##
## last update: march 2021            ##
##                                    ##
## getting Bitcoin difficulty         ##
##                                    ##
########################################

## clear the cache _____________________
rm(list = ls())

########################################
## Install & load new packages        ##
########################################

# packages _____________________________
packages <- c("rtweet", "dplyr", "tidyr", "Quandl", "ggplot2", "zoo", "lubridate")

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
today.3y <- today %m+% years(-3)
today.5y <- today %m+% years(-5)

########################################
## Quandl API                         ##
########################################

# get API-Key from config ______________
quandl <- config::get("quandl")
apikey <- quandl$apikey

# get data from quandl _________________
data <- Quandl("BCHAIN/DIFF"
               , api_key=apikey
               ,start_date=today.3y
               , end_date=today)

data$Value <- data$Value/1000000000000


########################################
## Data preparation                   ##
########################################

# Make zoo object of data ______________
temp.zoo<-zoo(data$Value)

# Calculate moving average with window and make first and last value as NA (to ensure identical length of vectors)
ma<-rollmean(temp.zoo, 100, fill = list(NA, NULL, NA))

# Add calculated moving averages to existing data frame
data$ma=coredata(ma)

########################################
## Plotting                           ##
########################################

# plot _________________________________
plot <- ggplot(data=data, aes(x=Date, y=Value)) + 
  geom_line(color = "black", size=1) +
  geom_line(aes(Date,ma),color="orange", size=2) +
  ylab('Mining Difficulty (Trillions)') +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(caption="Source: Quandl (https://www.quandl.com/data/BCHAIN/DIFF-Bitcoin-Difficulty)") +
  theme(legend.position = "none", axis.title.x=element_blank())


plot

# save plot ____________________________
ggsave("BTC_difficulty.png", plot = last_plot())

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

########################################
## Post Tweet                         ##
########################################

# twitter text length max 140
twitter.text <- paste0("#Bitcoin Mining Difficulty (Trillions) ", as.character(today), " #BTC")

# post tweet ___________________________
post_tweet(
  status = twitter.text,
  media = ("BTC_difficulty.png"),
  
)






