########################################
## JoBisch                            ##
## last update: march 2021            ##
##                                    ##
## fear and greed index               ##
##                                    ##
########################################

## clear the cache _____________________
rm(list = ls())

########################################
## Install & load new packages        ##
########################################

# packages _____________________________
packages <- c("rtweet", "dplyr", "tidyr", "ggplot2", "httr", "jsonlite", "lubridate", "grid", "RColorBrewer")

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
  wd <- '/home/R_Twitter-Bot/FearnGreedIndex'
}

setwd(wd)

# variable today _______________________
today <- Sys.Date()
today.3y <- today %m+% years(-3)
today.5y <- today %m+% years(-5)

########################################
## alternative API                    ##
########################################

#https://alternative.me/crypto/api/

path <- "https://api.alternative.me/fng/?limit=0&format=json"

request <- GET(path)

########################################
## Data preparation                   ##
########################################

# flatten API request as text file
response  <- content(request, as = "text", encoding = "UTF-8")

# extract json information
data <- fromJSON(response, flatten = TRUE)$data %>% 
  data.frame()

# transform unix timecode to readable date
data$date <- as.POSIXct(as.numeric(as.character(data$timestamp)), origin="1970-01-01", tz="GMT")

# value from string to number
data$value <- as.integer(data$value)

########################################
## Plotting                           ##
########################################

# own color palette
redgreen <- rev(c("#FF8080", "#FFBF80", "#FFFF80", "#BFFF80", "#80FF80"))

# background color ____________
g <- rasterGrob(redgreen, width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 
#grid.draw(g) 

# plot _________________________________
plot <- ggplot(data=data, aes(x=date, y=value)) + 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_line(color = "black", size=0.7) +
  geom_point(data=data[1:1, ], aes(x=date, y=value), colour="black", size=2) +
  ylab('Fear & Greed Index by alternative') +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(caption="red: fear & green: greed | Source: alternative (https://alternative.me/crypto/fear-and-greed-index)") +
  theme(legend.position = "none", axis.title.x=element_blank())

plot

# save plot ____________________________
ggsave("FearnGreed.png", plot = last_plot())

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
twitter.text <- paste0("#Bitcoin Fear & Greed Index ", as.character(today), " #BTC")

# post tweet ___________________________
post_tweet(
  status = twitter.text,
  media = ("FearnGreed.png"),
  
)
