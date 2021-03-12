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
              ,"hexbin")

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

########################################
## Plot                               ##
########################################

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/17_ListGPSCoordinates.csv", sep=",", header=T)

# Get the world polygon
world <- map_data("world")

# plot
ggplot(data, aes(x=homelon, y=homelat)) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_hex(bins=100) +
  #ggplot2::annotate("text", x = 175, y = 80, label="Worldwide Bitcoin Interest (Google Trends)", colour = "black", size=4, alpha=1, hjust=1) +
  #ggplot2::annotate("segment", x = 100, xend = 175, y = 73, yend = 73, colour = "black", size=0.2, alpha=1) +
  theme_void() +
  ylim(-70, 80) +
  scale_fill_viridis(
    trans = "log", 
    breaks = seq(from = 0, to = 100, by = 10),
    name=" ", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.3, 0.01),
    legend.title=element_text(color="black", size=6),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) +
  labs(caption="Source: Google Trends (https://www.google.com/trends)")

#labs(caption="Source: Google Trends (https://www.google.com/trends)")

# save plot ____________________________
ggsave("gtrends_bitcoin_world", plot = last_plot())


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
tweet(text = paste0("Worldwide Bitcoin Interest (Google Trends) ", as.character(today), " #Bitcoin #BTC"), mediaPath = ("gtrends_bitcoin_world.png"))


