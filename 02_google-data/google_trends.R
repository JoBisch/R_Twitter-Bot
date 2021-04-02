########################################
## JoBisch                            ##
## last update: may 2020              ##
##                                    ##
## fetches google trends data         ##
##                                    ##
########################################

# Twitter Bot: https://medium.com/@randerson112358/create-a-twitter-bot-using-r-5a94f1b1b886
# Google Trends API: https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
# Twitter API: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
# Windows Task Scheduler: https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html
# Telegram Bot: https://www.giga.de/apps/telegram/tipps/telegram-bot-erstellen-loeschen-andere-befehle-so-geht-s/

## clear the cache _____________________

rm(list = ls())

########################################
## Install & load new packages        ##
########################################

# packages _____________________________
packages <- c("gtrendsR"
              ,"twitteR"
              ,"ggplot2"
              ,"lubridate"
              ,"config"
              ,"rstudioapi"
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
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/02_google-data'
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

Cairo::Cairo(
  16, #length
  12, #width
  file = paste("google_trends_5y", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

# bitcoin google trends plot 5 years ___
p <- ggplot(data=gtrends.data.interest, aes(x=date, y=hits)) +
  geom_line(color = "black", size=0.5) +
  labs(title = "Worldwide Bitcoin Interest Over Time",
       subtitle = "Google Trends",
       x = NA,
       y = 'Relative Interest',
       caption = "@data99076083 | Source: Google Trends (https://www.google.com/trends)") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_ipsum() +
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        plot.title = element_text(color = "#f7931b"),
        plot.subtitle = element_text(color = "#3b3b3b"),
        plot.caption = element_text(color = "#646464", face = 'bold')) #f7931b


  #p <- p + scale_color_manual(name = '',
  #                            labels = c('Black', 'Red', 'Gray'),
  #                            values = c('#000000', '#EC0108', '#ACAEAD'))

p

# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(0.5, 'inches'))
dev.off()

# save plot ____________________________
#ggsave("gtrends_bitcoin_5y.png", plot = last_plot(), dpi = 300, width = NA, height = NA)


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
tweet(text = paste0("Worldwide Bitcoin Interest Over Time (Google Trends) ", as.character(today), " #Bitcoin #BTC"), mediaPath = ("google_trends_5y.png"))


