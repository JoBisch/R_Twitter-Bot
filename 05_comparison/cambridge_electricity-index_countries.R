##**************************************
## JoBisch                            ##
## last update: august 2021           ##
##                                    ##
## fetches bitcoin electricity index  ##
##                                    ##
##**************************************

# Cambridge Bitcoin Electricity Consumption Index: https://cbeci.org 

## clear the cache _____________________

rm(list = ls())

##**************************************
## Install & load new packages      ----
##**************************************

# packages _____________________________
packages <- c("twitteR"
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

##**************************************
## Sets working directory           ----
##**************************************

# sets working directory to RScript location
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/05_comparison'
}

setwd(wd)

##**************************************
## Global Variables                 ----
##**************************************

# variable today _______________________

today <- Sys.Date()

today.5y <- today %m+% years(-5)

# gold mining TWh 2006 _________________
gold = 131

##**************************************
## Cambridge Electricity Index      ----
##**************************************

# get data  ____________________________
data <- read.csv("https://cbeci.org/api/v1.1.0/download/data?price=0.06", stringsAsFactors=FALSE)

# clean columns ________________________
names(data) <- c("timestamp", "datetime", "max", "min", "guess")
data <- data[-1,]

# assign data types ____________________
data$date <-
  as.POSIXct(as.numeric(as.character(data$timestamp)), origin = "1970-01-01", tz = "GMT")

data$max <- as.numeric(data$max)
data$min <- as.numeric(data$min)
data$guess <- as.numeric(data$guess)


##**************************************
## Plot                             ----
##**************************************

Cairo::Cairo(
  24, #length
  16, #width
  file = paste("cambridge_electricity-index", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

# bitcoin google trends plot 5 years ___
p <- ggplot(data = data, aes(x = date, y = guess)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey") +
  geom_line(color = "black", size = 0.5) +
  geom_hline(yintercept=gold, color = "#3b3b3b") +
  annotate("text"
           , x = data$date[100] 
           , y=gold+10
           , size = 3.5
           , label = "Gold mining (2006)"
           , color = "#3b3b3b") +
  geom_point(data=tail(data, n=1)
             ,aes(x=date, y=guess)
             ,colour="black"
             ,size=1.5) +
  labs(
    title = "Cambridge Bitcoin Electricity Consumption Index",
    subtitle = "Comparison To Gold Mining From 2006",
    x = NA,
    y = 'TWh annualised',
    caption = "@data99076083 | Source: Cambridge Centre for Alternative Finance (https://www.cbeci.org)"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_text(color = "#f7931b"),
    plot.subtitle = element_text(color = "#3b3b3b"),
    plot.caption = element_text(color = "#646464", face = 'bold'),
    panel.border = element_rect(
      colour = "grey",
      fill = NA,
      size = 1
    )
  )

p

# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(0.5, 'inches'))
dev.off()

# save plot ____________________________
#ggsave("gtrends_bitcoin_5y.png", plot = last_plot(), dpi = 300, width = NA, height = NA)


##**************************************
## Twitter Api                      ----
##**************************************

twitter <- config::get("twitter")

# twitter login credentuials ___________
consumerKey <- twitter$consumerKey
consumerSecret <- twitter$consumerSecret
accessToken <- twitter$accessToken
accessTokenSecret <- twitter$accessTokenSecret

# connect to twitter ___________________
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

# post tweet ___________________________
tweet(text = paste0("Cambridge Bitcoin Electricity Consumption Index (Cambridge Centre for Alternative Finance) ", as.character(today), " #Bitcoin #BTC"), mediaPath = ("cambridge_electricity-index.png"))


