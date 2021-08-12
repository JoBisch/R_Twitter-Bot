##**************************************
## JoBisch                            ##
## last update: august 2021           ##
##                                    ##
## fetches hashrate by countries      ##
##                                    ##
##**************************************

# Cambridge Bitcoin Hashrate By Countries: https://cbeci.org/mining_map
# Twitter Emojis: https://twemoji.twitter.com | https://emojipedia.org/twitter/twemoji-13.1/
# Image In ggplot: https://www.statworx.com/de/blog/using-emojis-and-png-as-icons-in-your-ggplot/

## clear the cache _____________________

rm(list = ls())

##**************************************
## Install & load new packages      ----
##**************************************

# packages _____________________________
packages <- c("twitteR"
              ,"dplyr"
              ,"tidyverse"
              ,"ggplot2"
              ,"ggimage"
              ,"ggrepel"
              ,"lubridate"
              ,"config"
              ,"rstudioapi"
              ,"hrbrthemes"
              ,"magick"
              ,"Cairo"
              ,"emojifont")

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
  wd <- '/home/R_Twitter-Bot/99_blockchain-data'
}

setwd(wd)

##**************************************
## Global Variables                 ----
##**************************************

# variable today _______________________

today <- Sys.Date()

today.5y <- today %m+% years(-5)

# get emoji mapping ____________________
emoji.mapping <- read.csv2('custom/mapping_country-emoji.csv')

##**************************************
## Cambridge Electricity Index      ----
##**************************************

# get data  ____________________________
data <- read.csv("https://cbeci.org/api/v1.1.0/download/mining_countries", stringsAsFactors=FALSE)

# clean column names ___________________
names(data) <- tolower(names(data))

# join emoji location __________________
data <- left_join(x=data, y=emoji.mapping, by = "country")

# assign data types ____________________
data$date <-
  as.POSIXct(data$date)

write.csv(data,"data\\cambridge_hashrate_countries.csv", row.names = TRUE)

# subset for labels ____________________
data.labels <- subset(data, date==max(data$date))
data.labels <- data.labels[order(data.labels$country),]
data.labels <- data.labels %>% map_df(rev)
data.labels$cum <- cumsum(data.labels$share.of.global.hashrate)
data.labels <- data.labels %>% mutate(location = cum - (share.of.global.hashrate/2))
data.labels <- data.labels %>% mutate(country.legend = paste0(sprintf("%.1f", share.of.global.hashrate)," % ", country))

##**************************************
## Plot                             ----
##**************************************

Cairo::Cairo(
  20, #length
  14, #width
  file = paste("cambridge_hashrate_countries", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

# bitcoin google trends plot 5 years ___
p <- ggplot(data = data, aes(x = date, y = share.of.global.hashrate, fill = country)) +
  geom_area(alpha=0.5 , size=0.5, colour="black") +
  scale_fill_brewer(palette="Greys") +
  labs(
    title = "Share Of Global Bictoin Hashrate",
    subtitle = "Monthly Average",
    x = NA,
    y = '%',
    caption = "@data99076083 | Source: Cambridge Centre for Alternative Finance (https://www.cbeci.org/mining_map)"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_datetime(expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
  theme_ipsum() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
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

p + #geom_text_repel(data=data.labels, aes(x=date %m+% months(-2), y=cum, label=country), point.padding = 0.9, segment.alpha = 0) +
  geom_image(data=data.labels, aes(x=date %m+% days(-15), y=location, image = emoji), size = 0.04)
  

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
tweet(text = paste0("Share Of Global Bictoin Hashrate By Country (Cambridge Centre for Alternative Finance) ", as.character(max(data$date)), " #Bitcoin #BTC"), mediaPath = ("cambridge_hashrate_countries.png"))


