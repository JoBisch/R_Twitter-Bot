##**************************************
## @JoBisch                           ##
## last update: march 2021            ##
##                                    ##
## getting Bitcoin hashrate           ##
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
  "dplyr"
  ,
  "tidyr"
  ,
  "Quandl"
  ,
  "ggplot2"
  ,
  "zoo"
  ,
  "lubridate"
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
# attention all chunks and scripts must be in the same path as the scripts
#wd <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(wd)

# sets working directory to RScript location
if (Sys.getenv("RSTUDIO") == "1") {
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  wd <- '/home/R_Twitter-Bot/03_quandl-data'
}

setwd(wd)

# variable today _______________________
today <- Sys.Date()
today.3y <- today %m+% years(-3)
today.5y <- today %m+% years(-5)

##**************************************
## Quandl API                       ----
##**************************************

# get API-Key from config ______________
quandl <- config::get("quandl")
apikey <- quandl$apikey

# get data from quandl _________________
data <- Quandl(
  "BCHAIN/HRATE"
  ,
  api_key = apikey
  ,
  start_date = today.3y
  ,
  end_date = today
)

data$Value <- data$Value / 1000000


##**************************************
## Data preparation                 ----
##**************************************

# Make zoo object of data ______________
temp.zoo <- zoo(data$Value)

# Calculate moving average with window and make first and last value as NA (to ensure identical length of vectors)
ma <- rollmean(temp.zoo, 100, fill = list(NA, NULL, NA))

# Add calculated moving averages to existing data frame
data$ma = coredata(ma)

##**************************************
## Plotting                         ----
##**************************************

Cairo::Cairo(
  24,
  #length
  16,
  #width
  file = paste("quandl_hashrate", ".png", sep = ""),
  type = "png",
  #tiff
  bg = "white",
  #white or transparent depending on your requirement
  dpi = 300,
  units = "cm" #you can change to pixels etc
)

# plot _________________________________
plot <- ggplot(data = data, aes(x = Date, y = Value)) +
  geom_line(color = "black",
            size = 0.4,
            alpha = 0.72) +
  
  geom_line(aes(Date, ma),
            color = "#f7931b",
            size = 1.5,
            alpha = 0.85) +
  
  geom_point(
    data = data[1:1,]
    ,
    aes(x = Date, y = Value)
    ,
    colour = "orange"
    ,
    size = 4.5
    ,
    stroke = 2.1
    ,
    shape = 1
    ,
    alpha = 0.85
  ) +
  
  labs(
    title = "#Bitcoin Hash Rate (EH/s)",
    subtitle = "100-day Simple Moving Average",
    x = NA,
    y = 'Hash Rate (EH/s)',
    caption = "@data_bitcoin | Source: Quandl (https://www.quandl.com/data/BCHAIN/HRATE-Bitcoin-Hash-Rate)"
  ) +
  
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  #scale_x_date(expand = c(0, 0)) +
  
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_text(color = "#f7931b"),
    #plot.title.position = "plot",
    plot.subtitle = element_text(color = "#3b3b3b"),
    plot.caption = element_text(color = "#646464", face = 'bold'),
    panel.border = element_rect(
      colour = "grey",
      fill = NA,
      size = 1
    )
  )


plot

# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(
  logo,
  x = 0.07,
  y = 0.03,
  just = c('left', 'bottom'),
  width = unit(0.5, 'inches')
)
dev.off()

# save plot ____________________________
#ggsave("quandl_hashrate.png", plot = last_plot())

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

# twitter text length max 140
text <- ""

if (data[1, 2] == max(data$Value)) {
  text <- paste0(
    "Today With ",
    round(data[1, 2], 1),
    " EH/s The Bitcoin Hashrate Reached A New All Time High. #BTC #Bitcoin"
  )
} else {
  text <- paste0(
    "Today With ",
    round(data[1, 2], 1),
    " EH/s The Bitcoin Hashrate Is ",
    round(max(data$Value) * 100 / data[1, 2] - 100, 1),
    " % Away From All Time High. #BTC #Bitcoin"
  )
}

# post tweet ___________________________
post_tweet(status = paste0(text, " | https://t.me/data_bitcoin"),
           media = "quandl_hashrate.png")

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
  photo = "quandl_hashrate.png",
  caption = paste0(text, "\n\n\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"),
  parse_mode = "HTML"
)
