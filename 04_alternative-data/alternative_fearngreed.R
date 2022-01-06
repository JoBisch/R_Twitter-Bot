##**************************************
## JoBisch                            ##
## last update: march 2021            ##
##                                    ##
## fear and greed index               ##
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
  "ggplot2"
  ,
  "httr"
  ,
  "jsonlite"
  ,
  "lubridate"
  ,
  "grid"
  ,
  "RColorBrewer"
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
  wd <- '/home/R_Twitter-Bot/04_alternative-data'
}

setwd(wd)

# variable today _______________________
today <- Sys.Date()
today.3y <- today %m+% years(-3)
today.5y <- today %m+% years(-5)

##**************************************
## alternative API                  ----
##**************************************

#https://alternative.me/crypto/api/

path <- "https://api.alternative.me/fng/?limit=0&format=json"

request <- GET(path)

##**************************************
## Data preparation                 ----
##**************************************

# flatten API request as text file
response  <- content(request, as = "text", encoding = "UTF-8")

# extract json information
data <- fromJSON(response, flatten = TRUE)$data %>%
  data.frame()

# transform unix timecode to readable date
data$date <-
  as.POSIXct(as.numeric(as.character(data$timestamp)), origin = "1970-01-01", tz = "GMT")

# value from string to number
data$value <- as.integer(data$value)

##**************************************
## Text                             ----
##**************************************

text <- ""

if (data[1, 1] <= 25) {
  text <- paste0(
    "Today Extreme Fear In The Market With A ",
    data[1, 1],
    " Bitcoin Fear & Greed Index. #BTC #Bitcoin"
  )
} else if (data[1, 1] < 45) {
  text <- paste0("Today Fear In The Market With A ",
                 data[1, 1],
                 " Bitcoin Fear & Greed Index. #BTC #Bitcoin")
  
} else if (data[1, 1] <= 55) {
  text <- paste0(
    "Today Neutral Sentiment In The Market With A ",
    data[1, 1],
    " Bitcoin Fear & Greed Index. #BTC #Bitcoin"
  )
  
} else if (data[1, 1] <= 75) {
  text <- paste0("Today Greed In The Market With A ",
                 data[1, 1],
                 " Bitcoin Fear & Greed Index. #BTC #Bitcoin")
  
} else {
  text <- paste0(
    "Today Extreme Greed In The Market With A ",
    data[1, 1],
    " Bitcoin Fear & Greed Index. #BTC #Bitcoin"
  )
}

##**************************************
## Plotting                         ----
##**************************************

# own color palette
redgreen <-
  rev(c("#FF8080", "#FFBF80", "#FFFF80", "#BFFF80", "#80FF80"))

# background color ____________
g <-
  rasterGrob(
    redgreen,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
#grid.draw(g)


# cairo plot specification
Cairo::Cairo(
  28,
  #length
  18,
  #width
  file = paste("alternative_fearngreed", ".png", sep = ""),
  type = "png",
  #tiff
  bg = "white",
  #white or transparent depending on your requirement
  dpi = 300,
  units = "cm" #you can change to pixels etc
)


# plot _________________________________
plot <- ggplot(data = data, aes(x = date, y = value)) +
  annotation_custom(
    g,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +
  
  geom_line(color = "black", size = 0.5) +
  
  geom_point(
    data = data[1:1,]
    ,
    aes(x = date, y = value)
    ,
    colour = "black"
    ,
    size = 4.5
    ,
    stroke = 2.1
    ,
    shape = 1
  ) +
  
  
  labs(
    title = "#Bitcoin Fear & Greed Index",
    subtitle = text,
    x = NA,
    y = 'Fear & Greed Index by alternative',
    caption = "@data_bitcoin | red: fear; green: greed | Source: alternative (https://alternative.me/crypto/fear-and-greed-index)"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_text(color = "#f7931b"),
    plot.subtitle = element_text(color = "#3b3b3b"),
    plot.caption = element_text(color = "#646464", face = 'bold')
  )

plot

# add logo to plot and save as png: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo <- image_read("../pics/logo_twitter-account.jpg")

grid::grid.raster(
  logo,
  x = 0.07,
  y = 0.03,
  just = c('left', 'bottom'),
  width = unit(0.45, 'inches')
)

dev.off()

# save plot ____________________________
#ggsave("FearnGreed.png", plot = last_plot())

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

# post tweet ___________________________
post_tweet(status = paste0(text, " | https://t.me/data_bitcoin"),
           media = "alternative_fearngreed.png")

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
  photo = "alternative_fearngreed.png",
  caption = paste0(text, "\n\n\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"),
  parse_mode = "HTML"
)
