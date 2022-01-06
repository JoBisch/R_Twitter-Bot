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
packages <- c(
  "telegram.bot"
  ,
  "rtweet"
  ,
  "dplyr"
  ,
  "tidyverse"
  ,
  "ggplot2"
  ,
  "ggimage"
  ,
  "ggtext"
  ,
  "ggrepel"
  ,
  "lubridate"
  ,
  "config"
  ,
  "rstudioapi"
  ,
  "hrbrthemes"
  ,
  "magick"
  ,
  "Cairo"
  ,
  "emojifont"
  ,
  "reshape"
  ,
  "cowplot"
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
## Cambridge Hashrate               ----
##**************************************

# get and save data  ___________________
data <-
  read.csv("https://ccaf.io/cbeci/api/v1.1.1/download/mining_countries",
           stringsAsFactors = FALSE)
write.csv(data, "data\\cambridge_hashrate_countries.csv", row.names = TRUE)

# clean column names ___________________
names(data) <- tolower(names(data))

# assign data types ____________________
data$date <-
  as.POSIXct(data$date)

# select relevant data _________________
maxdate <- max(data$date)
data <-
  subset(data,
         date == max(data$date) | date == max(data$date) %m-% months(12))

# transform data _______________________
data <- cast(data, country ~ date)
data <- data[order(data[, 3], decreasing = TRUE), ]
colnames(data)[2] <- "lastyear"
colnames(data)[3] <- "thisyear"

# join emoji location __________________
data <- left_join(x = data, y = emoji.mapping, by = "country")

# add id _______________________________
data$id <- rev(seq.int(nrow(data)))

# text preparation
country <-
  str_trim(str_replace_all(data$country[1], "[^[:alnum:]]", " "))
share <- round(data$thisyear[1], 1)

# text
text <- paste0(
  country,
  " Has With ",
  share,
  " % The Highest Share Of Bitcoin Hashrate In ",
  as.character(format(maxdate, "%B %Y")),
  "."
)

##**************************************
## Preparing images                 ----
##**************************************

labels <- c()

for (i in 1:length(data$emoji)) {
  img.name <- data$ALPHA.3[i]
  
  labels <-
    c(labels,
      paste0("<img src='", data$emoji[i],  "' width='15' /><br>", img.name))
  
}

##**************************************
## Plot                             ----
##**************************************

Cairo::Cairo(
  1200,
  #length
  900,
  #width
  file = paste("cambridge_hashrate_countries_lollipop", ".png", sep = ""),
  type = "png",
  #tiff
  bg = "white",
  #white or transparent depending on your requirement
  dpi = 140,
  units = "px" #you can change to pixels etc
)

p <- ggplot(data, aes(x = country, y = thisyear)) +
  geom_segment(aes(
    x = reorder(country, thisyear) ,
    xend = country,
    y = lastyear,
    yend = thisyear
  ),
  color = "#3b3b3b") +
  geom_point(size = 3, color = "#f7931b") +
  geom_point(aes(x = country, y = lastyear), color = "#BCBCBC", size = 3) +
  geom_point(aes(x = country, y = thisyear), color = "#f7931b", size = 3) +
  
  annotate(
    "text",
    label = "this year",
    x = nrow(data) - 0.7,
    y = data[2, 3] + 3,
    size = 4,
    color = "#e6840e",
    fontface = "bold"
  ) +
  geom_curve(
    aes(
      x = nrow(data) - 0.85,
      y = data[2, 3] + 3,
      xend = nrow(data) - 1,
      yend = data[2, 3] + 0.7
    ),
    colour = "#e6840e",
    size = 0.3,
    curvature = -0.2,
    arrow = arrow(length = unit(0.015, "npc"))
  ) +
  
  annotate(
    "text",
    label = "last year",
    x = nrow(data) - 1.5,
    y = data[2, 2] + 3.2,
    size = 4,
    color = "#A8A8A8",
    fontface = "bold"
  ) +
  
  geom_curve(
    aes(
      x = nrow(data) - 1.35,
      y = data[2, 2] + 4,
      xend = nrow(data) - 1.1,
      yend = data[2, 2] + 0.6
    ),
    colour = "#A8A8A8",
    size = 0.3,
    curvature = -0.15,
    arrow = arrow(length = unit(0.015, "npc"))
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  
  labs(
    title = paste0("Share Of Global Bictoin Hashrate - Monthly Average"),
    subtitle = text,
    x = "",
    y = '%',
    caption = "@data_bitcoin | Source: Cambridge Centre for Alternative Finance (https://www.cbeci.org/mining_map)"
  ) +
  theme_ipsum() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(color = "#f7931b"),
    plot.subtitle = element_text(color = "#3b3b3b"),
    plot.caption = element_text(color = "#646464", face = 'bold'),
    panel.border = element_rect(
      colour = "grey",
      fill = NA,
      size = 1
    )
  )

# add logos to y axis: https://stackoverflow.com/questions/68747698/ggplot2-images-as-y-axis-labels-solved/68751201#68751201
p <- p +   scale_x_discrete(name = NULL,
                            labels = rev(labels)) +
  theme(axis.text.y = element_markdown(color = "black", size = 11))

p

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
#ggsave("gtrends_bitcoin_5y.png", plot = last_plot(), dpi = 300, width = NA, height = NA)


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
nchar(paste0(text, " #Bitcoin #BTC | https://t.me/data_bitcoin"))

# post tweet ___________________________
post_tweet(
  status = paste0(text, " #Bitcoin #BTC | https://t.me/data_bitcoin"),
  media = ("cambridge_hashrate_countries_lollipop.png"),
  
)


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
  photo = "cambridge_hashrate_countries_lollipop.png",
  caption = paste0(text, "\n\n\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"),
  parse_mode = "HTML"
)

