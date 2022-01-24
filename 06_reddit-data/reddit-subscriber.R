## clear the cache _____________________

rm(list = ls())

## remove scientific notation e+ _______
options(scipen = 999)
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")

##**************************************
## Install & load new packages      ----
##**************************************

# packages _____________________________
packages <- c(
  "telegram.bot",
  "rtweet",
  "dplyr",
  "tidyr",
  "plyr",
  "ggplot2",
  "lubridate",
  "httr",
  "jsonlite",
  "Cairo",
  "hrbrthemes",
  "magick",
  "rstudioapi",
  "scales"
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
  wd <- "/home/R_Twitter-Bot/06_reddit-data"
}

setwd(wd)

# variable today _______________________
today <- Sys.Date()
today_api <- format(Sys.Date(), format = "%d-%m-%Y")

variables <- config::get("variables")

##**************************************
## Data preparation                 ----
##**************************************

data <- data.frame()

# import csv files
filenames <-
  list.files("../../R_Twitter-Bot_cdc/04_crypto-com-social/data", pattern = "*reddit-subscribers*", full.names = TRUE)

for (f in 1:length(filenames)) {
  csv <- read.csv2(
    filenames[f],
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  
  print(head(csv))
  
  data <- rbind(data, csv[, c("subreddit", "subscribers", "datetime")])
  
}

# date manipulation

data["date"] <- as.Date(data$datetime)

data <- data[data["subreddit"] == "bitcoin", ]

# text
data.text <- data[data$date == max(data$date), ]

weekly.rise.num <- data.text[data.text$subreddit == "bitcoin", 2]-data[data$date == max(data$date)-7 & data$subreddit == "bitcoin", 2]

weekly.rise <- format(as.numeric(weekly.rise.num), nsmall=0, big.mark=",")

weekly.abs <- format(as.numeric(data.text[data.text$subreddit == "bitcoin", 2]), nsmall=0, big.mark=",")

if (weekly.rise.num < 0) {
  
  weekly.rise.text <- paste0("r/Bitcoin Has ",weekly.abs," Subscriber And Lost ", weekly.rise," Subscriber Over The Past Week.")
  
} else if (weekly.rise.num > 0) {
  
  weekly.rise.text <- paste0("r/Bitcoin Has ",weekly.abs," Subscriber And Gained ", weekly.rise," New Subscriber Over The Past Week.")
  
} else {
  
  weekly.rise.text <- paste0("r/Bitcoin Has ",weekly.abs," Subscriber And Not Gained Or Lost Any Subscriber Over The Past Week.")
  
}

##**************************************
## Plotting                         ----
##**************************************

# cairo plot specification
Cairo::Cairo(
  28,
  #length
  18,
  #width
  file = paste("reddit-subscriber", ".png", sep = ""),
  type = "png",
  #tiff
  bg = "white",
  #white or transparent depending on your requirement
  dpi = 300,
  units = "cm" #you can change to pixels etc
)


# plot _________________________________
plot <- ggplot(data = data, aes(x = date, y = subscribers)) +
  
  geom_line(color = "grey50", size = 0.7) +
  
  labs(
    title = "r/Bitcoin Subscriber Over Time",
    subtitle = weekly.rise.text,
    x = NA,
    y = 'Subscriber',
    caption = paste0(
      variables$author,
      " | Source: Reddit (https://subredditstats.com/r/bitcoin)"
    )
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3),
                     expand = c(0, 0)) +
  
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

watermark <- image_read("../pics/icons/icons8-reddit-250_alpha.png")

grid::grid.raster(
  watermark,
  x = 0.5,
  y = 0.5,
  just = c('centre', 'centre'),
  width = unit(2, 'inches')
)

dev.off()

# save plot ____________________________
#ggsave("FearnGreed.png", plot = last_plot())

##**************************************
## Twitter Api                      ----
##**************************************

twitter <- config::get("twitter")

# twitter login credentuials ___________
appname <- "cdc_stats"
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
nchar(paste0(weekly.rise.text, " #BTC #Bitcoin"))

# post tweet ___________________________
post_tweet(status = paste0(weekly.rise.text, " #BTC #Bitcoin"),
           media = "reddit-subscriber.png",
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
  photo = "reddit-subscriber.png",
  caption = paste0(
    weekly.rise.text,
    "\n\n\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"
  ),
  parse_mode = "HTML"
)
