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
              ,"ggtext"
              ,"ggrepel"
              ,"ggalt"
              ,"lubridate"
              ,"config"
              ,"rstudioapi"
              ,"hrbrthemes"
              ,"magick"
              ,"Cairo"
              ,"emojifont"
              ,"reshape"
              ,"cowplot")

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
## Cambridge Hashrate               ----
##**************************************

# get and save data  ___________________
data <- read.csv("https://cbeci.org/api/v1.1.0/download/mining_countries", stringsAsFactors=FALSE)
write.csv(data,"data\\cambridge_hashrate_countries.csv", row.names = TRUE)

# clean column names ___________________
names(data) <- tolower(names(data))

# assign data types ____________________
data$date <-
  as.POSIXct(data$date)

# select relevant data _________________
maxdate <- max(data$date)
data <- subset(data, date==max(data$date) | date==max(data$date) %m-% months(12))

# transform data _______________________
data <- cast(data, country ~ date)
data <- data[order(data[,3], decreasing = TRUE),]
colnames(data)[2] <- "lastyear"
colnames(data)[3] <- "thisyear"

# join emoji location __________________
data <- left_join(x=data, y=emoji.mapping, by = "country")

# add id _______________________________
data$id <- rev(seq.int(nrow(data)))

##**************************************
## Preparing images                 ----
##**************************************

labels <- c()

for (i in 1:length(data$emoji)){
  
  img.name <- data$ALPHA.3[i]
  labels <- c(labels, paste0(img.name, " <img src='", data$emoji[i],  "' width='15' />"))
  
  #labels <- c(labels, paste0("<img src='", data$emoji[i],  "' width='15' /><br>", img.name))
  
}

##**************************************
## Plot                             ----
##**************************************

Cairo::Cairo(
  1200, #length
  900, #width
  file = paste("cambridge_hashrate_countries_dumbbell", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "px" #you can change to pixels etc 
)

p <- ggplot(data,
            aes(
              x =  lastyear,
              xend = thisyear,
              y = reorder(country, thisyear),
              yend = country
            ),
            color = "#3b3b3b") +

  geom_dumbbell(
    colour = "grey80",
    colour_xend = "#f7931b",
    size = 4.0,
    dot_guide =  TRUE,
    dot_guide_size = 0.15,
    dot_guide_colour = "#3b3b3b"
  ) +


  scale_x_continuous(expand = expansion(mult = c(0, .05))) +

  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  
  labs(
    title = "Share Of Global Bictoin Hashrate",
    subtitle = paste0("Monthly Average ", as.character(format(maxdate, "%B %Y"))),
    y = "",
    x = '%-share of total hashrate',
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
p <- p +   scale_y_discrete(name = NULL,
                       labels = rev(labels)) +
  theme(axis.text.y = element_markdown(color = "black", size = 11))

p

ggsave("test.png", p, dpi = 300)

ggsave(
  filename = "test.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("cm"),
  dpi = 110,
  limitsize = TRUE
)

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
tweet(text = paste0("Share Of Global Bictoin Hashrate By Country (Cambridge Centre for Alternative Finance) ", as.character(maxdate), " #Bitcoin #BTC"), mediaPath = ("cambridge_hashrate_countries_lollipop.png"))


