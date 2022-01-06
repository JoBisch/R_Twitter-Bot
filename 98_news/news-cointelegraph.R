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
packages <- c("telegram.bot"
              , "httr"
              , "qdapRegex")

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
  wd <- '/home/R_Twitter-Bot/98_news'
}

setwd(wd)

##**************************************
## Global Variables                 ----
##**************************************

# variable today _______________________

today <- Sys.Date()

##**************************************
## RSS Feed cointelegraph           ----
##**************************************

path <- "https://cointelegraph.com/editors_pick_rss"

request <- GET(path)

content <-
  content(request,
          as = "text",
          type = "xml",
          encoding = "UTF-8")

date <-
  substr(qdapRegex::ex_between(content, "<lastBuildDate>", "</lastBuildDate>"),
         1,
         16)

description <-
  qdapRegex::ex_between(content, "<title><![CDATA[", "]]></title")[[1]]

link <- qdapRegex::ex_between(content, "<link>", "</link")[[1]][3:7]

links <- ""

for (x in 1:5) {
  print(x)
  print(links)
  links <- paste0(links,
                  x,
                  ". <a href=\"",
                  link[x],
                  "\">",
                  description[x],
                  "</a>\n")
}

text <- paste0(
  "&#x1F5DE Top Cointelegraph News - ",
  date,
  ":\n\n",
  links,
  "\n",
  "\U0001F916 <a href=\"https://twitter.com/data_bitcoin\">@data_bitcoin</a>"
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
  photo = "../pics/cointelegraph.png",
  caption = text,
  parse_mode = "HTML"
)
