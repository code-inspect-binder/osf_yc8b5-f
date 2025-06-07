## GET TWEETS FROM TWITTER API USING RTWEET PACKAGE
## Authors: Diego Iglesias & Miguel A. Sorrel


# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)

# Lang and date
lang <- "es" # texto en español
date <- "_20_03_22" 


## whatever name you assigned to your created app
appname <- "diego_iglesias"
## api key (example below is not a real key)
key <- "eefgnt45lmkgnsJ"
## api secret (example below is not a real key)
secret <- "1245tghfkmnien7"

## from the app website
access_token <- "62ssssS9EB7afrtjdngfstsa"
access_secret <- "Kekfnurmens2qz"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


## SEARCH AND STORE TWEETS
## search for 1000 tweets using the #feliz hashtag
feliz <- search_tweets(q = "#feliz", n = 1000, include_rts = FALSE, type = "mixed", lang = lang)
