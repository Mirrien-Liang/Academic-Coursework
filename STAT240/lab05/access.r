# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

appname <- "Translink-disruption"

key <- "RRClwMJRrZoADnBUBtkoxJ963"

secret <- "iTd6m8zub282J0TO4sBDe2Z5h24w9POpS1GOqJ9dwPD230ZEXi"

access_token <- "1497398114113634306-d6WOVmtuT3MxN8jsmvHsu6R7bnLcxN"

access_secret <- "e5au8uIQl3vt7HDpZ8hGdAcitGIajQ8IGpWr00Zz4YhhJ"


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

setup_twitter_oauth(key,secret,access_token, access_secret)


rm(key)
rm(secret)
rm(access_token)
rm(access_secret)
