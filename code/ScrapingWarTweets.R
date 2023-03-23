#------Clean the environment----------
rm(list=ls()) 

#------Install and load packages------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)

setwd("C:/Users/Lenne/Documents/UNI2021-2022/Semester 2/Social Media and Web Analytics")

#------Tokens and keys------
source('tokensandkeys.R')
get_token()
token

# ------Scraping Tweets-------
tweets_putin <- search_tweets("#putin", n = 18000, 
                                include_rts = FALSE,
                                retryonratelimit = FALSE,
                                type="mixed",
                                lang="en")

#Save as .csv with date of today
today <- as.character(Sys.Date())
filename <- paste('C://Users//Lenne//Documents//UNI2021-2022//Semester 2//Social Media and Web Analytics//Assignment//data//tweets_#putin_', today, '.csv', sep="")
write_as_csv(x = tweets_putin, filename)

------------------------------------------------------------
tweets_ww3 <- search_tweets("#ww3", n = 18000, 
                                include_rts = FALSE,
                                retryonratelimit = FALSE,
                                type="mixed",
                                lang = "en")

#Save as .csv with date of today
today <- as.character(Sys.Date())
filename <- paste('C://Users//Lenne//Documents//UNI2021-2022//Semester 2//Social Media and Web Analytics//Assignment//data//tweets_#ww3_', today, '.csv', sep="")
write_as_csv(x = tweets_ww3, filename)
------------------------------------------------------------
tweets_stopwar <- search_tweets("#stopwar", n = 18000, 
                                include_rts = FALSE,
                                retryonratelimit = FALSE,
                                type="mixed",
                                lang = 'en')


#Save as .csv with date of today
today <- as.character(Sys.Date())
filename <- paste('C://Users//Lenne//Documents//UNI2021-2022//Semester 2//Social Media and Web Analytics//Assignment//data//tweets_#stopwar_', today, '.csv', sep="")
write_as_csv(x = tweets_stopwar, filename)
------------------------------------------------------------
tweets_stopwar <- search_tweets("#RussiaUkraineWar", n = 18000, 
                                  include_rts = FALSE,
                                  retryonratelimit = FALSE,
                                  type="mixed",
                                  lang = 'en')


#Save as .csv with date of today
today <- as.character(Sys.Date())
filename <- paste('C://Users//Lenne//Documents//UNI2021-2022//Semester 2//Social Media and Web Analytics//Assignment//data//tweets_#stopwar_', today, '.csv', sep="")
write_as_csv(x = tweets_stopwar, filename)