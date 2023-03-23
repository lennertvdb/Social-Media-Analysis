# aggregate_tweets.R
#
#

# load packages
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)

## load origina tweets concat
#tweets_concat <- read.csv("./../data/tweets_concat.csv")

## load and preprocess toppics data
load("./../data/df_withTopics.RData")
tweets_topic <- df_topic_documents_unique
colnames(tweets_topic)[11] <- "topic_value"
colnames(tweets_topic)[12] <- "topic_prob"
rm(df_topic_documents_unique) # remove old from memory
tweets_topic$created_at <- parse_date_time(tweets_topic$created_at, "ymd HMS")   # convert to datetime
tweets_topic$followers_count_scaled <- ((tweets_topic$followers_count)^(1/4)/20) # get followers count to a non exploding distribution between 0 and 1
tweets_topic$dummy <- 1  # create dummy column used in the advanced calculations

## load and preprocess sentiment data
load("./../data/tweets_sentiment.RData")
tweets_sentiment <- tweets
rm(tweets)
tweets_sentiment$created_at <- parse_date_time(tweets_sentiment$created_at, "ymd HMS")   # convert to datetime 
#tweets_sentiment$sentiment_scaled <- 2*(tweets_sentiment$ave_sentiment - min(tweets_sentiment$ave_sentiment))/(max(tweets_sentiment$ave_sentiment)- min(tweets_sentiment$ave_sentiment)) -1   ## SCALING of sentiment not included in final solution
tweets_sentiment$followers_count_scaled <- ((tweets_sentiment$followers_count)^(1/4)/20)
tweets_sentiment$dummy <- 1

## group toppics data
tweets_topic_grouped <- tweets_topic %>% group_by(created_at=floor_date(created_at, "5 minutes"), topic_value) %>%
                                         summarize(topic_impact = sum((media + url + 2*verified + 4*followers_count_scaled)*dummy), topic_count = n())

## group sentiment data
tweets_sentiment_grouped <- tweets_sentiment %>% group_by(created_at=floor_date(created_at, "5 minutes")) %>%
                                                 summarize(sentiment_impact = mean((media + url + 2*verified + 4*followers_count_scaled)*ave_sentiment), sentiment_impact_std = sd((media + url + 2*verified + 4*followers_count_scaled)*ave_sentiment) , tweet_count = n())

## Process topic impact scores
# get wide format
tweets_topic_wide <- tweets_topic_grouped %>% pivot_wider(names_from = topic_value, values_from = c(topic_impact, topic_count))

# normalise importance score (independent of number of tweets)
tweets_topic_wide <- tweets_topic_wide %>%  mutate(topic_1_impact_normalised = topic_impact_1/sum(topic_count_1, topic_count_2, na.rm = TRUE)) %>%
                                            mutate(topic_2_impact_normalised = topic_impact_2/sum(topic_count_1, topic_count_2, na.rm = TRUE))
  
## select relevant columns
# topics
tweets_topic_grouped <- tweets_topic_wide %>% select(c(created_at, topic_1_impact_normalised, topic_2_impact_normalised)) %>%
                                           mutate(topic_1_impact_normalised =  replace_na(topic_1_impact_normalised,0)) %>%
                                           mutate(topic_2_impact_normalised =  replace_na(topic_2_impact_normalised,0))
# sentiment
tweets_sentiment_grouped <- tweets_sentiment_grouped %>% select(c(created_at, sentiment_impact, sentiment_impact_std))


## save results to disk
save(tweets_topic_grouped,tweets_sentiment_grouped, file = "./../data/aggregated_twitterData.rdata")


# load tweets again as check
load("./../data/aggregated_twitterData.rdata")



# --------------- MAKE RELEVANT FIGURES -----------------
# scaling of followers_count
ggplot(tweets_topic, aes(x=followers_count)) + geom_histogram() + ggtitle("Distrubution of followers count")
ggplot(tweets_topic, aes(x=(followers_count)^(1/4)/20)) + geom_histogram() + ggtitle("Distrubution of transformed followers count")

# quick fix drop NA VALUES...
#tweets_topic_wide <- tweets_topic_wide %>% drop_na()

# time series - topic impact
tweets_topic_wide_plot <- melt(tweets_topic_wide ,  id.vars = 'created_at', variable.name = 'topic')
tweets_topic_wide_plot <- tweets_topic_wide_plot %>% filter(topic %in% c("topic_1_impact_normalised","topic_2_impact_normalised"))

# make sure the thing is as a factor
tweets_topic_wide_plot <- tweets_topic_wide_plot %>% mutate(topic = as_factor(topic))

#
ggplot(tweets_topic_wide_plot, aes(x = created_at,y =  value)) + geom_line(aes(colour = topic)) # , alpha = 0.9  # 

# plot sentiment tweets histogram
ggplot(tweets_sentiment, aes(x = ave_sentiment)) + geom_histogram()
#ggplot(tweets_sentiment, aes(x = sentiment_scaled)) + geom_histogram() + scale_y_log10() #scaled sentiment (-1 tot 1)
ggplot(tweets_sentiment_grouped, aes(x = sentiment_impact)) + geom_histogram() #scaled sentiment (-1 tot 1)
ggplot(tweets_sentiment_grouped, aes(x = sentiment_impact_std)) + geom_histogram() #scaled sentiment (-1 tot 1)
