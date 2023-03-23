# create_basetable.R
#
library(ggplot2)
library(lubridate)
library(dplyr)

# load in the data
load("./../data/aggregated_twitterData.rdata")
data_stocks <- read.csv("./../data/stocks.csv", row.names = "X")

# convert to datetime
data_stocks$data_stock <- parse_date_time(data_stocks$data_stock, "ymd HM")

# create basetable
basetable <- data.frame(time = seq(ymd_hms('2022-03-02 13:00:00'),ymd_hms('2022-03-22 10:25:00'), by = '5 mins'))

# join 
basetable <- merge(basetable, data_stocks, by.x = "time", by.y = "data_stock", all.x = TRUE)
basetable <- merge(basetable, tweets_topic_grouped, by.x = "time", by.y = "created_at", all.x = TRUE)
basetable <- merge(basetable, tweets_sentiment_grouped, by.x = "time", by.y = "created_at", all.x = TRUE)

## impute values not explicitly present in dataset
basetable <- basetable %>% mutate(topic_1_impact_normalised = ifelse(is.na(topic_1_impact_normalised), 0, topic_1_impact_normalised)) %>%  # topic 1 
                           mutate(topic_2_impact_normalised = ifelse(is.na(topic_2_impact_normalised), 0, topic_2_impact_normalised)) %>%  # topic 2
                           mutate(sentiment_impact = ifelse(is.na(sentiment_impact), 0, sentiment_impact)) %>%                             # sentiment
                           mutate(sentiment_impact_std = ifelse(is.na(sentiment_impact_std), 0, sentiment_impact_std)) %>%                 # sentiment std
                           mutate(volume_stock = ifelse(is.na(volume_stock), 0, volume_stock))                                             # volume

# price stock
stock_last <- basetable$price_stock[1]
for (i in 2:nrow(basetable)){
  
  if (is.na(basetable$price_stock[i])){
    basetable$price_stock[i] <- stock_last
    print(paste(i,"replaced"))
  }else{
    stock_last <- basetable$price_stock[i]
  }
}

#

## filter basetable for times when the market is open
# quit elaborate to code this explicitly. A few constraints that has to be met
# 1 - from 15u20 tem 22u00
# 2 - select only weekdays
# 3 - starting from 14/03, the opening times seems shifted by 1 hour
# --> The easy solution is to filter the rows of this small dataset by hand

#write.csv(basetable,"./../data/basetable_fullPeriod.csv", row.names = FALSE)
basetable <- read.csv("./../data/basetable_selectedPeriod.csv", header = TRUE) 
basetable$time <- parse_date_time(basetable$time, "ymd HMS")

# create dependent value (Shift 5 minutes)
dependend_colum <- c(NA, basetable$price_stock[1:length(basetable$price_stock) -1])
basetable$dependend <- dependend_colum

## save baseteble_temporary
save(basetable, file = "./../data/basetable_final.rdata")
load("./../data/basetable_final.rdata")

## MAKE NICE VISUALISATIONS
# visualise tweets
ggplot(tweets_topic_grouped, aes(x = created_at, y = topic_1_impact_normalised)) + geom_line()
ggplot(tweets_topic_grouped, aes(x = created_at, y = topic_2_impact_normalised)) + geom_line()

# visualise stockprice
ggplot(data_stocks, aes(x = data_stock, y = price_stock)) + geom_line()
ggplot(data_stocks, aes(x = data_stock, y = volume_stock)) + geom_line()
