# concat_tweets.R
#
# (To be checked with the group if this is optimal strategy)

# load packages
library(tidyverse)
library(stringr)
library(lubridate)

# define hashtags
hashtags_selected <- c("#putin","#RusUkrWar","#stopwar","#ww3")


for (i in 1:length(hashtags_selected)){
  
  # get the paths and filenames
  path_to_data <- str_c("./../data/" ,hashtags_selected[i])
  files <- list.files(path_to_data)
  
  # concat tweets of same hashtag
  for (j in 1:length(files)){
    
    print(paste(i,j))
    
    # load twitter df
    tweets_loaded <- read_csv(str_c(path_to_data,"/",files[j]), show_col_types = FALSE)
   
    # check if actual data in file, otherwise continue
    if (nrow(tweets_loaded) == 0){
      print(paste("No data in file: ",files[j]))
      next
    }
    
    # add scrape hashtag to df
    tweets_loaded <- tweets_loaded %>% mutate(hashtag_scrape = hashtags_selected[i])
    
    # add day of scrape to df
    date_scrape <- str_match(files[j],"\\w+(\\d{4}-\\d{2}-\\d{2})\\.csv")[2]
    tweets_loaded <- tweets_loaded %>% mutate(date_scrape = date_scrape)
    
    # get booleans if media and url or not
    tweets_loaded <- tweets_loaded %>% mutate(url = !is.na(urls_url)) %>% 
                                       mutate(media = !is.na(media_url))
    
    # select relevant columns
    cols_relevant <- c("date_scrape","created_at","text","media","url","favorite_count","retweet_count","hashtags","hashtag_scrape","screen_name","followers_count","friends_count","verified")
    tweets_loaded <- tweets_loaded %>% select(cols_relevant)
    
    # append df's row-wise in correct way
    if (j == 1){
      tweets_hashtag <- tweets_loaded
    }else{
      tweets_hashtag <- rbind(tweets_hashtag, tweets_loaded)
    }
  }
  
  # concat tweets of different hashtags 
  if (i == 1){
    tweets <- tweets_hashtag
  }else{
    tweets <- rbind(tweets,tweets_hashtag)
  }
}

# remove scrape dependent information of tweet (EXCEPTION for FOLLOWERS_COUNT and FRIENDS_COUNT)
cols_relevant <- c("created_at","text","media","url","hashtags","hashtag_scrape","screen_name","followers_count","friends_count","verified")
tweets_unique <- tweets %>% select(cols_relevant) %>% unique() %>% arrange(created_at,text)

tweets_unique_grouped <- tweets_unique %>%
                         group_by(created_at,text,media,url,hashtags,hashtag_scrape,screen_name,verified) %>%
                         summarise(followers_count = mean(followers_count),friends_count = mean(friends_count)) 
        
# save tweet df
write.csv(tweets_unique_grouped,"./../data/tweets_concat.csv", row.names = FALSE)

# load tweets again as check
test <- read.csv("./../data/tweets_concat.csv")

