text_sentiment<- text %>% str_to_lower() %>% 
        replace_emoji() %>% 
        replace_emoticon() %>% 
        replace_contraction() %>%
        replace_internet_slang() %>% 
        replace_kern() %>% 
        replace_word_elongation() %>%
        str_replace_all("[[:digit:]]", "") %>%
        str_squish() %>% cleanText() %>% removeWords(stopwords('english'))


save(text_sentiment,file="text_sentiment.RData")
save(text_sentiment_final,file="text_sentiment_final.RData")


lemma_dictionary_hs <- make_lemma_dictionary(text_sentiment,engine = 'hunspell')
text_sentiment_final <- lemmatize_strings(text_sentiment, dictionary = lemma_dictionary_hs)

sentiment<-text_sentiment_final %>% sentiment_by(averaging.function = average_weighted_mixed_sentiment)

tweets<-cbind(tweets,sentiment)


save(tweets,file="tweets_sentiment.RData")
