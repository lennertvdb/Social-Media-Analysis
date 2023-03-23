#DESCRIPTIVE STATISTICS
#______________________

#loading fo required packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(rtweet, httr,tidyverse)
p_load(tm,wordcloud, wordcloud2)
p_load(SnowballC, slam, tm)
p_load(tidytext)
p_load(vader)
p_load(tidyverse,textclean, textstem, sentimentr, lexicon)
p_load(text2vec,Rtsne,scales,ggrepel,tidyverse,tm)

#Required functions
cleanText <- function(text) {
        clean_texts <- text %>%
                str_replace_all("<.*>", "") %>%                         # remove remainig emojis
                str_replace_all("&amp;", "") %>%                        # remove &
                str_replace_all("(RT|via)((?:\\b\\W*@\\w+)+)", "") %>%  # remove retweet entities
                str_replace_all("@\\w+", "") %>%                        # remove @ people, replace_tag() also works
                str_replace_all('#', "") %>%                            #remove only hashtag, replace_hash also works
                str_replace_all("[[:punct:]]", "") %>%                  # remove punctuation
                str_replace_all("[[:digit:]]", "") %>%                  # remove digits
                str_replace_all("http\\w+", "") %>%                     # remove html links replace_html() also works
                str_replace_all("[ \t]{2,}", " ") %>%                   # remove unnecessary spaces
                str_replace_all("^\\s+|\\s+$", "") %>%                  # remove unnecessary spaces
                str_trim() %>% 
                str_to_lower()
        return(clean_texts)
}
create_adjacency_matrix <- function(object, probs=0.99){
        
        #object = output from function create_document_term_matrix (a document by term matrix)
        #probs = select only vertexes with degree greater than or equal to quantile given by the value of probs
        
        cat("Create adjacency matrix \n")
        p_load(sna)
        
        mat <- as.matrix(object)
        mat[mat >= 1] <- 1 #change to boolean (adjacency) matrix
        Z <- t(mat) %*% mat
        
        cat("Apply filtering \n")
        ind <- sna::degree(as.matrix(Z),cmode = "indegree") >= quantile(sna::degree(as.matrix(Z),cmode = "indegree"),probs=0.99)
        #ind <- sna::betweenness(as.matrix(Z)) >= quantile(sna::betweenness(as.matrix(Z)),probs=0.99)
        
        Z <- Z[ind,ind]        
        
        cat("Resulting adjacency matrix has ",ncol(Z)," rows and columns \n")
        dim(Z)
        list(Z=Z,termbydocmat=object,ind=ind)
}
plot_network <- function(object){
        #Object: output from the create_adjacency_matrix function
        
        #Create graph from adjacency matrix
        p_load(igraph)
        g <- graph.adjacency(object$Z, weighted=TRUE, mode ='undirected')
        g <- simplify(g)
        
        #Set labels and degrees of vertices
        V(g)$label <- V(g)$name
        V(g)$degree <- igraph::degree(g)
        
        layout <- layout.auto(g)
        opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
        #Adjust the widths of the edges and add distance measure labels
        #Use 1 - binary (?dist) a proportion distance of two vectors
        #The binary distance (or Jaccard distance) measures the dissimilarity, so 1 is perfect and 0 is no overlap (using 1 - binary)
        edge.weight <- 7  #A maximizing thickness constant
        z1 <- edge.weight*(1-dist(t(object$termbydocmat)[object$ind,], method="binary"))
        E(g)$width <- c(z1)[c(z1) != 0] #Remove 0s: these won't have an edge
        clusters <- spinglass.community(g)
        cat("Clusters found: ", length(clusters$csize),"\n")
        cat("Modularity: ", clusters$modularity,"\n")
        plot(g, layout=layout, vertex.color=rainbow(4)[clusters$membership], vertex.frame.color=rainbow(4)[clusters$membership] )
}


#Wordcloud
hashtags_selected <- c("#putin","#RusUkrWar","#stopwar","#ww3")
test <- read.csv("./../data/tweets.csv")

text<-tweets %>% pull(text)

text_clean<- text %>% str_to_lower() %>% 
        replace_emoji() %>% 
        replace_emoticon() %>% 
        replace_contraction() %>%
        replace_internet_slang() %>% 
        replace_kern() %>% 
        replace_word_elongation() %>%
        str_replace_all("[[:punct:]]","") %>%
        str_replace_all("[[:digit:]]", "") %>%
        str_squish() %>% cleanText() %>% removeWords(stopwords('english'))

lemma_dictionary_hs <- make_lemma_dictionary(text_clean, engine = 'hunspell')
text_final <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)
text_final<-text_final %>%  removeWords(c("tcoogoglsbho","tcozrmbjdfmgz",
                                          "tcoqkjaca","tcoyjnkqssa","look",
                                          "rainbow",
                                          "tcoclewu","tcojhoup","yep",
                                          "the","tcojsbkzywqd","amp","end","like","russia","stopwar",
                                          "ukrainerussiawar","stoprussia","ukrainian","russian","ukrainrussianwar",
                                          "russianukrainianwar","since","will","ukraine","ukrainewar",
                                          "putin","ukrainians","russiauraine","putins",
                                          "ukrainerussianwar","let","said","vladimir","standwithukraine","stopwarinukraine",
                                          "kremlinrussiae","see","call",
                                          "stopped","can","get","russians","russiaukraineconflict","ukrainrussia",
                                          "many","already","putinswar","every","russiaukrainewar",
                                          "ukraineunderattack","must","stopputin","also","stop","war","russiaukraine","ukrainerussia","start","ww"))

save(text_final,file="text_final.RData")
save(text_clean,file="text_clean.RData")

load("./../data/text_final.RData")
tdm <- TermDocumentMatrix(Corpus(VectorSource(text_final)))
tdm_sparse<-removeSparseTerms(tdm, sparse= 0.995)
tdm_sparse
m<-as.matrix(tdm_sparse)
v<-sort(rowSums(m),decreasing=TRUE)
d<-tibble(word=names(v),freq=v)
for(i in 1:length(hashtags_selected)){
        d<-d %>% filter(word != tolower(hashtags_selected[i]))
}
d <- d %>% arrange(desc(freq))
wordcloud2(d[1:200,],size=2,minSize = 1)




#Create a wordgraph
#------------------
myCorpus <- Corpus(VectorSource(text_final))
doc_term_mat <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
dtm_sparse<-removeSparseTerms(doc_term_mat, sparse= 0.9982)
dtm_sparse


adj_mat <- create_adjacency_matrix(dtm_sparse)

plot_network(adj_mat)

#apply t-SNE
#___________

top_400<-d[1:100,"word"]
top_400<-top_400$word

tokens <- space_tokenizer(text_final)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 3L)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove <- GloVe$new(rank = 86, 
                   x_max = 5)
word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = 40)
word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components)

word_vec <- word_vectors[top_400,]
train_df <- data.frame(word_vec) %>%
        rownames_to_column("word")
tsne <- Rtsne(train_df[,-1], 
              dims = 2, perplexity = 30,
              verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
        mutate(
                word = train_df$word,
                col = colors[train_df$word]) %>%
        left_join(vocab, by = c("word" = "term")) 

ggplot(plot_df, aes(X1, X2, label = word, color = col)) + 
        geom_text(size = 4) +
        xlab("") + ylab("") +
        theme(legend.position = "none")   
