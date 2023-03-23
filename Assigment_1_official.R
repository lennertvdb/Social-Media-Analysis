###########################################################################
######## Assignment 1 #########
###########################################################################

##########################
### Part1 ###
##########################

# Clean the environment
rm(list=ls()) 

# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv)

#------Tokens and keys------
app <- 'mthwgw'
consumer_key <- 'RpL3gebERbqY0UQ3EMjbx2N7M'
consumer_secret <- '9waUy1RJJjY4w3mjd1wrQ9mPywIcdIwP9uuWpindc85CSSdiwk'
access_token <- '3438213070-rwzCiHxDgxpBdF3XVIuJbGwbRPdUSkiGVwyHtTU'
access_secret <- '24TO6GwRaC1Ye0Uh9A4lPO9OE6iRSKCJQ27SwJR1E1rby'

token <- create_token(
  app = app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret, set_renv = FALSE)
get_token()
token

# Twitter screen name
my_name <- "mthwgw"

# All of my_name's followers
follower_names <- get_followers(my_name)

# Get the user- object for my_name 
status <- FALSE
while (status==FALSE) {
  rate <- rate_limits(get_token(), "lookup_users")
  status <- as.numeric(rate$remaining) > 50
  if (status) {
    
    cat('Extracting...')
    userInfo <- lookup_users(my_name)
  } else {
    cat("Waiting...")
    Sys.sleep(600) # wait 10 minutes
  }
}
glimpse(userInfo)

# Return user ID's
firstdegree <- get_followers(
  my_name, n = userInfo$followers_count, retryonratelimit = TRUE)

# Get screenname
followers <- lookup_users(firstdegree$user_id) %>% 
  pull(screen_name)

# Get second degree followers
seconddegree <- lookup_users(followers)

#S ee if we can find these second degree followers in my_name his followers
ind <- which(followers %in% seconddegree$screen_name)
followers[ind]

# Now extract user ids of followers-of-followers
l <- list()
for (i in 1:nrow(seconddegree)) {
  
  cat('... Scraping: ', seconddegree$screen_name[i], '\n')
  
  followersseconddegree <- character(seconddegree$followers_count[i])
  
  followersseconddegree <- get_followers(seconddegree$user_id[i], 
                                         retryonratelimit = TRUE) %>% pull(user_id)
  l[[i]] <- lookup_users(followersseconddegree) %>% pull(screen_name)
}

# Add followers-of-followers to followers list
l[[length(l)+1]] <- followers
names(l) <- c(seconddegree$screen_name,userInfo$screen_name)

#followers of my_name and followers of followers
glimpse(l)

# Only find the mutual followers
network_mthwgw <- vector(mode = "list", length = length(l))
a=1
for(i in 1:length(l)){
  for(b in 1: length(l[[i]])){
    if (l[[i]][b] %in% followers){
      network_mthwgw[[i]] <-  append(network_mthwgw[[i]],l[[i]][b]) 
      a <- a+1
      
    }
  }
}
names(network_mthwgw) <- followers

#delete my_name out of list
network_mthwgw[[80]] <- NULL

# Take a look at my_name his followers and the mutual followers
glimpse(network_mthwgw)

mm <- do.call("c", lapply(network_mthwgw, paste, collapse=" "))

############### Process data to create adjacency matrix ###########

#Install and load the text mining package to preprocess the data
p_load(SnowballC, tm)

myCorpus <- Corpus(VectorSource(mm))

#inspect the result
inspect(myCorpus)

# This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

#we can also look at the actual matrix
inspect(userfollower)

########################## Network Analysis ##############################

#load the required packages
p_load(igraph)

# Compute the adjacencuy matrix
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

# Take a look at the adjacency matrix
A

# Create a graph object based on the adjacency matrix & remove loop edges
g <- graph.adjacency(A, weighted=TRUE,
                     mode ='undirected') %>% simplify()

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)

set.seed(3952)

#prepare graph
layout <- layout.auto(g)
mar <- par()$mar 
par(mar=rep(0, 4))
plot(g, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","blue")[ifelse(V(g)$name %in%
                                             names(igraph::degree(g)[tail(order(igraph::degree(g)),5)]) ==TRUE,1,2)])

# The top 5 vertices based on degree are in green

# Compute the degree of the followers and save it in ground_truth_ranking
ground_truth_ranking <- sort(degree(g),decreasing = T)

# Take a look at the ranked degrees of the followers
ground_truth_ranking

###############################
### Part 2 ###
###############################

corr <- vector(mode = "list", length = length(l))

# Determine the correlation between the ground truth ranking and the current ranking of i followers
for (i in 2: length(network_mthwgw)) {
  #-compute adjacency matrix A using i followers
  adj_foll <- network_mthwgw[1:i]
  mm2 <- do.call("c", lapply(adj_foll, paste, collapse=" "))
  myCorpus2 <- Corpus(VectorSource(mm2))
  userfollower2 <- DocumentTermMatrix(myCorpus2, control = list(wordLengths = c(0, Inf)))
  adj <- t(as.matrix(userfollower2)) %*% as.matrix(userfollower2)
  #-compute the degree for all your followers
  g2 <- graph.adjacency(adj, weighted=TRUE,
                        mode ='undirected') %>% simplify()
  degree(g2)
  #-rank all your followers based on their degree and store in current ranking
  current_ranking <-  sort(degree(g2),decreasing = T)
  #-Compute Spearman's rank-based correlation
  correlation <- cor(ground_truth_ranking[1:length(current_ranking)],current_ranking, 
                     method="spearman")
  #-store both i and correlation
  corr[i] <-  append(corr[[i]],correlation) 
  names(corr[i]) <- i
}

# Delete the first observation from the list 
corr[[1]] <- NULL

# Delete my_name from the list
corr[[79]] <- NULL

# Look at the correlation
corr

# Plot the correlation 
x <- seq(1:78)
plot(x,corr[x],xlab = "number of followers", ylab = "correlation", main = "Correlation")



