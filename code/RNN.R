#------------ RNN for stock prediction ----------------

#------Clean the environment----------
rm(list=ls()) 

#------Install and load packages------
if (!require('pacman')) install.packages('pacman') ; require('pacman')
p_load(tidyverse, keras, caret, ggplot2)

#------Setup working directory------
setwd('C:/Users/Lenne/Desktop/Social_media_group12')

#------Loading data------
load('C:/Users/Lenne/Desktop/Social_media_group12/data/basetable_final.RData')

#------Exploring data------
#Price 
price <- basetable %>% 
  ggplot(aes(x = time,
             y = price_stock))
price + geom_line(size = 0.5)

#Volume
volume <- basetable %>% 
  ggplot(aes(x = time,
             y = volume_stock))
volume + geom_line(size = 0.5)


#------Recurrent Neural Network------
set.seed(123)

##---Set model parameters
max_len <- 2
batch_size <- 32
total_epochs <- 600

##---Sub sampling technique
#-As we have not much observations to train a deep learning
# model we will stretch out our data, we can use something called 
# moving-block sub-sampling, which is where we cut up our vector into 
# overlapping chunks. 

#-IMPORTANT: we care about the order that our observations are in and 
# want to make sure it's preserved. 

# Cut the text in overlapping sample sequences of max_len characters

# get a list of start indexes for our (overlapping) chunks
start_indexes <- seq(1, length(basetable$price_stock) - (max_len + 1), by = 1)

# create an empty matrix to store our data in
stock_matrix <- matrix(nrow = length(start_indexes), ncol = max_len + 1)

# fill our matrix with the overlapping slices of our dataset
for (i in 1:length(start_indexes)){
  stock_matrix[i,] <- basetable$price_stock[start_indexes[i]:(start_indexes[i] + max_len)]
}

# remove na's, just to be sure
if(anyNA(stock_matrix)){
  stock_matrix <- na.omit(stock_matrix)
}

##--- Splitting in train and test 
# split our data into the 'moment' we want to predict (y)
# sequence of 'moments' leading up to it (X)
X <- stock_matrix[,-ncol(stock_matrix)]
y <- stock_matrix[,ncol(stock_matrix)]

# create an index to split our data into testing & training sets
training_index <- createDataPartition(y, p = .8, 
                                      list = FALSE, 
                                      times = 1)
# training data
X_train <- array(X[training_index,], dim = c(length(training_index), max_len, 5))
y_train <- y[training_index]

# testing data
X_test <- array(X[-training_index,], dim = c(length(y) - length(training_index), max_len, 5))
y_test <- y[-training_index]

##--- Specify our model 

# We will keep things simple and choose to have just 3 layers. 
# An input layer, 1 hidden layer and 1 output layer.

# initialize our model
model <- keras_model_sequential()

#- Input Layer
# dimensions of our input data
dim(X_train)

# our input layer
model %>%
  layer_dense(input_shape = dim(X_train)[2:3], units = max_len)

#- Hidden layer
model %>% 
  layer_simple_rnn(units = 6)

#- Output layer 
model %>%
  layer_dense(units = 1, activation = 'linear')

##--- Model Architecture
summary(model)

model %>% compile(loss = 'MSE', 
                  optimizer = 'RMSprop',
                  metrics = c("mae"))

#--- Training our model 
trained_model <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = batch_size, # how many samples to pass to our model at a time
  epochs = total_epochs, # how many times we'll look @ the whole dataset
  validation_split = 0.2) # how much data to hold out for testing as we go along

plot(trained_model)

#--- Evaluate model 
model %>% evaluate(X_test, y_test, batch_size = batch_size)
