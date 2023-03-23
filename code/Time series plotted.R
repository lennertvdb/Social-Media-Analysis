##############################
##############################
###### 1. Descriptives  ######
##############################
##############################


#0_Set working directory, load the required packages, and clear the environment
setwd("c:/Users/vando/OneDrive/Documenten/Data Science 4 Business/2_Social Media and Web Analytics/Assignment/Social_media_group12/code/")

rm(list=ls())

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(ggplot2, dplyr, corrplot, Hmisc)

load("./../data/basetable_final.rdata")
head(basetable)

colSums(is.na(basetable))/nrow(basetable) ; sum(is.na(basetable)) 

# 1_plot stockprice
ggplot(basetable, aes(x=1:length(basetable[,1]), y=price_stock)) +
  geom_line() + 
  xlab("")

# 2_plot stock volume
ggplot(basetable, aes(x=1:length(basetable[,1]), y=volume_stock)) +
  geom_line() + 
  xlab("")

# 3_plot sentiment
ggplot(basetable, aes(x=1:length(basetable[,1]), y=sentiment_impact)) +
  geom_line() + 
  xlab("")


# 3_plot the covariant matrix
df_cor <- basetable %>% select(-c(time))

cor(df_cor, use = "complete.obs")