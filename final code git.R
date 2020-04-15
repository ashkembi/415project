##############################################################################
#   STATS 415 Final Project
##############################################################################


# setting working directory
setwd('/Users/abasshkembi/Downloads/UMICH\ Classes/2019-2020/W20/STATS\ 415/project/')

# packages
library(tidyverse)
library(ISLR)
library(MASS)
library("FNN")

# reading final_project.csv
final <- read.csv('final_project.csv')
# creating t variable, which is the minute
final <- final %>% mutate(t = X + 1) %>% dplyr::select(-X)

#########################################
##### Problem 2.1
#########################################


### I currently have this code commented out because it's just easier to
### read in the resulting csv created from this code each time


######

############# code for h = 3
#BRet_3 <- final %>% mutate(t = ifelse(t - 3 < 1, 1, t-3)) %>%
#left_join(final, by="t") %>%
#mutate(Asset_1_BRet_3 = round((Asset_1.x - Asset_1.y)/Asset_1.y, 4),
#       Asset_2_BRet_3 = round((Asset_2.x - Asset_2.y)/Asset_2.y, 4),
#       Asset_3_BRet_3 = round((Asset_3.x - Asset_3.y)/Asset_3.y, 4)) %>%
#dyplr::select(Asset_1_BRet_3, Asset_2_BRet_3, Asset_3_BRet_3) %>%
#mutate(t = 1:nrow(.))

############# code for h = 10
#BRet_10 <- final %>% mutate(t = ifelse(t - 10 < 1, 1, t-10)) %>% # h = 10
#left_join(final, by = "t") %>%
#mutate(Asset_1_BRet_10 = round((Asset_1.x - Asset_1.y)/Asset_1.y, 4),
#       Asset_2_BRet_10 = round((Asset_2.x - Asset_2.y)/Asset_2.y, 4),
#       Asset_3_BRet_10 = round((Asset_3.x - Asset_3.y)/Asset_3.y, 4)) %>%
#dyplr::select(Asset_1_BRet_10, Asset_2_BRet_10, Asset_3_BRet_10) %>%
#mutate(t = 1:nrow(.))

############# code for h = 30
#BRet_30 <- final %>% mutate(t = ifelse(t - 30 < 1, 1, t-30)) %>% # h = 30
#left_join(final, by = "t") %>%
#mutate(Asset_1_BRet_30 = round((Asset_1.x - Asset_1.y)/Asset_1.y, 4),
#       Asset_2_BRet_30 = round((Asset_2.x - Asset_2.y)/Asset_2.y, 4),
#       Asset_3_BRet_30 = round((Asset_3.x - Asset_3.y)/Asset_3.y, 4)) %>%
#dyplr::select(Asset_1_BRet_30, Asset_2_BRet_30, Asset_3_BRet_30) %>%
#mutate(t = 1:nrow(.))

#BRet_full <- left_join(BRet_3, BRet_10, by="t") %>%
#left_join(BRet_30, by="t") %>%
#dyplr::select(-t)

#write.csv(BRet_full, 'bret.csv', row.names=FALSE)

######

# reading in resulting csv from 2.1
BRet_full <- read.csv('bret.csv')


####################################
##### Problem 2.2
####################################


### I currently have this code commented out because it's just easier to
### read in the resulting csv created from this code each time


#Rho <- BRet_full %>%
#  mutate(t = 1:nrow(.)) %>%
#  dyplr::select(t, Asset_1_BRet_3, Asset_2_BRet_3, Asset_3_BRet_3)


#w = 21*24*60
#Rho_1_2 <- vector(length = nrow(final))
#Rho_1_3 <- vector(length = nrow(final))
#Rho_2_3 <- vector(length = nrow(final))

#for(i in 1:nrow(BRet_full)){
#  min_t = ifelse(Rho$t[i] - w < 1, 1, Rho$t[i] - w)
#  max_t = Rho$t[i]

#  Asset_1 <- Rho$Asset_1_BRet_3[min_t:max_t]
#  Asset_2 <- Rho$Asset_2_BRet_3[min_t:max_t]
#  Asset_3 <- Rho$Asset_3_BRet_3[min_t:max_t]

#  Rho_1_2[i] <- round(cor(Asset_1, Asset_2), 4)
#  Rho_1_3[i] <- round(cor(Asset_1, Asset_3), 4)
#  Rho_2_3[i] <- round(cor(Asset_2, Asset_3), 4)
#}

#Rho_corr <- tibble(Rho_1_2, Rho_1_3, Rho_2_3)

#write.csv(Rho_corr, 'corr.csv', row.names=FALSE)



#########################################
##### Problem 2.3
#########################################


f_10 <- final %>% mutate(t = ifelse(t + 10 < nrow(final), t + 10, nrow(final))) %>% # h = 10
  left_join(final, by="t") %>%
  mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
         Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
         Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
  dyplr::select(Asset_1_BRet_10f, Asset_2_BRet_10f, Asset_3_BRet_10f) %>%
  mutate(t = 1:nrow(.)) %>%
  dyplr::select(t, everything())

# adding the 10 minute forward return of Asset 1 into bret data
bret_2.3 <- BRet_full %>% mutate(Asset_1_BRet_10f = f_10$Asset_1_BRet_10f)

### creating train and test df
set.seed(816)
train_id <- sample(1:nrow(bret_2.3), trunc(0.7*nrow(bret_2.3)))
train2.3 <- bret_2.3[train_id,]
test2.3 <- bret_2.3[-train_id,]
###

# linear model
mod2.3 <- lm(Asset_1_BRet_10f ~ ., data = train2.3)
summary(mod2.3)
# we see that Asset_2_BRet_3, Asset_3_BRet_3, Asset_2_BRet_10, Asset_3_BRet_10, and Asset_2_BRet_30 are all significant 
# predictors, while only Asset_3_BRet_30 is not for asset 3

# prediction, creating new variable in each dataset with prediction
train2.3_pred <- train2.3 %>% mutate(Asset_1_BRet_10f.pred = predict(mod2.3, train2.3))
test2.3_pred <- test2.3 %>% mutate(Asset_1_BRet_10f.pred = predict(mod2.3, test2.3))

# in-sample correlation
cor(train2.3$Asset_1_BRet_10f, train2.3_pred$Asset_1_BRet_10f.pred)

# out-of-sample correlation
cor(test2.3$Asset_1_BRet_10f, test2.3_pred$Asset_1_BRet_10f.pred)

# 3-week backward correlation between Asset_1_BRet_10f and Asset_1_BRet_10f.pred
Rho2.3 <- rbind(train2.3_pred, test2.3_pred) %>% mutate(t = 1:nrow(.))

# 3-weeks in minutes
w = 21*24*60
#initializing new vector
Rho_10f_10f.pred <- vector(length = nrow(final))

for(i in 1:nrow(final)){
  min_t = ifelse(Rho2.3$t[i] - w < 1, 1, Rho2.3$t[i] - w) # minumum t to select (3-weeks ago)
  max_t = Rho2.3$t[i]                                     # maximum t (current time)
  
  #vector of Asset_1_BRet_10f from minute "min_t" to minute "max_t"
  Asset_1_BRet_10f <- Rho2.3$Asset_1_BRet_10f[min_t:max_t]
  #vector of Asset_1_BRet_10f.pred from minute "min_t" to minute "max_t"
  Asset_1_BRet_10f.pred <- Rho2.3$Asset_1_BRet_10f.pred[min_t:max_t]
  
  # store correlation between Asset_1_BRet_10f and Asset_1_BRet_10f.pred from min_t to max_t
  Rho_10f_10f.pred[i] <- round(cor(Asset_1_BRet_10f, Asset_1_BRet_10f.pred), 4)
}

Rho2.3_corr <- Rho2.3 %>% dplyr::select(t, Asset_1_BRet_10f, Asset_1_BRet_10f.pred) %>%
  mutate(Rho_10f_10f = Rho_10f_10f.pred)

########
### write.csv(Rho2.3_corr, 'Rho2.3_corr.csv')
########

# plot of 3-week rolling back correlations over the whole year
Rho2.3_corr %>% 
  filter(Rho_10f_10f > -0.1) %>%
  ggplot() +
  geom_line(aes(x = t, y = Rho_10f_10f)) +
  theme_bw()


#########################################
##### Problem 2.4
#########################################

# create train and test set for 2.4 in matrices
trainX2.4 <- as.matrix(train2.3[-length(colnames(train2.3))]) # removing Asset_1_BRet_10f
testX2.4 <- as.matrix(test2.3[-length(colnames(test2.3))]) # removing Asset_1_BRet_10f

#set values of K
k_vals <- c(5, 25, 125, 625, 1000)

###############################################

##### This should work (as I tried it with the first 100 rows) but takes several minutes to run
##### Try to find a new way to make it run faster

# initializing new train and test MSE vectors
knnTrainErr <- vector(length = length(k_vals))
knnTestErr <- vector(length = length(k_vals))
for (i in 1:length(k_vals)) {
  knn_pred_train <- knn.reg(train = trainX2.4, 
                            test = trainX2.4, 
                            y = train2.3$Asset_1_BRet_10f, 
                            k = k_vals[i])
  knn_pred_test <- knn.reg(train = trainX2.4, 
                           test = testX2.4, 
                           y = train2.3$Asset_1_BRet_10f, 
                           k = k_vals[i])
  
  knnTrainErr[i] <- mean((train2.3$Asset_1_BRet_10f - knn_pred_train$pred)^2)
  knnTestErr[i] <- mean((test2.3$Asset_1_BRet_10f - knn_pred_test$pred)^2)
}
#################################################

##### Trying to make the KNN run faster

# creating knn function to then use "map" to get errors
knn_fun <- function(x) {
  knn_pred_train <- knn.reg(train = trainX2.4, 
                            test = trainX2.4, 
                            y = train2.3$Asset_1_BRet_10f, 
                            k = x)
  knn_pred_test <- knn.reg(train = trainX2.4, 
                           test = testX2.4, 
                           y = train2.3$Asset_1_BRet_10f, 
                           k = x)
  
  return(list(mean((train2.3$Asset_1_BRet_10f - knn_pred_train$pred)^2), # train MSE
              mean((test2.3$Asset_1_BRet_10f - knn_pred_test$pred)^2)))  # test MSE
}

# mapping knn_fun over all of the k_vals
k_vals %>% map(knn_fun)
















