# setting working directory
setwd('/Users/gabrielle/Desktop//STATS\ 415/')

# packages
library(tidyverse)
library(ISLR)
library(MASS)
library(FNN)
library(glmnet)
library(pls)

backwards_return <- function(h) { 
  BRet_h <- final %>% mutate(t = ifelse(t - h < 1, 1, t-h)) %>%
    left_join(final, by = "t") %>%
    mutate(Asset_1_BRet_h = round((Asset_1.x - Asset_1.y)/Asset_1.y, 4),
           Asset_2_BRet_h = round((Asset_2.x - Asset_2.y)/Asset_2.y, 4),
           Asset_3_BRet_h = round((Asset_3.x - Asset_3.y)/Asset_3.y, 4)) %>%
    dplyr::select(Asset_1_BRet_h, Asset_2_BRet_h, Asset_3_BRet_h) %>%
    mutate(t = 1:nrow(.))
  
  return (BRet_h)
}

rolling_var <- function(input, w) {
  var_1 <- vector(length = nrow(input))
  var_2 <- vector(length = nrow(input))
  
  for(i in 1:nrow(input)){
    min_t = ifelse(i - w < 1, 1, i - w)
    max_t = i
    
    Asset_1 <- input[min_t:max_t,1]
    Asset_2 <- input[min_t:max_t,2]

    var_1[i] <- round(var(Asset_1), 4)
    var_2[i] <- round(var(Asset_2), 4)
  }
  return(tibble(var_1, var_2))
}

# reading final_project.csv
final <- read.csv('final_project.csv')
# creating t variable, which is the minute
final <- final %>% mutate(t = X + 1) %>% dplyr::select(-X)

set.seed(816)
train_id <- sample(1:nrow(final), trunc(0.7*nrow(final)))

f_10 <- final %>% mutate(t = ifelse(t + 10 < nrow(final), t + 10, nrow(final))) %>% # h = 10
  left_join(final, by="t") %>%
  mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
         Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
         Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
  dplyr::select(Asset_1_BRet_10f, Asset_2_BRet_10f, Asset_3_BRet_10f) %>%
  mutate(t = 1:nrow(.)) %>%
  dplyr::select(t, everything())

horizons <- c(3,10,30,120,1440)

BRet <- sapply(horizons, backwards_return)
BRet[c(1:5)*4] <- NULL
BRet_full<- data.frame(matrix(nrow = length(BRet[[1]])))
BRet_full[, c("Asset_1_BRet_3", "Asset_2_BRet_3", "Asset_3_BRet_3",
              "Asset_1_BRet_10", "Asset_2_BRet_10", "Asset_3_BRet_10",
              "Asset_1_BRet_30", "Asset_2_BRet_30", "Asset_3_BRet_30",
              "Asset_1_BRet_120", "Asset_2_BRet_120", "Asset_3_BRet_120",
              "Asset_1_BRet_1440", "Asset_2_BRet_1440", "Asset_3_BRet_1440")] <- BRet
BRet_full[,1] <- NULL


BRet_full$Asset_1_BRet_10f <- f_10$Asset_1_BRet_10f

X = model.matrix(Asset_1_BRet_10f ~ .^2 - 1, BRet_full)
y <- BRet_full$Asset_1_BRet_10f

lasso.mod = glmnet(X[train_id,], y[train_id], alpha=1)
cv.mod = cv.glmnet(X[train_id,], y[train_id], alpha=1)

bestlam = cv.mod$lambda.min

c<-coef(lasso.mod,s=bestlam ,exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables != '(Intercept)']



# BRet_full[,c("var1_30b","var2_30b")] <- rolling_var(BRet_full[,6:9], 420)
# placeholder <- BRet_full[,c("var1_30b","var2_30b")]
BRet_full[,c("var1_30b","var2_30b")] <- NULL
horizons <- c(180,240,360,480,600,720,960,1200)
BRet <- sapply(horizons, backwards_return)
BRet[c(1:8)*4] <- NULL
BRet_full[, c("Asset_1_BRet_180", "Asset_2_BRet_180", "Asset_3_BRet_180",
              "Asset_1_BRet_240", "Asset_2_BRet_240", "Asset_3_BRet_240",
              "Asset_1_BRet_360", "Asset_2_BRet_360", "Asset_3_BRet_360",
              "Asset_1_BRet_480", "Asset_2_BRet_480", "Asset_3_BRet_480",
              "Asset_1_BRet_600", "Asset_2_BRet_600", "Asset_3_BRet_600",
              "Asset_1_BRet_720", "Asset_2_BRet_720", "Asset_3_BRet_720",
              "Asset_1_BRet_960", "Asset_2_BRet_960", "Asset_3_BRet_960",
              "Asset_1_BRet_1200", "Asset_2_BRet_1200", "Asset_3_BRet_1200")] <- BRet

outcome <- "Asset_1_BRet_10f"
variables[81] = "."

# our modeling effort, 
# fully parameterized!
form_lasso <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

lm_mod_adv <- lm(form_lasso, data = BRet_full[train_id,])
summary(lm_mod_adv)

cor(BRet_full[-train_id,]$Asset_1_BRet_10f, predict(lm_mod_adv, BRet_full[-train_id,]))

# when training: 
save(lm_mod_adv, file = "model.RData")
