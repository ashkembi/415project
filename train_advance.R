# save(a, file = 'model.RData')
BRet_full <- read.csv('bret.csv')


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

forward_10 <- function(input_period) {
  f_10 <- input_period %>% mutate(t = ifelse(t + 10 < nrow(input_period), t + 10, nrow(input_period))) %>% # h = 10
    left_join(input_period, by="t") %>%
    mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
           Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
           Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
    dplyr::select(Asset_1_BRet_10f, Asset_2_BRet_10f, Asset_3_BRet_10f) %>%
    mutate(t = 1:nrow(.)) %>%
    dplyr::select(t, everything())
  
  return(f_10)
}



Rho <- BRet_full %>%
  mutate(t = 1:nrow(.)) %>%
  dplyr::select(t, Asset_1_BRet_3, Asset_2_BRet_3, Asset_3_BRet_3)

rolling_corr <- function(input, w) {
  Rho_1_2 <- vector(length = nrow(input))
  Rho_1_3 <- vector(length = nrow(input))
  Rho_2_3 <- vector(length = nrow(input))
  
  for(i in 1:nrow(input)){
    min_t = ifelse(i - w < 1, 1, i - w)
    max_t = i
    
    Asset_1 <- input[min_t:max_t,1]
    Asset_2 <- input[min_t:max_t,2]
    Asset_3 <- input[min_t:max_t,3]
    
    Rho_1_2[i] <- round(cor(Asset_1, Asset_2), 4)
    Rho_1_3[i] <- round(cor(Asset_1, Asset_3), 4)
    Rho_2_3[i] <- round(cor(Asset_2, Asset_3), 4)
  }
  return(tibble(Rho_1_2, Rho_1_3, Rho_2_3))
}

forward_10 <- function(input_period) {
  f_10 <- input_period %>% mutate(t = ifelse(t + 10 < nrow(input_period), t + 10, nrow(input_period))) %>% # h = 10
    left_join(input_period, by="t") %>%
    mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
           Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
           Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
    dplyr::select(Asset_1_BRet_10f, Asset_2_BRet_10f, Asset_3_BRet_10f) %>%
    mutate(t = 1:nrow(.)) %>%
    dplyr::select(t, everything())
  
  return(f_10)
}



Rho <- BRet_full %>%
  mutate(t = 1:nrow(.)) %>%
  dplyr::select(t, Asset_1_BRet_3, Asset_2_BRet_3, Asset_3_BRet_3)

rolling_var <- function(input, w) {
  var_1 <- vector(length = nrow(input))
  var_2 <- vector(length = nrow(input))
  
  for(i in 1:nrow(input)){
    min_t = ifelse(i - w < 1, 1, i - w)
    max_t = i
    
    Asset_1 <- input[min_t:max_t,1]
    Asset_2 <- input[min_t:max_t,2]
    Asset_3 <- input[min_t:max_t,3]
    
    var_1[i] <- round(var(Asset_1), 4)
    var_2[i] <- round(var(Asset_2), 4)
  }
  return(tibble(var_1, var_2))
}

# horizons <- c(120,180,240,360,480,600,720,960,1200,1440)
# 
# BRet_2_5 <- sapply(horizons, backwards_return)
# BRet_2_5[c(1:11)*4] <- NULL
# BRet_full[, c("Asset_1_BRet_60", "Asset_2_BRet_60", "Asset_3_BRet_60",
#               "Asset_1_BRet_120", "Asset_2_BRet_120", "Asset_3_BRet_120", 
#               "Asset_1_BRet_180", "Asset_2_BRet_180", "Asset_3_BRet_180",
#               "Asset_1_BRet_240", "Asset_2_BRet_240", "Asset_3_BRet_240",
#               "Asset_1_BRet_360", "Asset_2_BRet_360", "Asset_3_BRet_360", 
#               "Asset_1_BRet_480", "Asset_2_BRet_480", "Asset_3_BRet_480",
#               "Asset_1_BRet_600", "Asset_2_BRet_600", "Asset_3_BRet_600",
#               "Asset_1_BRet_720", "Asset_2_BRet_720", "Asset_3_BRet_720",
#               "Asset_1_BRet_960", "Asset_2_BRet_960", "Asset_3_BRet_960",
#               "Asset_1_BRet_1200", "Asset_2_BRet_1200", "Asset_3_BRet_1200",
#               "Asset_1_BRet_1440", "Asset_2_BRet_1440", "Asset_3_BRet_1440")] <- BRet_2_5
# 
# 
# Rho_corr <- read.csv('corr.csv')

horizons <- c(120,1440)

BRet_more <- sapply(horizons, backwards_return)
BRet_more[c(1:2)*4] <- NULL
BRet_full[, c("Asset_1_BRet_120", "Asset_2_BRet_120", "Asset_3_BRet_120",
              "Asset_1_BRet_1440", "Asset_2_BRet_1440", "Asset_3_BRet_1440")] <- BRet_more

BRet_full[,c("var1_30b","var2_30b","var3_30b")] <- rolling_var(BRet_full[,6:9], 420)
# BRet_full[,c("Rho_1_2_10b","Rho_1_3_10b","Rho_2_3_10b")] <- rolling_corr(BRet_full[,4:6], 1440)
# BRet_full[,c("Rho_1_2_60b","Rho_1_3_60b","Rho_2_3_60b")] <- rolling_corr(BRet_full[,c("Asset_1_BRet_60", "Asset_2_BRet_60", "Asset_3_BRet_60")], 1440)
# # BRet_full[,c("Rho_1_2_600b","Rho_1_3_600b","Rho_2_3_600b")] <- rolling_corr(BRet_full[,10:12], 1440)
# BRet_full[,c("Rho_1_2_1440b","Rho_1_3_1440b","Rho_2_3_1440b")] <- rolling_corr(BRet_full[,40:42], 1440)

bret_adv <- BRet_full %>% mutate(Asset_1_BRet_10f = f_10$Asset_1_BRet_10f)


set.seed(816)
train_id <- sample(1:nrow(bret_adv), trunc(0.7*nrow(bret_adv)))

outcome <- "Asset_1_BRet_10f"
variables[81] = "."

# our modeling effort, 
# fully parameterized!
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

mod_adv_lm_full <- lm(f, data = bret_adv[train_id,])
summary(mod_adv_lm_full)

# in-sample correlation
cor(bret_adv[train_id,]$Asset_1_BRet_10f, predict(mod_adv_lm_full, bret_adv[train_id,]))

# out-of-sample correlation
cor(bret_adv[-train_id,]$Asset_1_BRet_10f, predict(mod_adv_lm_full, bret_adv[-train_id,]))

X = model.matrix(Asset_1_BRet_10f ~ .^2 - 1, bret_adv)
y <- bret_adv$Asset_1_BRet_10f
lasso.mod.basic.2 = glmnet(X[train_id,], y[train_id], alpha=1)


lasso.mod = cv.glmnet(X[train_id,], y[train_id], alpha=1)
summary(lasso.mod)

bestlam = lasso.mod$lambda.min
lasso.pred_test = predict(lasso.mod.basic.2,s=bestlam,newx=X[-train_id,])

# lasso.pred_test = predict(lasso.mod,newx=X[-train_id,])

val_MSEs_lasso <- lapply(1:86, function(i) mean((lasso.pred_test[,i]-y[-train_id])^2))

best_lambda_lasso = lasso.mod$lambda[which.min(val_MSEs_lasso)]

lasso.coef = predict(lasso.mod.basic.2, type="coefficients", s=bestlam)
lasso.coef

c<-coef(lasso.mod.basic.2,s=bestlam ,exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables != '(Intercept)']

# in-sample correlation
cor(y[train_id], predict(lasso.mod, s=best_lambda_lasso, newx=X[train_id,]))

# out-of-sample correlation
cor(y[-train_id], lasso.pred_test)

horizons <- c(60,180,240,360,480,600,720,960,1200)

BRet_2_5 <- sapply(horizons, backwards_return)
BRet_2_5[c(1:9)*4] <- NULL
BRet_full[, c("Asset_1_BRet_60", "Asset_2_BRet_60", "Asset_3_BRet_60",
              "Asset_1_BRet_180", "Asset_2_BRet_180", "Asset_3_BRet_180",
              "Asset_1_BRet_240", "Asset_2_BRet_240", "Asset_3_BRet_240",
              "Asset_1_BRet_360", "Asset_2_BRet_360", "Asset_3_BRet_360",
              "Asset_1_BRet_480", "Asset_2_BRet_480", "Asset_3_BRet_480",
              "Asset_1_BRet_600", "Asset_2_BRet_600", "Asset_3_BRet_600",
              "Asset_1_BRet_720", "Asset_2_BRet_720", "Asset_3_BRet_720",
              "Asset_1_BRet_960", "Asset_2_BRet_960", "Asset_3_BRet_960",
              "Asset_1_BRet_1200", "Asset_2_BRet_1200", "Asset_3_BRet_1200")] <- BRet_2_5
