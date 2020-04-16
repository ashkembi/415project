prediction <- function(dataframe) {
  update <- dataframe %>%
    mutate(t = 1:nrow(dataframe)) %>%
    as.tibble %>%
    mutate(t = ifelse(t + 10 < nrow(dataframe), t + 10, nrow(dataframe))) %>% # h = 10
    left_join(mutate(dataframe, t = 1:nrow(dataframe)), by="t") %>%
    mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
           Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
           Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
    dplyr::select(-Asset_1.y, -Asset_2.y, -Asset_3.y) %>%
    mutate(t = 1:nrow(dataframe)) %>%
    mutate(t = ifelse(t - 10 < 1, 1, t-10)) %>%
    rename("Asset_1" = Asset_1.x, "Asset_2" = Asset_2.x, "Asset_3" = Asset_3.x) %>%
    left_join(mutate(dataframe, t = 1:nrow(dataframe)), by="t") %>%
    mutate(Asset_1_BRet_10b = round((Asset_1.x - Asset_1.y)/Asset_1.y, 4),
           Asset_2_BRet_10b = round((Asset_2.x - Asset_2.y)/Asset_2.y, 4),
           Asset_3_BRet_10b = round((Asset_3.x - Asset_3.y)/Asset_3.y, 4)) %>%
    dplyr::select(-Asset_1.y, -Asset_2.y, -Asset_3.y, -t) %>%
    as.tibble()
  
  tree.trial <- tree(Asset_1_BRet_10f ~ Asset_3_BRet_10f + Asset_2_BRet_10f + Asset_1.x + Asset_3.x + Asset_2.x, update)
  cv.trial <- cv.tree(tree.trial)
  tree.trial.final <- prune.tree(tree.trial, best=cv.trial$size[which.min(cv.trial$dev)])
  
  predict(tree.trial.final)[length(predict(tree.trial.final))] %>% as.vector
}

