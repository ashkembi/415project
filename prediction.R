prediction <- function(df) {
  update <- df %>%
    mutate(t = 1:nrow(df)) %>%
    as_tibble %>%
    mutate(t = ifelse(t + 10 < nrow(df), t + 10, nrow(df))) %>% # h = 10
    left_join(mutate(df, t = 1:nrow(df)), by="t") %>%
    mutate(Asset_1_BRet_10f = round((Asset_1.y - Asset_1.x)/Asset_1.x, 4),
           Asset_2_BRet_10f = round((Asset_2.y - Asset_2.x)/Asset_2.x, 4),
           Asset_3_BRet_10f = round((Asset_3.y - Asset_3.x)/Asset_3.x, 4)) %>%
    dplyr::select(-Asset_1.y, -Asset_2.y, -Asset_3.y, -t) %>%
    as_tibble() %>%
    return()
  
  load(file = "model.RData")
  
  predict(tree.trial.final.290, update)[length(predict(tree.trial.final.290))] %>% as.vector
}

