prediction <- function(input) {
  load(file="model.Rdata")
  horizons <- c(3,10,30,120,180,240,360,480,600,720,960,1200,1440)
  results <- list()
  for(h in horizons){
    t = ifelse(1440 - h < 1, 1, 1440-h)
    results<-append(results,(input$Asset_1[1440] - input$Asset_1[t])/input$Asset_1[t])
    results<-append(results,(input$Asset_2[1440] - input$Asset_2[t])/input$Asset_2[t])
    results<-append(results,(input$Asset_3[1440] - input$Asset_3[t])/input$Asset_3[t])
  }
  names(results) <-c("Asset_1_BRet_3", "Asset_2_BRet_3", "Asset_3_BRet_3",
                    "Asset_1_BRet_10", "Asset_2_BRet_10", "Asset_3_BRet_10",
                    "Asset_1_BRet_30", "Asset_2_BRet_30", "Asset_3_BRet_30",
                    "Asset_1_BRet_120", "Asset_2_BRet_120", "Asset_3_BRet_120",
                    "Asset_1_BRet_180", "Asset_2_BRet_180", "Asset_3_BRet_180",
                    "Asset_1_BRet_240", "Asset_2_BRet_240", "Asset_3_BRet_240",
                    "Asset_1_BRet_360", "Asset_2_BRet_360", "Asset_3_BRet_360",
                    "Asset_1_BRet_480", "Asset_2_BRet_480", "Asset_3_BRet_480",
                    "Asset_1_BRet_600", "Asset_2_BRet_600", "Asset_3_BRet_600",
                    "Asset_1_BRet_720", "Asset_2_BRet_720", "Asset_3_BRet_720",
                    "Asset_1_BRet_960", "Asset_2_BRet_960", "Asset_3_BRet_960",
                    "Asset_1_BRet_1200", "Asset_2_BRet_1200", "Asset_3_BRet_1200",
                    "Asset_1_BRet_1440", "Asset_2_BRet_1440", "Asset_3_BRet_1440")
  # BRet_2_5[c(1:11)*4] <- NULL
  # BRet_full[, c("Asset_1_BRet_3", "Asset_2_BRet_3", "Asset_3_BRet_3",
  #               "Asset_1_BRet_10", "Asset_2_BRet_10", "Asset_3_BRet_10",
  #               "Asset_1_BRet_30", "Asset_2_BRet_30", "Asset_3_BRet_30",
  #               "Asset_1_BRet_120", "Asset_2_BRet_120", "Asset_3_BRet_120", 
  #               "Asset_1_BRet_180", "Asset_2_BRet_180", "Asset_3_BRet_180",
  #               "Asset_1_BRet_240", "Asset_2_BRet_240", "Asset_3_BRet_240",
  #               "Asset_1_BRet_360", "Asset_2_BRet_360", "Asset_3_BRet_360", 
  #               "Asset_1_BRet_480", "Asset_2_BRet_480", "Asset_3_BRet_480",
  #               "Asset_1_BRet_600", "Asset_2_BRet_600", "Asset_3_BRet_600",
  #               "Asset_1_BRet_720", "Asset_2_BRet_720", "Asset_3_BRet_720",
  #               "Asset_1_BRet_960", "Asset_2_BRet_960", "Asset_3_BRet_960",
  #               "Asset_1_BRet_1200", "Asset_2_BRet_1200", "Asset_3_BRet_1200",
  #               "Asset_1_BRet_1440", "Asset_2_BRet_1440", "Asset_3_BRet_1440")]
  BRet_full[,1] <- NULL
  predict(lm_mod_adv, BRet_full[-train_id,])
}