tree.trial <- tree(Asset_1_BRet_10f ~ Asset_3_BRet_10f + Asset_2_BRet_10f + Asset_1.x + Asset_3.x + Asset_2.x, update)
cv.trial <- cv.tree(tree.trial)
tree.trial.final <- prune.tree(tree.trial, best=cv.trial$size[which.min(cv.trial$dev)])
a <- 1
save(a, file = 'model.RData')
