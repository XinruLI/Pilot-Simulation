#  Use overall effect size as prediction
#  The squared residual are weighted by the accuracy (1/vi)
xvalid4 <- function(node, y, vi, tau2, n.fold = 10){
  # A function that esimate cross-validation error based on random effects
  # meta-analysis model
  #
  # node: the vector to specify the nodes that the studies belong to
  # y: the vector of study effect sizes
  # vi: the vector of study accuracies (sampling variances)
  # tau2: the residual heterogeneity esimated in the meta-regression tree
  # n.fold: the number of fold
  #
  # Returns: a vector of estimate of cross-validation error and the se
  wts <- 1/vi
  wy <- y/vi
  samp <- data.frame(y, vi, node, wts, wy)
  N <- nrow(samp)
  inx <- sample(1:N)
  # To devide the dataset into k folds
  inx.xvalid <- c(round(seq(from = 1, by= round(N/n.fold), length.out = n.fold)),N+1)
  pred <- rep(NA, N)
  for (i in 1:n.fold){
    inx.test <- inx[inx.xvalid[i]:(inx.xvalid[i+1]-1)]
    print(inx.test)
    test <- samp[inx.test, ]
    train <- samp[-inx.test, ]
    mean.wt <- sum(train$wy)/sum(wts)
    if (length(unique(node)) == 1) {
     test.pred <- rep(mean.wt, length(inx.test))
    }  else {
      train.pred <- tapply(train$wy, train$node, sum)/tapply(train$wts, train$node, sum)
      test.pred <- train.pred[as.character(test$node)]
      inx.na <- is.na(test.pred)
      test.pred[inx.na] <- mean.wt
    }
    pred[inx.test] <- test.pred
  }
  x.error <- sum((y-pred)^2)/sum((y-sum(wy)/sumwts)^2)
  sdx.error <- sqrt(sum(((y-pred)^2 - mean((y-pred)^2))^2))/sum((y-sum(my)/sum(wts))^2)   
    #sd((y-pred)^2) #/sum((y-mean.wt)^2) 
  return(c(x.error, sdx.error))
}


