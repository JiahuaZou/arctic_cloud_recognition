library(e1071)
library(ggplot2)
library(corrplot)
library(dplyr)
library(MASS)
library(RColorBrewer)
library(caret)
library(stringr)
library(class)
library(cluster)
library(factoextra)
library(heplots)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(gridExtra)

loss <- function(label, label_hat){
  # loss function to compute error rate.
  ls <- mean(label != label_hat)
  return(ls)
}

#' @title CV generic function
#' @description aggregate classification methods including GDA, Logistic, random forest, knn and classification tree
#' @param classifier the classification method
#' @param trainFeature the features in train data set
#' @param trainLabel the label in train data set
#' @param K the parameter for k-fold CV
#' @param lossfn the loss function defined to compute error/accuracy
#' @return the combination of k successes can occur in n trials
CVgeneric <- function(classifier, trainFeature, trainLabel, K = 8, lossfn){
  
  set.seed(154)
  num_blocks <- unique(trainFeature$blocks)
  folds <- createFolds(num_blocks, K)
  error_df <- matrix(0, nrow = K, ncol = 1)
  rownames(error_df) <- 1:K
  
  if(!(str_to_lower(classifier) %in% c("lda", "qda", "logistic", "randomforest", "knn", "tree"))){
    stop("classifier must be one of lda, qda, logistics, randomforest, KNN or tree.")
  }else if(str_to_lower(classifier) == "logistic"){
    for(i in 1:K){
      model <- glm(((trainLabel$label[trainLabel$blocks %notin% folds[[i]]] + 1)/2) ~ 
                     NDAI + CORR + SD, 
                   data = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3],
                   family = binomial(link = "logit"))
      yhat <- predict(model, trainFeature[trainFeature$blocks %in% folds[[i]], 1:3], type = "response")
      class <- yhat > 0.5
      error_df[i,1] <- lossfn(as.numeric(class),
                              ((trainLabel$label[trainLabel$blocks %in% folds[[i]]] + 1)/2))
    }
    return(error_df)
    
  }else if(str_to_lower(classifier) == "qda"){
    for(i in 1:K){
      model <- qda(trainLabel$label[trainLabel$blocks %notin% folds[[i]]] ~ NDAI + CORR + SD,
                   data = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3])
      pred_qda <- predict(model, trainFeature[trainFeature$blocks %in% folds[[i]], 1:3])$class
      error_df[i,1] <- lossfn(pred_qda, trainLabel$label[trainLabel$blocks %in% folds[[i]]])
    }
    return(error_df)
    
  }else if(str_to_lower(classifier) == "lda"){
    for(i in 1:K){
      model <- lda(trainLabel$label[trainLabel$blocks %notin% folds[[i]]] ~ NDAI + CORR + SD, data = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3])
      pred_lda <- predict(model, trainFeature[trainFeature$blocks %in% folds[[i]], 1:3])$class
      error_df[i,1] <- lossfn(pred_lda, trainLabel$label[trainLabel$blocks %in% folds[[i]]])
    }
    return(error_df)
    
  }else if(str_to_lower(classifier) == "knn"){
    for(i in 1:K){
      model <- knn(train = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3], 
                   test = trainFeature[trainFeature$blocks %in% folds[[i]], 1:3],
                   cl = trainLabel$label[trainLabel$blocks %notin% folds[[i]]],
                   k = 50, prob = TRUE)
      error_df[i,1] <- lossfn(model, trainLabel$label[trainLabel$blocks %in% folds[[i]]])
    }
    return(error_df)
    
  }else if(str_to_lower(classifier) == "tree") {
    for (i in 1:K) {
      model <- rpart(factor(trainLabel$label[trainLabel$blocks %notin% folds[[i]]]) ~ NDAI + CORR + SD, 
                     data = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3])
      pred_tree <- predict(model, trainFeature[trainFeature$blocks %in% folds[[i]], 1:3], type = "class")
      yhat <- as.numeric(as.character(pred_tree))
      error_df[i,1] <- lossfn(yhat, trainLabel$label[trainLabel$blocks %in% folds[[i]]])
    }
    return(error_df)
    
  }else if(str_to_lower(classifier) == "randomforest") {
    for (i in 1:K) {
      model <- randomForest(factor(trainLabel$label[trainLabel$blocks %notin% folds[[i]]]) ~NDAI + CORR + SD,data = trainFeature[trainFeature$blocks %notin% folds[[i]], 1:3])
      pred_rf <- predict(model, trainFeature[trainFeature$blocks %in% folds[[i]], 1:3],
                         type = "class")
      yhat <- as.numeric(as.character(pred_rf))
      error_df[i,1] <- lossfn(yhat, trainLabel$label[trainLabel$blocks %in% folds[[i]]])
    }
    return(error_df)
  }
}

