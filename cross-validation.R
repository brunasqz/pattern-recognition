rm(list = ls())
library(DMwR)
library(kernlab)
library(pROC)

#=======================================#
#   Funcao Cross-validation             #
#=======================================#
cross_validation <- function(xc1, y, kfolds, c, paramh) {
  
  auc1 <- c()
  ROC <- list()
  
  seqc1 <- sample(length(xc1[,1]))
  
  group1 <- seq(1, length(xc1[,1]),trunc(length(xc1[,1])%/%kfolds))
  
  for(i in seq(1, kfolds, 1)) {
    
    #Separação dos folds
    if (i == 1) {
      xc1test  <- xc1[seqc1[1:group1[i+1]],]
      xc1train <- xc1[seqc1[(group1[i+1]+1):length(xc1[,1])],]
      
      ytest <- y[seqc1[1:group1[i+1]],]
      ytrain <- y[seqc1[(group1[i+1]+1):length(xc1[,1])],]
      
    } else if(i == 10) {
      xc1test  <- xc1[seqc1[(group1[i]+1):length(xc1[,1])],]
      xc1train <- xc1[seqc1[1:group1[i]],]
      
      ytest <- y[seqc1[(group1[i]+1):length(xc1[,1])],]
      ytrain <- y[seqc1[1:group1[i]],]
    } else {
      xc1test  <- xc1[seqc1[(group1[i]+1):group1[i+1]],]
      xc1train <- rbind(
        xc1[seqc1[1:group1[i]],],
        xc1[seqc1[(group1[i+1]+1):length(xc1[,1])],]
      )
      
      ytest  <- y[seqc1[(group1[i]+1):group1[i+1]],]
      ytrain <- c(
        y[seqc1[1:group1[i]],],
        y[seqc1[(group1[i+1]+1):length(xc1[,1])],]
      )
      
    }
    svmtrain <- ksvm(as.matrix(xc1train), ytrain, type='C-bsvc', kernel='rbfdot', kpar=(list(paramh)), C= c)
    
    yhat <- predict(svmtrain,xc1test,type="response")
    
    
    ROC[[i]] <- multiclass.roc(response = factor(ytest),  predictor = c(yhat), levels = c("rare", "common"))
    auc1[i] <- auc(ROC[[i]])
  }
  
  result <- list(auc1, ROC)
  names(result) <- c("auc-vector","ROC-element" )
  return(result)
}