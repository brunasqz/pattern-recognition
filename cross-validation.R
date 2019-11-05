rm(list = ls())
#library(unbalanced)
library(DMwR)
library(kernlab)
library(pROC)

#=======================================#
#   Inicialização dados e funções       #
#=======================================#
cross_validation <- function(xc1, y, kfolds, c, paramh) {
  
  auc1 <- c()
  
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
    
    
    ROC <- multiclass.roc(response = factor(ytest),  predictor = c(yhat), levels = c("rare", "common"))
    auc1[i] <- auc(ROC)
    
    #plot
    # a <- alpha(svmtrain)
    # ai <- SVindex(svmtrain)
    # nsvec <- nSV(svmtrain)
    # points(xc1[ai,1], xc1[ai,2], col="green")
  }
  
  return(auc1)
}

#Leitura dos dados
data <- read.csv("database.csv", sep= ",")

#=======================================#
#       Classificação balanceando       #
#=======================================#
#SMOTE
data$X0 <- factor(ifelse(data$X0 == 1, "rare", "common"))
data_smoted <- SMOTE(X0 ~ ., data, perc.over = 100, k = 5, leaner = NULL)


auc <- cross_validation(as.matrix(data_smoted[,1:8]), as.matrix(data_smoted[,9]),
                          kfolds = 10, c = 0.5, paramh = 2)
