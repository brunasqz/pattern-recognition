rm(list = ls())
#library(unbalanced)
library(DMwR)
library(kernlab)
library(pROC)

#=======================================#
#   Inicialização dados e funções       #
#=======================================#
#Função de separação para treinamento e teste
treinaTeste <- function(train,xc){
  tam <- length(xc[,1])
  Ntrain <- train*tam
  
  seqc <- sample(tam)
  
  xctrain <- xc[seqc[(1:Ntrain)],]
  xctest  <- xc[seqc[(Ntrain+1):tam],]
  
  return(list(
    xctrain,
    xctest
  ))
}

#Leitura dos dados
data <- read.csv("database.csv", sep= ",")

#=======================================#
#       Classificação balanceando       #
#=======================================#
#SMOTE
data$X0 <- factor(ifelse(data$X0 == 1, "rare", "common"))
data_smoted <- SMOTE(X0 ~ ., data, perc.over = 300, k = 5, leaner = NULL)

#Separação dos dados 
ic0 <- which(data_smoted$X0 == "common")
ic1 <- which(data_smoted$X0 == "rare")

x_com <- data_smoted[ic0,]
x_rare <- data_smoted[ic1,]

traintest <- treinaTeste(0.3, x_com)
xctrain <- traintest[[2]]
xctest <- traintest[[1]]

traintest2 <- treinaTeste(0.3, x_rare)
xrtrain <- traintest2[[2]]
xrtest  <- traintest2[[1]]


#Aplicação do SVM
svmtrain <- ksvm(rbind(xctrain[,1:8],xrtrain[,1:8]), 
                 c(xctrain[,9],xrtrain[,9]), 
                 type='C-bsvc', kernel='rbfdot', kpar=(list(2)), C= 5)

yhat <- predict(svmtrain,
                rbind(xctest[,1:8],xrtest[,1:8]),
                type="response")

#Curva ROC
ROC <- multiclass.roc(rbind(xctest[,1:8],xrtest[,1:8]), yhat)
AUC <- auc(ROC)

#=======================================#
#    Classificação desbalanceando       #
#=======================================#
