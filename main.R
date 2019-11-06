rm(list = ls())
#library(unbalanced)
library(DMwR)
library(kernlab)
library(pROC)
library(rfUtilities)

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
data_smoted <- SMOTE(X0 ~ ., data, perc.over = 100, k = 5, leaner = NULL)

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
svmtrain <- ksvm(as.matrix(rbind(xctrain[,1:8],xrtrain[,1:8])), 
                 c(xctrain[,9],xrtrain[,9]), 
                 type='C-bsvc', kernel='rbfdot', kpar=(list(2)), C= 0.5)

yhat <- predict(svmtrain,
                as.matrix(rbind(xctest[,1:8],xrtest[,1:8])),
                type="response")

#Curva ROC
ROC <- multiclass.roc(c(xctest[,9],xrtest[,9]), yhat)
AUC <- auc(ROC)

rs_balanceada <- ROC[['rocs']]
plot.roc(rs_balanceada[[1]])



# Calculo de acuracia e outras estatisticas
result_balanceado <- accuracy(yhat, c(xctest[,9],xrtest[,9]))


#=======================================#
#    Classificação desbalanceando       #
#=======================================#

data <- read.csv("database.csv", sep= ",")
ic0 <- which(data$X0 == 0)
ic1 <- which(data$X0 == 1)

x_com <- data[ic0,]
x_rare <- data[ic1,]

traintest <- treinaTeste(0.3, x_com)
xctrain <- traintest[[2]]
xctest <- traintest[[1]]

traintest2 <- treinaTeste(0.3, x_rare)
xrtrain <- traintest2[[2]]
xrtest  <- traintest2[[1]]


#Aplicação do SVM
svmtrain <- ksvm(as.matrix(rbind(xctrain[,1:8],xrtrain[,1:8])), 
                 c(xctrain[,9],xrtrain[,9]), 
                 type='C-bsvc', kernel='rbfdot', kpar=(list(2)), C= 0.5)

yhat <- predict(svmtrain,
                as.matrix(rbind(xctest[,1:8],xrtest[,1:8])),
                type="response")

#Curva ROC
ROC <- multiclass.roc(c(xctest[,9],xrtest[,9]), yhat)
AUC <- auc(ROC)

rs_desbalanceada <- ROC[['rocs']]
plot.roc(rs_desbalanceada[[1]])



# Calculo de acuracia e outras estatisticas
result_desbalanceado <- accuracy(yhat, c(xctest[,9],xrtest[,9]))


#=====================================================#
#    COmparação entre balanceado e desbalanceado      #
#=====================================================#


#Plot de curvas ROC
plot.roc(rs_desbalanceada[[1]], col = 'black', main="Comparação entre métodos", lty=2)
par(new=T)
plot.roc(rs_balanceada[[1]], col = 'blue')
legend("bottomright", inset = .02, legend=c("SMOOT e SVM", "SVM"),
       col=c("blue", "black"), lty=1:2, cex=0.8,
       title="Métodos", text.font=4)




