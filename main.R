rm(list = ls())
library(DMwR)
library(kernlab)
library(pROC)
source('cross-validation.R')

#=======================================#
#   Inicialização dados e funções       #
#=======================================#
data <- read.csv("database.csv", sep= ",")
data$X0 <- factor(ifelse(data$X0 == 1, "rare", "common"))

amostra_classes <- matrix(nrow = 2, ncol=2, 0)
rownames(amostra_classes) <- c("Comum", "Raro")
colnames(amostra_classes) <- c("Desbalanceada", "Balanceada")

amostra_classes[1,1] <- length(which(data$X0 == "common")) 
amostra_classes[2,1] <- length(which(data$X0 == "rare"))


#=======================================#
#    Classificação desbalanceando       #
#=======================================#
result_raw<- cross_validation(as.matrix(data[,1:8]), as.matrix(data[,9]),
                                 kfolds = 10, c = 0.5, paramh = 2)


# resultados da AUC de cada fold
for(i in 1:10){
  print( paste("Fold:", i, ": AUC =", format(round(result_raw[['auc-vector']][i], 4), nsmall = 4),"" ))
}


#=======================================#
#       Classificação balanceando       #
#=======================================#
# SMOTE
data_smoted <- SMOTE(X0 ~ ., data, perc.over = 100, k = 5, leaner = NULL)
amostra_classes[1,2] <- length(which(data_smoted$X0 == "common")) 
amostra_classes[2,2] <- length(which(data_smoted$X0 == "rare"))

result_smote <- cross_validation(as.matrix(data_smoted[,1:8]), as.matrix(data_smoted[,9]),
                          kfolds = 10, c = 0.5, paramh = 2)

# resultados da AUC de cada fold
for(i in 1:10){
  print( paste("Fold:", i, ": AUC =", format(round(result_smote[['auc-vector']][i], 4), nsmall = 4),"" ))
}


#=======================================#
#       Comparacao de resultados        #
#=======================================#

# Quantidade de amostras em cada um dos experimentos
print(amostra_classes)


# Escolhe o melhor fold para plotar resultados:
f_raw <- which(result_raw[['auc-vector']] == max(result_raw[['auc-vector']]))
f_smote <- which(result_smote[['auc-vector']] == max(result_smote[['auc-vector']]))


legenda <- c()
legenda[1] <-paste("SVM com dados desbalanceados - fold:", f_raw, "(AUC =", 
              format(round(result_raw[['auc-vector']][f_raw], 4), nsmall = 4),")" )
legenda[2] <-paste("SVM com SMOTE para balanceamento - fold:", f_smote, "(AUC =", 
              format(round(result_smote[['auc-vector']][f_smote], 4), nsmall = 4),")" )

 
# Salva um arquivo com a comparacao entre curvas ROC
png(file="comparacao_curvas_ROC.jpeg")

plot.roc( result_smote[['ROC-element']][[f_smote]][['rocs']][[1]]
          ,main="Comparação entre curvas ROC", lwd =2
          ,xlab = "Taxa de falso positivo"
          ,ylab = "Taxa de verdadeiro positivo"
          ,col = 'blue'
          )
par (new = T)
plot.roc( result_raw[['ROC-element']][[f_raw]][['rocs']][[1]]
          ,main="Comparação entre curvas ROC", lwd =2
          ,xlab = "Taxa de falso positivo"
          ,ylab = "Taxa de verdadeiro positivo"
          ,col = 'red'
)

legend("bottomright", legend = legenda,
       col=c('red','blue'), lty=1, cex=0.8,
       title="Resultados", text.font=4)

dev.off()