rm(list = ls())
library(unbalanced)

#Leitura dos dados
data <- read.csv("database.csv", sep= ",")
data <- as.matrix(data)

x <- data[,1:8]
y <- data[,9]

balanced-data <- ubOSS(x, y)

