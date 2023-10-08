###################################################
# Author: Éric Dias da Silva Rosso
# Course: Data Mining
# Federal University of Pampa - Bage - PPGCAP
#
# This script will create a neural classifier for Wine data set
#
###################################################

# Carregar bibliotecas necessárias
library(caret) # Para criação dos conjuntos de treino e teste
library(neuralnet) # Para construção de redes neurais

rm(list = ls())

setwd("~/Downloads")
# dataset source: https://www.kaggle.com/datasets/akhil0007/wine-data
wine <- read.csv("wine.data", header = FALSE)

# Definindo as novas colunas
colnames(wine) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline")

wine$Class = as.character(wine$Class)
wine$Class1 <- wine$Class==1
wine$Class2 <- wine$Class==2
wine$Class3 <- wine$Class==3
wine_scaling <- preProcess(as.data.frame(wine[,2:ncol(wine)]), method=c("center"))
wine[,2:ncol(wine)] <- predict(wine_scaling, as.data.frame(wine[,2:ncol(wine)]))

# create a partition (80% training 20% testing)
set.seed(1)
index <- createDataPartition(wine$Class, p=0.80, list=FALSE)

# select 20% of the data for testing
testset <- wine[-index,]

# select 80% of data to train the models
trainset <- wine[index,]
trainset= trainset[sample(1:nrow(trainset)), ]

# build the model with the training set
model <- neuralnet::neuralnet(Class1 + Class2 + Class3 ~ Alcohol + Malic_acid + Ash + Alcalinity_of_ash + Magnesium + Total_phenols + Flavanoids + Nonflavanoid_phenols + Proanthocyanins + Color_intensity + Hue + OD280_OD315_of_diluted_wines + Proline, data=trainset, hidden=c(6), stepmax=250000, threshold=0.01, lifesign = "full", act.fct='logistic', err.fct="ce", linear.output=FALSE)

# plot the model
plot(model)

# predictions on the training set
prediction_train <- predict(model, trainset[, c(-17,-16,-15,-1)])
predicted_train <- max.col(prediction_train)
confusionMatrix(as.factor(as.character(predicted_train)), as.factor(trainset$Class))

# predictions on the test set
prediction_test <- predict(model, testset[, c(-17,-16,-15,-1)])
predicted_test <- max.col(prediction_test)
x <- confusionMatrix(as.factor(predicted_test), as.factor(testset$Class))
x
heatmap(x$table, Rowv=NA, Colv=NA, symm=TRUE, revC=TRUE)

# Plotar os resultados de teste
plot(x$table, col = cm.colors(nrow(x$table)),
     main = paste("Matriz de confusão\nAcurácia =", round(x$overall['Accuracy'], 3)), 
     xlab = "Classe Predita", ylab = "Classe Verdadeira")
legend("right", legend = paste("Classe", unique(testset$Class)), col = as.numeric(unique(testset$Class))+1, 
       pch = 20, cex = 1, bg="lightgray")


trainset$Class <- factor(trainset$Class)



# Matriz Scatterplot
pairs(trainset[, c(8,11,14)], col = as.numeric(trainset$Class), pch = 16)

# Define as cores para cada classe
class_colors <- c("red", "green", "blue")

# Principal Component Analysis (PCA)
pca <- prcomp(trainset[, 2:14], scale. = TRUE)
pca_scores <- predict(pca, trainset[, 2:14])

# Cria o gráfico de dispersão com as cores por classe
plot(pca_scores[, 1:2], col = class_colors[trainset$Class], pch = as.numeric(trainset$Class),
     main = "Principal Component Analysis",
     xlab = "Componente Principal 1", ylab = "Componente Principal 2")

# Adiciona a legenda das classes
legend("bottomright", legend = levels(trainset$Class), fill = class_colors, bty = "n")



# Parallel Coordinates Plot
library(GGally)
trainset$Class <- as.factor(trainset$Class)
ggparcoord(trainset, columns = c(8,11,14), groupColumn = "Class", alphaLines = 0.5)

