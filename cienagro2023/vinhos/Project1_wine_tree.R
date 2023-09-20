###################################################
# Author: Éric Dias da Silva Rosso
# Course: Data Mining
# Federal University of Pampa - Bage - PPGCAP
#
# This script will create a classifier for Wine data set
#
###################################################

# Carregar bibliotecas necessárias
library(caret) # Para criação dos conjuntos de treino e teste
library(rpart) # Para construção de árvores de decisão
library(rpart.plot) #Para visualizar a arvore de decisão

rm(list = ls())

wine <- read.csv("wine.data", header = FALSE)

# Definindo as novas colunas
colnames(wine) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline")

# # Converter a variável de classe para fator e criar variáveis binárias para cada classe (Caso feito, acurácia sobe pra 1)
# wine$Class <- as.factor(wine$Class)
# wine$Class1 <- ifelse(wine$Class == 1, 1, 0)
# wine$Class2 <- ifelse(wine$Class == 2, 1, 0)
# wine$Class3 <- ifelse(wine$Class == 3, 1, 0)

# Normalizar os dados
wine_scaling <- preProcess(as.data.frame(wine[,2:ncol(wine)]), method=c("center", "scale"))
wine[,2:ncol(wine)] <- predict(wine_scaling, as.data.frame(wine[,2:ncol(wine)]))

# Criar conjunto de treino e teste
set.seed(1)
training.samples <- createDataPartition(wine$Class, p = 0.8, list = FALSE)
trainset <- wine[training.samples, ]
testset <- wine[-training.samples, ]

# Treinar uma árvore de decisão
model.rpart <- rpart(Class ~ ., data = trainset, method = "class")
pred.rpart <- predict(model.rpart, newdata = testset, type = "class")

# Calcular a acurácia da árvore de decisão
accuracy.rpart <- mean(pred.rpart == testset$Class)
cat("A acurácia da árvore de decisão é:", round(accuracy.rpart, 3), "\n")

# Visualizando a arvore de decisão
rpart.plot(model.rpart, box.palette = list("red","green","blue"), type=0, extra=104)

# plotando o gráfico de dispersão
plot(trainset$Flavanoids, trainset$Color_intensity, xlab="Flavanoids", ylab="Color intensity", pch=as.numeric(trainset$Class)+1, col=as.numeric(trainset$Class)+1, main="Train Set")

# Adicionando retângulos coloridos com base nas classes da árvore de decisão
rect(-10, -10, 10, -0.6893412, col=rgb(0, 1, 0, 0.2), lty=2) # Classe 1
rect(-10, -0.6893412, -0.449783, 10, col=rgb(0, 0, 1, 0.2), lty=2) # Classe 2
rect(-0.449783, -0.6893412, 10, 10, col=rgb(1, 0, 0, 0.2), lty=0) # Classe 3

# Adicionando uma legenda com as classes da árvore de decisão
legend("topright", legend=levels(trainset$Class), pch=unique(as.numeric(trainset$Class))+1, col=unique(as.numeric(trainset$Class))+1, bg="white")

# Obter os níveis de fator para as previsões e as classes verdadeiras
pred.levels <- levels(factor(pred.rpart))
testset.levels <- levels(factor(testset$Class))

# Obter a união dos níveis de fator
all.levels <- unique(c(pred.levels, testset.levels))

# Converter as previsões e as classes verdadeiras em fatores com os mesmos níveis
pred.rpart <- factor(pred.rpart, levels = all.levels)
testset$Class <- factor(testset$Class, levels = all.levels)

# Calcular a matriz de confusão
cm.rpart <- confusionMatrix(pred.rpart, testset$Class)

# Visualizar a matriz de confusão
cm.rpart$table

# Acurácia
cm.rpart$overall['Accuracy']

# Plotar os resultados de teste
plot(cm.rpart$table, col = cm.colors(length(all.levels)), 
     main = paste("Matriz de confusão\nAcurácia =", round(cm.rpart$overall['Accuracy'], 3)), 
     xlab = "Classe Predita", ylab = "Classe Verdadeira")
legend("topright", legend = paste("Classe", all.levels), col = cm.colors(length(all.levels)), 
       pch = 20, cex = 1)


