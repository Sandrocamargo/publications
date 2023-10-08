###################################################
# Author: Éric Dias da Silva Rosso and Sandro Camargo
# Course: Data Mining
# Federal University of Pampa - Bage - PPGCAP
#
# This script will create a classifier for Wine data set
#
###################################################

# Loading libraries
library(caret) # Para criação dos conjuntos de treino e teste
library(rpart) # Para construção de árvores de decisão
library(rpart.plot) #Para visualizar a arvore de decisão
library(mixOmics) # mood.medtest
library(RVAideMemoire)

# Clear workspace
rm(list = ls())

# Set environment
setwd("~/Documents/bioinformatics/cienagro23/vinhos")
saveplots = 1

# Loading dataset
wine <- read.csv("wine.data", header = FALSE)

# Defining Column Names
colnames(wine) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline")

# Splitting dataset
set.seed(1)
training.samples <- createDataPartition(wine$Class, p = 0.8, list = FALSE)
trainset <- wine[training.samples,]
testset <- wine[-training.samples,]

# Training the decision tree
model.rpart <- rpart(Class ~ ., data = trainset, method = "class")

# Classifying samples on test set
pred.rpart <- predict(model.rpart, newdata = testset, type = "class")

# Computing the accuracy
accuracy.rpart <- mean(pred.rpart == testset$Class)
cat("A acurácia da árvore de decisão é:", round(accuracy.rpart, 3), "\n")

# Viewing the decision tree
if (saveplots) png("images/wine-tree.png")
rpart.plot(model.rpart, box.palette = list("red","green","blue"), type=0, extra=104)
if (saveplots) dev.off()

# Plotting the decision surfaces
if (saveplots) png("images/wine-decisionsurface.png")
plot(testset$Color_intensity, testset$Flavanoids, xlab="Color intensity", ylab="Flavanoids", pch=as.numeric(testset$Class)+1, col=as.numeric(testset$Class)+1, main="Test Set")
# Drawing background rectangles
rect(-10, -10, 3.48, 10, col=rgb(0, 1, 0, 0.2), lty=0, border=NULL) # Classe 1
rect(3.48, -10, 20, 1.58, col=rgb(0, 0, 1, 0.2), lty=0, border=NULL) # Classe 2
# Adding legends
legend("topright", legend=levels(as.factor(testset$Class)), pch=unique(as.numeric(as.factor(testset$Class)))+1, col=unique(as.numeric(as.factor(testset$Class)))+1, bg="white")
if (saveplots) dev.off()

if (saveplots) png("images/wine-decisionsurface2.png")
plot(testset$Flavanoids, testset$Proline, xlab="Flavanoids", ylab="Proline", pch=as.numeric(testset$Class)+1, col=as.numeric(testset$Class)+1, main="Test Set", )
# Drawing background rectangles
rect(1.58, 0, 10, 737, col=rgb(0, 1, 0, 0.2), lty=0, border=NULL) # Classe 2
rect(1.58, 737, 10, 1600, col=rgb(1, 0, 0, 0.2), lty=0, border=NULL) # Classe 3
# Adding legends
legend("topright", legend=levels(as.factor(testset$Class)), pch=unique(as.numeric(as.factor(testset$Class)))+1, col=unique(as.numeric(as.factor(testset$Class)))+1, bg="white")
if (saveplots) dev.off()

# Obtaining the factor levels for predictions and real values
pred.levels <- levels(factor(pred.rpart))
testset.levels <- levels(factor(testset$Class))
# Union of predictions and real values
all.levels <- unique(c(pred.levels, testset.levels))
# Transforming predictions and real values to the same levels
pred.rpart <- factor(pred.rpart, levels = all.levels)
testset$Class <- factor(testset$Class, levels = all.levels)

# Computing confusion matrix
cm.rpart <- confusionMatrix(pred.rpart, testset$Class)
# Viewing confusion matrix
cm.rpart$table
# Viewing accuracy
cm.rpart$overall['Accuracy']

# Ploting distribution for the most informative features
if (saveplots) png('images/wine-distributions.png')
par(mfrow=c(3,2))
plot(density(wine$Flavanoids[wine$Class==1]), xlab = "Flavonoids", xlim=c(min(wine$Flavanoids),max(wine$Flavanoids)), ylim=c(0,2), col="red", main="Density Distribution")
lines(density(wine$Flavanoids[wine$Class==2]), col="green")
lines(density(wine$Flavanoids[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

plot(density(wine$Proline[wine$Class==1]), xlab = "Proline", xlim=c(min(wine$Proline),max(wine$Proline)), ylim=c(0,0.0035), col="red", main="Density Distribution")
lines(density(wine$Proline[wine$Class==2]), col="green")
lines(density(wine$Proline[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

plot(density(wine$Total_phenols[wine$Class==1]), xlab = "Total Phenols", xlim=c(min(wine$Total_phenols),max(wine$Total_phenols)), ylim=c(0,1.6), col="red", main="Density Distribution")
lines(density(wine$Total_phenols[wine$Class==2]), col="green")
lines(density(wine$Total_phenols[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

plot(density(wine$Color_intensity[wine$Class==1]), xlab = "Color Intensity", xlim=c(min(wine$Color_intensity),max(wine$Color_intensity)), ylim=c(0,0.6), col="red", main="Density Distribution")
lines(density(wine$Color_intensity[wine$Class==2]), col="green")
lines(density(wine$Color_intensity[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

plot(density(wine$Alcohol[wine$Class==1]), xlab = "Alcohol", xlim=c(min(wine$Alcohol),max(wine$Alcohol)), ylim=c(0,1), col="red", main="Density Distribution")
lines(density(wine$Alcohol[wine$Class==2]), col="green")
lines(density(wine$Alcohol[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

plot(density(wine$OD280_OD315_of_diluted_wines[wine$Class==1]), xlab = "OD280_OD315_of_diluted_wines", xlim=c(min(wine$OD280_OD315_of_diluted_wines),max(wine$OD280_OD315_of_diluted_wines)), ylim=c(0,1.8), col="red", main="Density Distribution")
lines(density(wine$OD280_OD315_of_diluted_wines[wine$Class==2]), col="green")
lines(density(wine$OD280_OD315_of_diluted_wines[wine$Class==3]), col="blue")
legend("topright", legend=c(1,2,3), lty=1, col=c("red","green","blue"))

if (saveplots) dev.off()

# Ploting boxplots for the most informative features
if (saveplots) png('images/wine-boxplot.png')
par(mfrow=c(3,2))
boxplot(wine$Flavanoids~wine$Class, xlab="Classe", ylab="Flavonoids", col=c("red","green","blue"))
boxplot(wine$Proline~wine$Class, xlab="Classe", ylab="Proline", col=c("red","green","blue"))
boxplot(wine$Total_phenols~wine$Class, xlab="Classe", ylab="Proline", col=c("red","green","blue"))
boxplot(wine$Color_intensity~wine$Class, xlab="Classe", ylab="Color Intensity", col=c("red","green","blue"))
boxplot(wine$Alcohol~wine$Class, xlab="Classe", ylab="Alcohol", col=c("red","green","blue"))
boxplot(wine$OD280_OD315_of_diluted_wines~wine$Class, xlab="Classe", ylab="OD280 OD315", col=c("red","green","blue"))
if (saveplots) dev.off()

par(mfrow=c(1,1))

# Showing P value for a nonparametric test to define most informative features
class = as.factor(wine$Class)
for (i in 2:ncol(wine)) {
  tuk <- mood.medtest(unlist(wine[,i])~as.numeric(class)) #Median test
  print(paste(colnames(wine[i]),tuk$p.value))
}

