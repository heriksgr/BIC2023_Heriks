#01 - INSTALANDO E CARREGANDO PACOTES
#install.packages('randomForest')
library(randomForest)
#install.packages('caTools')
library(caTools)
library(dplyr)
#install.packages('caret')
library(ggplot2)
library(caret)
#install.packages('e1071', dependencies=TRUE)
library(e1071)
install.packages("ROCR")
library(ROCR)
#02 - VISUALIZACAO - IRIS
View(iris)

#03 - ATRIBUICAO DE IRIS NO OBJETO DATA
data=as.data.frame(iris[,c(1:5)])

#04 - TRANSFORMANDO SPECIES EM FATORES NA VARIAVEL TESTE: 0, 1, 2
data$teste[data$Species == 'setosa'] <- 0
data$teste[data$Species == 'versicolor'] <- 1
data$teste[data$Species == 'virginica'] <- 2

#05 - SUMARIZANDO INFOS, CLASSES DE CADA VARIAVEL
summary(data)
sapply(data, class)

#06 - NOVA VARIAVEL "TESTE" COMO UM FATOR
data <- transform(data, teste=as.factor(teste))

#07 - CONFERINDO A TRANSFORMACAO
sapply(data, class)
summary(data)

#08 - PARTICIONANDO A AMOSTRA
sample = sample.split(data$teste, SplitRatio = .7)

#09 - SUBSETS DE TREINO E TESTE
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

#10 - CONFERINDO O TAMANHO DOS SUBSETS
dim(train)
dim(test)

#11 - ATRIBUICAO DO RANDOM FOREST A VARIAVEIS CONTINUAS [1:4] AO FATOR [6],
#NUMERO DE ARVORES, NUMERO DE PREDICAO PARA CADA NO, IMPORTANCIA
rf <- randomForest(x=train[,1:4], y=train[,6], ntreeTry=500, mtry=2, importance=F, keep.forest=T)

#12 - PLOTANDO O RANDOM FOREST PARA CADA FATOR E MOSTRANDO O ERRO
#PELO NUMERO DE ARVORES SELECIONADO
rf
plot(rf, main = "Random Forest")

#13 - DEFININDO NUMERO DE AXIS
axis(2)

#14 - IMPORTANCIA DE CADA FATOR E GRAFICO DE IMPORTANCIA
importance(rf)
varImpPlot(rf,bg = "skyblue", cex=.9, main = "Feature Importance")

#15 - MATRIZ DE CONFUSAO
pred = predict(rf, newdata=test)
pred

confusionMatrix(table(pred, test$teste))
