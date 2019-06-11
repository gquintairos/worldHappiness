#working directory
setwd("C:/Users/user/Documents/Máster Big Data/Tipología y ciclo de vida de los datos/Prácticas/Práctica2/src")

#######carga y preprocesamiento de datos######
library(xlsx)
data <- read.xlsx("C:/Users/user/Documents/Máster Big Data/Tipología y ciclo de vida de los datos/Prácticas/Práctica2/data/WorldHappinessReport.xls", 1, stringsAsFactors=FALSE)
data <- data[,-17:-26]
str(data)


#######limpieza de datos######
#valores nulos
sapply(data, function(x) sum(x==0, na.rm = TRUE))
sapply(data, function(x) sum(is.na(x)))
#knn-imputation
library(VIM)
data$Log.GDP.per.capita <- kNN(data)$Log.GDP.per.capita
data$Social.support <- kNN(data)$Social.support
data$Healthy.life.expectancy.at.birth <- kNN(data)$Healthy.life.expectancy.at.birth
data$Freedom.to.make.life.choices <- kNN(data)$Freedom.to.make.life.choices
data$Generosity <- kNN(data)$Generosity
data$Perceptions.of.corruption <- kNN(data)$Perceptions.of.corruption
data$Positive.affect <- kNN(data)$Positive.affect
data$Negative.affect <- kNN(data)$Negative.affect
data$Confidence.in.national.government <- kNN(data)$Confidence.in.national.government
data$Democratic.Quality <- kNN(data)$Democratic.Quality
data$Delivery.Quality <- kNN(data)$Delivery.Quality

#outliers
boxplot.stats(data$Life.Ladder)$out
boxplot.stats(data$Log.GDP.per.capita)$out
boxplot.stats(data$Social.support)$out
boxplot.stats(data$Healthy.life.expectancy.at.birth)$out
boxplot.stats(data$Freedom.to.make.life.choices)$out
boxplot.stats(data$Generosity)$out
boxplot.stats(data$Perceptions.of.corruption)$out
boxplot.stats(data$Positive.affect)$out
boxplot.stats(data$Negative.affect)$out
boxplot.stats(data$Confidence.in.national.government)$out
boxplot.stats(data$Democratic.Quality)$out
boxplot.stats(data$Delivery.Quality)$out


######Análisis de datos#######