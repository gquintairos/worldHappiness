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

#Generacion nuevo dataset
write.xlsx(data, "C:/Users/user/Documents/Máster Big Data/Tipología y ciclo de vida de los datos/Prácticas/Práctica2/data/WorldHappinessReport_trasLimpieza.xls", row.names = FALSE)

######Análisis de datos#######

###analisis descriptivo
summary(data)

###análisis inferencial

##separacion en grupos
# Datos por año
data2005 <- data[data$Year == "2005",]
data2006 <- data[data$Year == "2006",]
data2007 <- data[data$Year == "2007",]
data2008 <- data[data$Year == "2008",]
data2009 <- data[data$Year == "2009",]
data2010 <- data[data$Year == "2010",]
data2011 <- data[data$Year == "2011",]
data2012 <- data[data$Year == "2012",]
data2013 <- data[data$Year == "2013",]
data2014 <- data[data$Year == "2014",]
data2015 <- data[data$Year == "2015",]
data2016 <- data[data$Year == "2016",]
data2017 <- data[data$Year == "2017",]
data2018 <- data[data$Year == "2018",]

# Paises por renta per capita
paisesRicos <- data[data$Log.GDP.per.capita >= mean(data$Log.GDP.per.capita),]
paisesPobres <- data[data$Log.GDP.per.capita < mean(data$Log.GDP.per.capita),]

# Paises por esperanza de vida
paisesEVAlta <- data[data$Healthy.life.expectancy.at.birth >= mean(data$Healthy.life.expectancy.at.birth),]
paisesEVBaja <- data[data$Healthy.life.expectancy.at.birth < mean(data$Healthy.life.expectancy.at.birth),]


##Comprobacion de la normalidad
alpha = 0.05
col.names = colnames(data)
for (i in 1:ncol(data)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(data[,i]) | is.numeric(data[,i])) {
    p_val = shapiro.test(data[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(data) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

##Comprobacion de la homocedasticidad
fligner.test(x = list(paisesRicos$Life.Ladder,paisesPobres$Life.Ladder))
fligner.test(x = list(paisesEVAlta$Life.Ladder,paisesEVBaja$Life.Ladder))

###Pruebas estadísticas
##Contraste de hipotesis entre rentas altas y bajas para ver si siguen la misma distribución
wilcox.test(paisesRicos$Life.Ladder, paisesPobres$Life.Ladder)

##regresión lineal para predecir el nivel de felicidad
modelo1 <- lm(Life.Ladder ~ . - Year - Country.name - Standard.deviation.of.ladder.by.country.year - Standard.deviation.Mean.of.ladder.by.country.year, data = data)
step(modelo1)
#creamos el modelo optimo segun el proceso anterior
modeloOptimo <- lm(formula = Life.Ladder ~ Log.GDP.per.capita + Social.support + Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Positive.affect + Confidence.in.national.government, data = data)
##inventamos dos paises, uno posiblemente feliz y otro posiblemente triste
paisInventadoFeliz <- data.frame(
  Log.GDP.per.capita = 13,
  Social.support = 0.99,
  Healthy.life.expectancy.at.birth = 100,
  Freedom.to.make.life.choices = 0.99,
  Generosity = 0.8,
  Perceptions.of.corruption = 0.01,
  Positive.affect = 0.99,
  Confidence.in.national.government = 0.95
)

paisInventadoInfeliz <- data.frame(
  Log.GDP.per.capita = 2.5,
  Social.support = 0.05,
  Healthy.life.expectancy.at.birth = 38,
  Freedom.to.make.life.choices = 0.1,
  Generosity = 0.01,
  Perceptions.of.corruption = 0.95,
  Positive.affect = 0.04,
  Confidence.in.national.government = 0.05
)
#realizamos las predicciones con el modelo
predict(modeloOptimo, paisInventadoFeliz)
predict(modeloOptimo, paisInventadoInfeliz)


###analisis de correlaciones
matrizCorrelaciones <- matrix(nc = 2, nr = 0)
colnames(matrizCorrelaciones) <- c("coeficiente", "p-valor")
# Coeficiente de correlacion de las variables independientes con respecto a la felicidad
for (i in 4:(ncol(data)-2)) {
  if (is.integer(data[,i]) | is.numeric(data[,i])) {
    spearman_test = cor.test(data[,i],data$Life.Ladder,method = "spearman")
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    matrizCorrelaciones <- rbind(matrizCorrelaciones, pair)
    rownames(matrizCorrelaciones)[nrow(matrizCorrelaciones)] <- colnames(data)[i]
  }
}
print(matrizCorrelaciones)


##gráficas y tablas
boxplot(paisesPobres$Life.Ladder, paisesRicos$Life.Ladder, names = c("Países pobres", "Países ricos"), ylab="Nivel de felicidad") #diferencia de felicidad entre ricos y pobres
boxplot(paisesEVBaja$Life.Ladder, paisesEVAlta$Life.Ladder, names = c("Países con esperanza de vida baja", "Países con esperanza de vida alta"), ylab="Nivel de felicidad") #diferencia de felicidad según esperanza de vida
hist(data$Life.Ladder[data$Year=="2018"]) #distribucion de la felicidad
