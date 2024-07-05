# Instalar y cargar los paquetes necesarios
if (!require(neuralnet)) install.packages("neuralnet")
if (!require(caret)) install.packages("caret")
if (!require(dplyr)) install.packages("dplyr")

library(neuralnet)
library(caret)
library(dplyr)

# Cargar los datos
data <- read.csv("data/Dry_Bean_Dataset.csv")

# Verificar las primeras filas del dataframe
head(data)

# Verificar si hay valores nulos
sum(is.na(data))

# Normalizar los datos (excepto la columna Class)
data_norm <- as.data.frame(scale(data[1:1000, -ncol(data)]))
data_norm$Class <- data[1:1000,]$Class

# Convertir la columna 'Class' a factor para clasificación
data_norm$Class <- as.factor(data_norm$Class)

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(data_norm$Class, p = 0.7, list = FALSE)
trainData <- data_norm[trainIndex, ]
testData <- data_norm[-trainIndex, ]

# Entrenar una red neuronal
nn_model <- neuralnet(Class ~ ., 
                      data = trainData, 
                      hidden = c(10, 7), 
                      act.fct = "tanh",
                      stepmax = 100,
                      linear.output = FALSE)

# Hacer predicciones
predictions <- compute(nn_model, testData[, -17])
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

# Evaluar el modelo
confusionMatrix(as.factor(predicted_classes), as.factor(testData$Species))
