# Cargar librerias
library(nnet)
library(caret)

# Crear un conjunto de datos de ejemplo
data(iris)
set.seed(123)
iris$Species <- as.factor(ifelse(iris$Species == "setosa", 1, ifelse(iris$Species == "versicolor", 2, 3)))

# Dividir el conjunto de datos
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Entrenar una red neuronal
nn_model <- nnet(Species ~ ., data = trainData, size = 2, decay = 0.1, maxit = 200, trace = FALSE)

# Hacer predicciones (obtener probabilidades)
predictions_raw <- predict(nn_model, testData, type = "raw")

# Convertir probabilidades a clases
predicted_classes <- apply(predictions_raw, 1, which.max)

# Evaluar el modelo
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), testData$Species)
print(confusion_matrix)
