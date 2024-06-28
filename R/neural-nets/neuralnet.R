# Instalar y cargar el paquete
library(neuralnet)
library(caret)

# Crear un conjunto de datos de ejemplo
data(iris)
set.seed(123)
iris$Species <- as.numeric(iris$Species) - 1

# Dividir el conjunto de datos
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Entrenar una red neuronal
nn_model <- neuralnet(Species ~ ., 
                      data = trainData, 
                      hidden = c(3, 2), 
                      act.fct = "tanh", 
                      linear.output = FALSE)

# Hacer predicciones
predictions <- compute(nn_model, testData[, -5])
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

# Evaluar el modelo
confusionMatrix(as.factor(predicted_classes), as.factor(testData$Species))
