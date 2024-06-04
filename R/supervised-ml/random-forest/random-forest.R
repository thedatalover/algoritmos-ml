# Cargar las librerías necesarias
library(randomForest)  # Para el algoritmo Random Forest
library(caret)         # Para la división de los datos y evaluación del modelo
library(ggplot2)       # Para la visualización de los resultados

# Cargar el dataset iris
data(iris)

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
data_train <- iris[trainIndex, ]
data_test <- iris[-trainIndex, ]

# Entrenar el modelo de Random Forest
rf_model <- randomForest(Species ~ ., data = data_train, importance = TRUE)

# Hacer predicciones en el conjunto de prueba
predictions <- predict(rf_model, data_test)

# Evaluar el rendimiento del modelo
confusion_matrix <- confusionMatrix(predictions, data_test$Species)
print(confusion_matrix)

# Visualizar la importancia de las variables
importance <- importance(rf_model)
varImpPlot(rf_model, main = "Importancia de las Variables en el Modelo de Random Forest")

# Visualización de los resultados (opcional)
# Crear una gráfica de los datos originales con los valores predichos
plot_data <- data_test
plot_data$Predicted <- predictions

ggplot(plot_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(aes(shape = Predicted), size = 3) +
  ggtitle("Predicciones del Modelo de Random Forest en el Conjunto de Prueba") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  theme_minimal()
