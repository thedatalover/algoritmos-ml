# Cargar las librerías necesarias
library(e1071)  # Para la función svm
library(caret)  # Para la división de los datos y evaluación del modelo

# Cargar el dataset iris
data(iris)

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
data_train <- iris[trainIndex, ]
data_test <- iris[-trainIndex, ]

# Entrenar el modelo SVM con kernel gaussiano (RBF)
svm_model <- svm(Species ~ ., data = data_train, kernel = "radial")

# Hacer predicciones en el conjunto de prueba
predictions <- predict(svm_model, data_test)

# Evaluar el rendimiento del modelo
confusion_matrix <- confusionMatrix(predictions, data_test$Species)
print(confusion_matrix)

# Visualización de los resultados (opcional)
# Crear una gráfica de los datos originales con los valores predichos
library(ggplot2)
plot_data <- data_test
plot_data$Predicted <- predictions

ggplot(plot_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(aes(shape = Predicted), size = 3) +
  ggtitle("Predicciones del Modelo SVM con Kernel Gaussiano en el Conjunto de Prueba") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  theme_minimal()
