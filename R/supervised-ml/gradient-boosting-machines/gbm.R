# Cargar las librerías necesarias
library(gbm)       # Para el algoritmo GBM
library(caret)     # Para la división de los datos y evaluación del modelo
library(ggplot2)   # Para la visualización de los resultados
library(dplyr)     # Para la manipulación de datos

# Cargar el dataset iris
data(iris)

# Convertir la variable Species a un factor (esto es importante para gbm)
iris$Species <- as.factor(iris$Species)

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
data_train <- iris[trainIndex, ]
data_test <- iris[-trainIndex, ]

# Entrenar el modelo GBM
gbm_model <- gbm(Species ~ ., 
                 data = data_train, 
                 distribution = "multinomial", 
                 n.trees = 100, 
                 interaction.depth = 3, 
                 n.minobsinnode = 10, 
                 shrinkage = 0.01, 
                 cv.folds = 5,
                 n.cores = NULL, 
                 verbose = FALSE)

# Seleccionar el número óptimo de árboles basados en validación cruzada
best.iter <- gbm.perf(gbm_model, method = "cv")
print(paste("Número óptimo de áboles: ", best.iter))

# Hacer predicciones en el conjunto de prueba
predictions <- predict(gbm_model, data_test, n.trees = best.iter, type = "response")
predicted_classes <- apply(predictions, 1, function(x) colnames(predictions)[which.max(x)])

# Evaluar el rendimiento del modelo
confusion_matrix <- confusionMatrix(factor(predicted_classes), data_test$Species)
print(confusion_matrix)

# Visualización de los resultados (opcional)
# Crear una gráfica de los datos originales con los valores predichos
plot_data <- data_test
plot_data$Predicted <- factor(predicted_classes)

ggplot(plot_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(aes(shape = Predicted), size = 3) +
  ggtitle("Predicciones del Modelo GBM en el Conjunto de Prueba") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  theme_minimal()
