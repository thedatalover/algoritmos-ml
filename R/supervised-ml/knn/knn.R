# Cargar las librerías necesarias
library(class)  # Para la función knn
library(caret)  # Para la división de los datos y validación cruzada
library(ggplot2)  # Para la visualización
library(dplyr)  # Para la manipulación de datos

# Cargar el dataset iris
data(iris)

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123) # Para reproducibilidad
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
data_train <- iris[trainIndex, ]
data_test <- iris[-trainIndex, ]

# Estandarizar los datos
# Nota: Solo estandarizamos las características (no las etiquetas)
train_scaled <- data_train %>%
  mutate(across(where(is.numeric), scale))

test_scaled <- data_test %>%
  mutate(across(where(is.numeric), scale))

# Buscar el número óptimo de K usando validación cruzada
set.seed(123)
control <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(.k = 1:20)

knn_fit <- train(Species ~ ., data = train_scaled, method = "knn", 
                 trControl = control, tuneGrid = tuneGrid)

# Mostrar los resultados del tuning
print(knn_fit)
plot(knn_fit)

# Obtener el mejor valor de K
optimal_k <- knn_fit$bestTune$k
print(paste("El número óptimo de K es:", optimal_k))

# Entrenar el modelo KNN con el valor óptimo de K
knn_model <- knn(train = train_scaled[, -5], test = test_scaled[, -5], 
                 cl = train_scaled$Species, k = optimal_k)

# Evaluar el modelo
confusion_matrix <- confusionMatrix(knn_model, data_test$Species)
print(confusion_matrix)

# Visualización de los resultados (opcional)
# Crear una gráfica de los datos originales con los valores predichos
plot_data <- data_test
plot_data$Predicted <- knn_model

ggplot(plot_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(aes(shape = Predicted), size = 3) +
  ggtitle(paste("Predicciones del Modelo KNN con K =", optimal_k)) +
  xlab("Petal Length") +
  ylab("Petal Width") +
  theme_minimal()
