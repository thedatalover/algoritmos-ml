# Cargar los paquetes necesarios
library(keras)
library(caret)

# Cargar y preparar el dataset iris
data(iris)
set.seed(123)

# Convertir la variable de respuesta a un formato numérico
iris$Species <- as.numeric(iris$Species) - 1

# Dividir el dataset en conjuntos de entrenamiento y prueba
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Convertir los datos a matrices y normalizar las características
x_train <- as.matrix(trainData[, -5])
y_train <- to_categorical(trainData$Species, 3)
x_test <- as.matrix(testData[, -5])
y_test <- to_categorical(testData$Species, 3)

# Definir el modelo
model <- keras_model_sequential() %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(4)) %>%
  layer_dense(units = 1, activation = 'softmax')

# Compilar el modelo
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Entrenar el modelo
history <- model %>% fit(
  x_train, y_train,
  epochs = 100,
  batch_size = 5,
  validation_split = 0.2
)

# Evaluar el modelo en el conjunto de prueba
scores <- model %>% evaluate(x_test, y_test)
cat('Test loss:', scores$loss, "\n")
cat('Test accuracy:', scores$accuracy, "\n")

# Hacer predicciones
predictions <- model %>% predict_classes(x_test)

# Convertir predicciones a factores
predicted_classes <- factor(predictions, levels = 0:2, labels = c("setosa", "versicolor", "virginica"))
true_classes <- factor(testData$Species, levels = 0:2, labels = c("setosa", "versicolor", "virginica"))

# Evaluar el modelo utilizando una matriz de confusión
confusion_matrix <- confusionMatrix(predicted_classes, true_classes)
print(confusion_matrix)
