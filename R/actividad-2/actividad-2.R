# Cargar las librerías necesarias
library(e1071)  # Para SVM, SVM Gaussiano y Naive Bayes
library(caret)  # Para la división de los datos y evaluación del modelo
library(randomForest)  # Para Random Forest

# Leer los datos
data <- read.csv("datos/data.csv")

# Eliminar la columna "ID"
data <- data[ , !(names(data) %in% c("ID"))]

# Convertir la columna "Diagnosis" a factor y codificar "B" como 0 y "M" como 1
data$Diagnosis <- as.factor(ifelse(data$Diagnosis == "B", 0, 1))

# Dividir el dataset en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]

# Función para entrenar y evaluar un modelo
evaluate_model <- function(model_func, model_name, data_train, data_test) {
  model <- model_func(data_train)
  predictions <- predict(model, data_test)
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(data_test$Diagnosis))
  
  kappa_value <- confusion_matrix$overall["Kappa"]
  accuracy <- confusion_matrix$overall["Accuracy"]
  
  cat("Resultados para", model_name, ":\n")
  print(confusion_matrix)
  cat("Kappa:", kappa_value, "\n")
  cat("Accuracy:", accuracy, "\n\n")
  
  return(list(confusion_matrix = confusion_matrix, kappa = kappa_value, accuracy = accuracy))
}

# Definir las funciones de los modelos
svm_model <- function(data) {
  svm(Diagnosis ~ ., data = data, kernel = "linear")
}

svm_gaussian_model <- function(data) {
  svm(Diagnosis ~ ., data = data, kernel = "radial")
}

random_forest_model <- function(data) {
  randomForest(Diagnosis ~ ., data = data, importance = TRUE)
}

naive_bayes_model <- function(data) {
  naiveBayes(Diagnosis ~ ., data = data)
}

# Evaluar los modelos
results_svm <- evaluate_model(svm_model, "SVM", data_train, data_test)
results_svm_gaussian <- evaluate_model(svm_gaussian_model, "SVM Gaussiano", data_train, data_test)
results_random_forest <- evaluate_model(random_forest_model, "Random Forest", data_train, data_test)
results_naive_bayes <- evaluate_model(naive_bayes_model, "Naive Bayes", data_train, data_test)

# Comparar los resultados
results <- data.frame(
  Model = c("SVM", "SVM Gaussiano", "Random Forest", "Naive Bayes"),
  Accuracy = c(results_svm$accuracy, results_svm_gaussian$accuracy, results_random_forest$accuracy, results_naive_bayes$accuracy),
  Kappa = c(results_svm$kappa, results_svm_gaussian$kappa, results_random_forest$kappa, results_naive_bayes$kappa)
)

print(results)
