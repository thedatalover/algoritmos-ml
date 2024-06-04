# Cargar las librerías necesarias
library(readr) # Para leer ficheros CSV
library(ggplot2) # Para crear los gráficos
library(factoextra) # Para facilitar la visualización de los cluster
library(dplyr) # Para manipular los DataFrame

# Leer el archivo CSV
data <- read_csv("../datos/shopping_data_cleaned.csv")

# Verificar las primeras filas del dataframe
head(data)

# Separar la columna "Genre"
genre <- data$Genre

# Escalar los datos, excluyendo la columna "Genre"
data_scaled <- data %>%
  select(-Genre) %>%
  scale()

# Añadir de nuevo la columna "Genre" al dataframe escalado
data_scaled <- as.data.frame(data_scaled)
data_scaled$Genre <- genre

# Calcular el valor óptimo de K utilizando el "elbow method"
set.seed(123)  # Para reproducibilidad
wss <- function(k) {
  kmeans(data_scaled, k, nstart = 12 )$tot.withinss
}

# Calcular el total within sum of squares (wss) para k = 1 a k = 10
k.values <- 1:12
wss_values <- sapply(k.values, wss)

# Graficar el "elbow curve" para encontrar el número óptimo de clusters
elbow_plot <- qplot(k.values, wss_values, geom = "line") +
  ggtitle("Elbow Curve para Encontrar el Número Óptimo de Clusters") +
  xlab("Número de Clusters K") +
  ylab("Total Within Sum of Squares (WSS)")

print(elbow_plot)

# Basado en la gráfica del "elbow curve", elegir el valor de K
optimal_k <- 6

# Aplicar K-Means con el valor óptimo de K
kmeans_result <- kmeans(data_scaled, centers = optimal_k, nstart = 25)

# Añadir el resultado de los clusters al dataframe original
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualizar los clusters utilizando ggplot2
# Suponiendo que el dataframe tiene dos dimensiones principales para la visualización, ajusta esto según tus datos
cluster_plot <- ggplot(data, aes(x = `Spending Score (1-100)`, y = `Annual Income`, color = Cluster)) + 
  geom_point() + 
  ggtitle(paste("Clusters Identificados con K-Means (K =", optimal_k, ")")) + 
  xlab("Spending Score") + 
  ylab("Anual Income")

print(cluster_plot)
