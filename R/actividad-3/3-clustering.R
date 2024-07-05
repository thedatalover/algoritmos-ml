# Instalar y cargar los paquetes necesarios
if (!require(cluster)) install.packages("cluster")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(cluster)
library(ggplot2)
library(dplyr)

# Cargar los datos
data <- read.csv("datos/gene_data_dimensions.csv")

# Contar el número de clases únicas en la columna "class"
num_classes <- data %>% select(class) %>% distinct() %>% nrow()

# Mostrar el número de clases únicas
paste("Número de clases únicas en la columna 'class': ", num_classes)

# Separar la columna "class" de los datos
class_column <- data$class
data <- select(data, -class)

##########################
# Clustering con K-Means #
##########################

# Determinar el número óptimo de clusters usando el método del codo
wss <- (nrow(data)-1)*sum(apply(data, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$tot.withinss)

# Graficar el método del codo
plot(1:15, wss, type="b", xlab="Número de clusters", ylab="Suma de cuadrados total intra-cluster", main="Método del codo para determinar el número óptimo de clusters")

# Aplicar K-means con el número óptimo de clusters (por ejemplo, 6)
set.seed(123)
kmeans_result <- kmeans(data, centers=6)

# Añadir los resultados de clustering al dataframe original
data$KMeans_Cluster <- kmeans_result$cluster

# Graficar los clusters de K-means (usando las dos primeras dimensiones)
ggplot(data, aes(x = data[,1], y = data[,2], color = as.factor(KMeans_Cluster))) +
  geom_point() +
  labs(title = "Clustering K-means", x = "Dimensión 1", y = "Dimensión 2") +
  theme_minimal()

########################################
# Clustering con Clustering Jerárquico #
########################################
# Calcular la matriz de distancias
dist_matrix <- dist(data)

# Aplicar clustering jerárquico
hc <- hclust(dist_matrix)

# Cortar el dendrograma en k clusters
data$Cluster_HC <- as.factor(cutree(hc, 6))

# Graficar los clusters de DBSCAN (usando las dos primeras dimensiones)
ggplot(data, aes(x = data[,1], y = data[,2], color = Cluster_HC)) +
  geom_point() +
  labs(title = "Clustering Jerárquico", x = "Dim1", y = "Dim2") +
  theme_minimal()
