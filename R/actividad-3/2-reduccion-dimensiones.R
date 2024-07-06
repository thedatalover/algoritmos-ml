# Instalar y cargar los paquetes necesarios
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(Rtsne)) install.packages("Rtsne")
if (!require(dplyr)) install.packages("dplyr")
if (!require(umap)) install.packages("umap")

library(ggplot2)
library(Rtsne)
library(dplyr)
library(umap)

# Leer el archivo CSV con los datos limpios y normalizados
data <- read_csv("datos/gene_data_normalized.csv")

# Creamos un conjunto de datos solo con variables (predictores)
dimensions_data <- select(data, -class)

# Definir el número de dimensiones
num_dim <- 2

####################################
# Aplicar PCA al conjunto de datos #
####################################

# Aplicar PCA
pca <- prcomp(dimensions_data, center = TRUE, scale. = FALSE)

# Ver la varianza explicada por cada componente principal
print(summary(pca))

# Crear un dataframe con los dos componentes principales de PCA
pca_data <- as.data.frame(pca$x[, 1:num_dim])

# Añadir la columna 'class' al dataframe de componentes principales
pca_data$class <- data$class

######################################
# Aplicar t-SNE al conjunto de datos #
######################################

set.seed(123)  # Para reproducibilidad
tsne_result <- Rtsne(
  dimensions_data,
  dims = num_dim,
  perplexity = 30,
  verbose = TRUE,
  max_iter = 1000
  )

# Crear un dataframe con los dos componentes de t-SNE
tsne_data <- as.data.frame(tsne_result$Y)

# Añadir la columna 'class' al dataframe de t-SNE
tsne_data$class <- data$class

#####################################
# Aplicar UMAP al conjunto de datos #
#####################################

# Configurar UMAP para reducir a 2 dimensiones
umap_config <- umap.defaults
umap_config$n_components <- num_dim

# Aplicar UMAP
umap_result <- umap(dimensions_data, config = umap_config)
umap_data <- as.data.frame(umap_result$layout)

# Añadir la columna 'class' al dataframe de UMAP
umap_data$class <- data$class

##########################
# Comparación de Modelos #
##########################

# Función para calcular el coeficiente de correlación de Pearson entre las distancias
distance_correlation <- function(original_data, reduced_data) {
  original_distances <- as.vector(dist(original_data))
  reduced_distances <- as.vector(dist(reduced_data))
  return(cor(original_distances, reduced_distances, method = "pearson"))
}

# Calcular la conservación de la distancia
pca_distance_corr <- distance_correlation(dimensions_data, pca_data)
umap_distance_corr <- distance_correlation(dimensions_data, umap_data)
tsne_distance_corr <- distance_correlation(dimensions_data, tsne_data)

paste("Conservación de la distancia (PCA): ", pca_distance_corr)
paste("Conservación de la distancia (UMAP): ", umap_distance_corr)
paste("Conservación de la distancia (t-SNE): ", tsne_distance_corr)

######################################################################
# Código para Probar Varios Valores de Componentes Principales (PCA) #
######################################################################

# Mostrar varianza explicada - PCA
paste("Varianza explicada (PCA): ", sum(summary(pca)$importance[2, 1:300]))

# Crear un dataframe con los 300 componentes principales de PCA
pca_data <- as.data.frame(pca$x[, 1:300])
pca_data$class <- data$class

# Guardar los datos con menos dimensiones
write.csv(pca_data, "datos/gene_data_dimensions.csv", row.names = FALSE)

